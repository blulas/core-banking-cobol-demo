      *================================================================*
      * PROGRAM  : DRPEDIT                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : DIVIDEND REINVESTMENT - INPUT EDIT. READS THE DRP   *
      *            TRANSACTIONS, UPDATES THE DRP MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. BANK HOLDING COMPANY STOCK      *
      *            DIVIDEND REINVESTMENT PLAN                          *
      * INPUT    : DRPMST (KSDS), DRPTRN, BANKCTL                      *
      * OUTPUT   : DRPOUT, DRPEDRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, DRFEE, DRSTAT, DRELG    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1984-07-11 EWS  ORIGINAL                                       *
      * 1988-10-05 BHS  PERFORMANCE - CASH-PAY READ AHEAD INC9675      *
      * 1989-11-06 RJT  PERFORMANCE - SHARES-AMT READ AHEAD INC6417    *
      * 1991-08-12 JLM  AUDIT FINDING - REINVEST CONTROL TOTALS PRB9893*
      * 1997-05-06 JLM  FEE EXCEPTIONS TO OPS QUEUE CHG5473            *
      * 1998-04-23 TWH  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2001-12-26 GAF  CASH-PAY CUTOFF MOVED REQ8667                  *
      * 2001-05-23 EWS  FEE EXCEPTIONS TO OPS QUEUE CHG1836            *
      * 2001-06-12 TWH  WITHHOLD EXCEPTIONS TO OPS QUEUE REQ2400       *
      * 2008-07-26 GAF  REINVEST RERUN PROTECTION CHG8641              *
      * 2009-02-07 TWH  FEE STATUS CODES ADDED REQ2230                 *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DRPEDIT.
       AUTHOR. B H SANDERS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "DRPMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS DRPM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "DRPTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "DRPOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "DRPEDRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY DRPMST.
       FD  TRAN-FILE.
       COPY DRPTRN.
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
                                           VALUE 20.000000.
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
      * WS-DIVIDEND-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-DIVIDEND-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000360500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000417500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005100000000225000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015100000000131000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017600000000430000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020100000000445500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022600000000255500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023100000000404500'.
       01  WS-DIVIDEND-BAND-TBL REDEFINES WS-DIVIDEND-BAND-VALUES.
           05  WS-DIVIDEND-BAND-ROW OCCURS 8 TIMES.
               10  WS-DIVIDEND-BAND-LOW    PIC 9(09)V99.
               10  WS-DIVIDEND-BAND-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-WITHHOLD-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-WITHHOLD-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000708000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000104500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000550000000895000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000650000000255000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001150000000735000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001250000000480500'.
       01  WS-WITHHOLD-BAND-TBL REDEFINES WS-WITHHOLD-BAND-VALUES.
           05  WS-WITHHOLD-BAND-ROW OCCURS 6 TIMES.
               10  WS-WITHHOLD-BAND-LOW    PIC 9(09)V99.
               10  WS-WITHHOLD-BAND-RATE   PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  DRPEDIT  DIVIDEND REINVESTMENT'.
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
       COPY DRFEEP.
       COPY DRSTATP.
       COPY DRELGP.

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
               MOVE 'DRPMST' TO ABND-FILE-NAME
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
           MOVE DRPT-KEY TO DRPM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-ACCUM-REASON
               PERFORM 3010-COUNT-PAYMENT
               PERFORM 3020-AGE-NOTICE
               PERFORM 3030-COUNT-NOTICE
               PERFORM 3040-SELECT-RATE
               PERFORM 3050-COUNT-SEQ
               PERFORM 3060-LIMIT-DIVIDEND
               PERFORM 3070-ACCUM-CHANNEL
               PERFORM 3080-ASSESS-STOP
               PERFORM 3090-ASSESS-OVERRIDE
               PERFORM 3100-CHECK-LAST-ACT
               PERFORM 3110-SET-TRAN
               PERFORM 3120-DESCRIBE-SOURCE
               PERFORM 3130-ACCRUE-RATE
               PERFORM 3140-AGE-EFF
               PERFORM 3150-EDIT-DESC
               PERFORM 3160-ACCRUE-FEE
               PERFORM 3170-COUNT-ITEM
               PERFORM 3180-SELECT-RATE
               PERFORM 3190-FLAG-REINVEST
               PERFORM 3200-SET-CHANNEL
               PERFORM 3210-EDIT-REJECT
               PERFORM 3220-DESCRIBE-CYCLE
               PERFORM 3230-COUNT-NOTICE
               PERFORM 3240-PRORATE-REINVEST
               PERFORM 3250-ACCRUE-DISCOUNT
               PERFORM 3260-DESCRIBE-REASON
               PERFORM 3270-AGE-NOTICE
               PERFORM 3280-SPLIT-WITHHOLD
               PERFORM 3290-EDIT-CUST-ID
               PERFORM 3300-ACCUM-CHANNEL
               PERFORM 3310-CHECK-NEXT-DUE
               PERFORM 3320-ASSESS-OVERRIDE
               PERFORM 3330-CHECK-LAST-MAINT
               PERFORM 3340-ACCRUE-FEE
               PERFORM 3350-AGE-NEXT-REVIEW
               PERFORM 3360-LIMIT-FEE
               PERFORM 3370-DESCRIBE-CYCLE
               PERFORM 3380-COUNT-ITEM
               PERFORM 3390-EDIT-REF-NO
               PERFORM 3400-SET-CYCLE
               PERFORM 3410-ASSESS-REVERSAL
               PERFORM 3420-SET-HOLDER-TYPE
               PERFORM 3430-SELECT-PENALTY-PCT
               PERFORM 3440-SELECT-RATE
               PERFORM 3450-SPLIT-REINVEST
               PERFORM 3460-SPLIT-REINVEST
               PERFORM 3470-DESCRIBE-CHANNEL
               PERFORM 3480-FLAG-SHARES-AMT
               PERFORM 3490-ACCRUE-CASH-PAY
               PERFORM 3500-FLAG-FEE
               PERFORM 3510-COUNT-EXCEPT
               PERFORM 3520-AGE-LAST-MAINT
               PERFORM 3530-DESCRIBE-TRAN
               PERFORM 3540-COUNT-ITEM
               PERFORM 3550-SPLIT-SHARES-AMT
               PERFORM 3560-CHECK-EFF
               PERFORM 3570-COUNT-SEQ
               PERFORM 3580-SELECT-RATE
               PERFORM 3590-COUNT-EXCEPT
               PERFORM 3600-PRORATE-DIVIDEND
               PERFORM 3610-LIMIT-CASH-PAY
               PERFORM 3620-EDIT-MEMO
               PERFORM 3630-SPLIT-CASH-PAY
               PERFORM 3640-EDIT-REF-NO
               PERFORM 3650-ACCRUE-REINVEST
               PERFORM 3660-PRORATE-FEE
               PERFORM 3670-ASSESS-REVERSAL
               PERFORM 3680-LIMIT-ITEM
               PERFORM 3690-ACCUM-CYCLE
               PERFORM 3700-CHECK-OPEN
               PERFORM 3710-SPLIT-CASH-PAY
               PERFORM 3720-ASSESS-REVERSAL
               PERFORM 3730-ACCUM-SOURCE
               PERFORM 3740-ACCUM-SOURCE
               PERFORM 3750-EDIT-CONTACT
               PERFORM 3760-ACCRUE-SHARES-AMT
               PERFORM 3770-EDIT-MEMO
               PERFORM 3780-LIMIT-FEE
               PERFORM 3790-AGE-NEXT-DUE
               PERFORM 3800-FLAG-ITEM
               PERFORM 3810-CHECK-EFFECTIVE
               PERFORM 3820-ASSESS-EXEMPT
               PERFORM 3830-PRORATE-DIVIDEND
               PERFORM 3840-EDIT-DESC
               PERFORM 3850-COUNT-EXCEPT
               PERFORM 3860-SET-SOURCE
               PERFORM 3870-COUNT-PAYMENT
               PERFORM 3880-ACCUM-PLAN-OPTION
               PERFORM 3890-FLAG-FEE
               PERFORM 3900-SPLIT-FEE
               PERFORM 3910-ACCRUE-CASH-PAY
               PERFORM 3920-AGE-LAST-MAINT
               PERFORM 3930-ACCRUE-CASH-PAY
               PERFORM 3940-ACCUM-HOLDER-TYPE
               PERFORM 3950-CHECK-EFF
               PERFORM 3960-FLAG-WITHHOLD
               PERFORM 3970-CHECK-OPEN
               PERFORM 3980-ACCRUE-RATE
               PERFORM 3990-SELECT-RATE
               PERFORM 4000-COUNT-NOTICE
               PERFORM 4010-SPLIT-SHARES-AMT
               PERFORM 4020-DESCRIBE-TRAN
               PERFORM 4030-EDIT-NAME
               PERFORM 4040-COUNT-ITEM
               PERFORM 4050-SELECT-RATE
               PERFORM 4060-DESCRIBE-REASON
               PERFORM 4070-SELECT-RATE
               PERFORM 4080-PRORATE-WITHHOLD
               PERFORM 4090-ASSESS-AUTO-RENEW
               PERFORM 4100-EDIT-CUST-ID
               PERFORM 4110-ACCRUE-FEE
               PERFORM 4120-SELECT-RATE
               PERFORM 4130-ACCRUE-CASH-PAY
               PERFORM 4140-AGE-NEXT-REVIEW
               PERFORM 4150-EDIT-OPERATOR
               PERFORM 4160-SELECT-RATE
               PERFORM 4170-COUNT-NOTICE
               PERFORM 4180-FLAG-REINVEST
               PERFORM 4190-DESCRIBE-CYCLE
               PERFORM 4200-DESCRIBE-SOURCE
               PERFORM 4210-SELECT-DISCOUNT
               PERFORM 4220-CHECK-POST
               PERFORM 7030-CALL-DRFEE
               PERFORM 7000-CALL-DRSTAT
               PERFORM 7020-CALL-DRELG
               REWRITE DRP-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'DRPMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM DRP-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * ACCUMULATE WITHHOLD AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3000-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPM-REASON-CD
                   ADD DRPM-WITHHOLD-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPM-WITHHOLD-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3010-COUNT-PAYMENT.
           IF DRPM-PAYMENT-CNT < 9999999
               ADD 1 TO DRPM-PAYMENT-CNT
           END-IF
           IF DRPM-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO DRPM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3020-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF DRPM-NOTICE-DATE > 0 AND DRPM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRPT-DIVIDEND-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD DRPT-DIVIDEND-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD DRPT-DIVIDEND-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD DRPT-DIVIDEND-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRPT-DIVIDEND-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3030-COUNT-NOTICE.
           IF DRPM-NOTICE-CNT < 9999999
               ADD 1 TO DRPM-NOTICE-CNT
           END-IF
           IF DRPM-NOTICE-CNT NOT < 10
               MOVE 'Y' TO DRPM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE WITHHOLD BAND TIER FOR THE CASH PAY YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3040-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-WITHHOLD-BAND-RATE(1) TO DRPM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF DRPM-CASH-PAY-YTD NOT < WS-WITHHOLD-BAND-LOW(WS-TX)
                   MOVE WS-WITHHOLD-BAND-RATE(WS-TX) TO DRPM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3050-COUNT-SEQ.
           IF DRPT-SEQ < 9999999
               ADD 1 TO DRPT-SEQ
           END-IF
           IF DRPT-SEQ NOT < 6
               MOVE 'Y' TO DRPT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DIVIDEND AMT IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3060-LIMIT-DIVIDEND.
           IF DRPT-DIVIDEND-AMT < 0.00
               MOVE 0.00 TO DRPT-DIVIDEND-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRPT-DIVIDEND-AMT > 1000.00
               MOVE 1000.00 TO DRPT-DIVIDEND-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3070-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPT-CHANNEL-CD
                   ADD DRPM-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPM-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE REINVEST FEE ON THE FEE AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3080-ASSESS-STOP.
           EVALUATE TRUE
               WHEN DRPM-STOP-FLAG = 'Y'
                   MOVE ZERO TO DRPT-REINVEST-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPM-PREFERRED-FLAG = 'Y'
                   COMPUTE DRPT-REINVEST-AMT ROUNDED = DRPM-FEE-AMT *
                       DRPM-DISCOUNT / 200
               WHEN DRPM-REASON-CD = 'B'
                   MOVE 3.00 TO DRPT-REINVEST-AMT
               WHEN DRPM-REASON-CD = 'G'
                   MOVE 10.00 TO DRPT-REINVEST-AMT
               WHEN OTHER
                   COMPUTE DRPT-REINVEST-AMT ROUNDED = DRPM-FEE-AMT *
                       DRPM-DISCOUNT / 100
           END-EVALUATE
           IF DRPT-REINVEST-AMT > 0 AND DRPT-REINVEST-AMT < 10.00
               MOVE 10.00 TO DRPT-REINVEST-AMT
           END-IF
           IF DRPT-REINVEST-AMT > 35.00
               MOVE 35.00 TO DRPT-REINVEST-AMT
           END-IF
           ADD DRPT-REINVEST-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE DIVIDEND FEE ON THE SHARES AMT AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3090-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN DRPM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO DRPM-DIVIDEND-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE DRPM-DIVIDEND-AMT ROUNDED =
                       DRPM-SHARES-AMT-AMT * DRPM-RATE / 200
               WHEN DRPM-REASON-CD = 'B'
                   MOVE 7.00 TO DRPM-DIVIDEND-AMT
               WHEN DRPM-REASON-CD = 'G'
                   MOVE 7.00 TO DRPM-DIVIDEND-AMT
               WHEN OTHER
                   COMPUTE DRPM-DIVIDEND-AMT ROUNDED =
                       DRPM-SHARES-AMT-AMT * DRPM-RATE / 100
           END-EVALUATE
           IF DRPM-DIVIDEND-AMT > 0 AND DRPM-DIVIDEND-AMT < 2.00
               MOVE 2.00 TO DRPM-DIVIDEND-AMT
           END-IF
           IF DRPM-DIVIDEND-AMT > 250.00
               MOVE 250.00 TO DRPM-DIVIDEND-AMT
           END-IF
           ADD DRPM-DIVIDEND-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 90 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3100-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF DRPM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO DRPT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * DV -> RI WHEN OPEN DATE IS PAST
      * RI -> CP WHEN OPEN DATE IS PAST
      * CP -> WH WHEN OPEN DATE IS PAST
      * WH -> SL WHEN OPEN DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3110-SET-TRAN.
           EVALUATE DRPT-TRAN-CD
               WHEN 'DV'
                   IF DRPM-OPEN-DATE > 0 AND DRPM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RI' TO DRPT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RI'
                   IF DRPM-OPEN-DATE > 0 AND DRPM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CP' TO DRPT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CP'
                   IF DRPM-OPEN-DATE > 0 AND DRPM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WH' TO DRPT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WH'
                   IF DRPM-OPEN-DATE > 0 AND DRPM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SL' TO DRPT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3120-DESCRIBE-SOURCE.
           EVALUATE DRPM-SOURCE-CD
               WHEN 'BT'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'FL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'NK'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'YC'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3130-ACCRUE-RATE.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * DRPM-RATE / 100 / 365
               ADD WS-WORK-AMT TO DRPM-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3140-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF DRPT-EFF-DATE > 0 AND DRPT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRPM-DIVIDEND-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD DRPM-DIVIDEND-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD DRPM-DIVIDEND-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD DRPM-DIVIDEND-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRPM-DIVIDEND-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E791 DESC REQUIRED
      * E126 SHARES AMT YTD NEGATIVE
      * E991 SHARES AMT YTD OVER 1000000
      * E981 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3150-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPM-DESC = SPACES
                   MOVE 'E791' TO WS-REJECT-CODE
               WHEN DRPM-SHARES-AMT-YTD < 0
                   MOVE 'E126' TO WS-REJECT-CODE
               WHEN DRPM-SHARES-AMT-YTD > 1000000.00
                   MOVE 'E991' TO WS-REJECT-CODE
               WHEN DRPM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E981' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3160-ACCRUE-FEE.
           IF DRPT-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRPT-FEE-AMT * DRPM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO DRPM-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3170-COUNT-ITEM.
           IF DRPM-ITEM-CNT < 9999999
               ADD 1 TO DRPM-ITEM-CNT
           END-IF
           IF DRPM-ITEM-CNT NOT < 6
               MOVE 'Y' TO DRPM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DIVIDEND BAND TIER FOR THE CASH PAY YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3180-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-DIVIDEND-BAND-RATE(1) TO DRPM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF DRPM-CASH-PAY-YTD NOT < WS-DIVIDEND-BAND-LOW(WS-TX)
                   MOVE WS-DIVIDEND-BAND-RATE(WS-TX) TO DRPM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * REINVEST YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3190-FLAG-REINVEST.
           IF DRPM-REINVEST-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE DRPM-REINVEST-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN EFFECTIVE DATE IS PAST
      * BAT -> MAL WHEN EFFECTIVE DATE IS PAST
      * MAL -> TEL WHEN EFFECTIVE DATE IS PAST
      * TEL -> ONL WHEN EFFECTIVE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3200-SET-CHANNEL.
           EVALUATE DRPT-CHANNEL-CD
               WHEN 'BRN'
                   IF DRPM-EFFECTIVE-DATE > 0 AND DRPM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO DRPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF DRPM-EFFECTIVE-DATE > 0 AND DRPM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO DRPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF DRPM-EFFECTIVE-DATE > 0 AND DRPM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO DRPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF DRPM-EFFECTIVE-DATE > 0 AND DRPM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO DRPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E829 REJECT CD REQUIRED
      * E940 FEE YTD NEGATIVE
      * E831 FEE YTD OVER 25000
      * E396 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3210-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPT-REJECT-CD = SPACES
                   MOVE 'E829' TO WS-REJECT-CODE
               WHEN DRPM-FEE-YTD < 0
                   MOVE 'E940' TO WS-REJECT-CODE
               WHEN DRPM-FEE-YTD > 25000.00
                   MOVE 'E831' TO WS-REJECT-CODE
               WHEN DRPM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E396' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3220-DESCRIBE-CYCLE.
           EVALUATE DRPM-CYCLE-CD
               WHEN 'DE'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'FB'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'PC'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'WY'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'XD'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ZK'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ZR'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3230-COUNT-NOTICE.
           IF DRPM-NOTICE-CNT < 9999999
               ADD 1 TO DRPM-NOTICE-CNT
           END-IF
           IF DRPM-NOTICE-CNT NOT < 6
               MOVE 'Y' TO DRPM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SHARES AMT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO REINVEST YTD.
      *----------------------------------------------------------------*
       3240-PRORATE-REINVEST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRPM-REINVEST-YTD ROUNDED =
                   DRPM-SHARES-AMT-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO DRPM-REINVEST-YTD
           END-IF
           ADD DRPM-REINVEST-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3250-ACCRUE-DISCOUNT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * DRPM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO DRPM-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3260-DESCRIBE-REASON.
           EVALUATE DRPM-REASON-CD
               WHEN 'B'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'G'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'J'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'K'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'L'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'T'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'Y'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3270-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF DRPM-NOTICE-DATE > 0 AND DRPM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRPT-REINVEST-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD DRPT-REINVEST-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD DRPT-REINVEST-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD DRPT-REINVEST-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRPT-REINVEST-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WITHHOLD YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3280-SPLIT-WITHHOLD.
           COMPUTE WS-WORK-AMT = DRPM-WITHHOLD-YTD / 4
           COMPUTE WS-WORK-AMT-2 = DRPM-WITHHOLD-YTD - (WS-WORK-AMT * 4)
           COMPUTE DRPM-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E422 CUST ID REQUIRED
      * E120 SHARES AMT AMT NEGATIVE
      * E120 SHARES AMT AMT OVER 10000
      * E779 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3290-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPM-CUST-ID = SPACES
                   MOVE 'E422' TO WS-REJECT-CODE
               WHEN DRPT-SHARES-AMT-AMT < 0
                   MOVE 'E120' TO WS-REJECT-CODE
               WHEN DRPT-SHARES-AMT-AMT > 10000.00
                   MOVE 'E120' TO WS-REJECT-CODE
               WHEN DRPM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E779' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE SHARES AMT AMT BY CHANNEL CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3300-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPM-CHANNEL-CD
                   ADD DRPM-SHARES-AMT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPM-SHARES-AMT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3310-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF DRPM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO DRPM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DIVIDEND FEE ON THE FEE AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3320-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN DRPM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO DRPT-DIVIDEND-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPM-PREFERRED-FLAG = 'Y'
                   COMPUTE DRPT-DIVIDEND-AMT ROUNDED = DRPT-FEE-AMT *
                       DRPM-DISCOUNT / 200
               WHEN DRPM-REASON-CD = 'B'
                   MOVE 10.00 TO DRPT-DIVIDEND-AMT
               WHEN DRPM-REASON-CD = 'G'
                   MOVE 12.00 TO DRPT-DIVIDEND-AMT
               WHEN OTHER
                   COMPUTE DRPT-DIVIDEND-AMT ROUNDED = DRPT-FEE-AMT *
                       DRPM-DISCOUNT / 100
           END-EVALUATE
           IF DRPT-DIVIDEND-AMT > 0 AND DRPT-DIVIDEND-AMT < 1.00
               MOVE 1.00 TO DRPT-DIVIDEND-AMT
           END-IF
           IF DRPT-DIVIDEND-AMT > 250.00
               MOVE 250.00 TO DRPT-DIVIDEND-AMT
           END-IF
           ADD DRPT-DIVIDEND-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 180 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3330-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF DRPM-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO DRPM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3340-ACCRUE-FEE.
           IF DRPM-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRPM-FEE-AMT * DRPM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO DRPM-SHARES-AMT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3350-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF DRPM-NEXT-REVIEW-DATE > 0 AND DRPM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRPM-WITHHOLD-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD DRPM-WITHHOLD-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD DRPM-WITHHOLD-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD DRPM-WITHHOLD-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRPM-WITHHOLD-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3360-LIMIT-FEE.
           IF DRPM-FEE-AMT < 10.00
               MOVE 10.00 TO DRPM-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRPM-FEE-AMT > 50000.00
               MOVE 50000.00 TO DRPM-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3370-DESCRIBE-CYCLE.
           EVALUATE DRPM-CYCLE-CD
               WHEN 'DE'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'FB'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'PC'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'WY'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'XD'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ZK'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'ZR'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3380-COUNT-ITEM.
           IF DRPM-ITEM-CNT < 9999999
               ADD 1 TO DRPM-ITEM-CNT
           END-IF
           IF DRPM-ITEM-CNT NOT < 5
               MOVE 'Y' TO DRPM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E524 REF NO REQUIRED
      * E061 DIVIDEND YTD NEGATIVE
      * E785 DIVIDEND YTD OVER 99999
      * E717 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3390-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPM-REF-NO = SPACES
                   MOVE 'E524' TO WS-REJECT-CODE
               WHEN DRPM-DIVIDEND-YTD < 0
                   MOVE 'E061' TO WS-REJECT-CODE
               WHEN DRPM-DIVIDEND-YTD > 99999.00
                   MOVE 'E785' TO WS-REJECT-CODE
               WHEN DRPM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E717' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * DE -> FB WHEN NEXT REVIEW DATE IS PAST
      * FB -> PC WHEN NEXT REVIEW DATE IS PAST
      * PC -> WY WHEN NEXT REVIEW DATE IS PAST
      * WY -> XD WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3400-SET-CYCLE.
           EVALUATE DRPM-CYCLE-CD
               WHEN 'DE'
                   IF DRPM-NEXT-REVIEW-DATE > 0 AND
                       DRPM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'FB' TO DRPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FB'
                   IF DRPM-NEXT-REVIEW-DATE > 0 AND
                       DRPM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'PC' TO DRPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PC'
                   IF DRPM-NEXT-REVIEW-DATE > 0 AND
                       DRPM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'WY' TO DRPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WY'
                   IF DRPM-NEXT-REVIEW-DATE > 0 AND
                       DRPM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'XD' TO DRPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE SHARES YTD FEE ON THE REINVEST AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3410-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN DRPT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO DRPM-SHARES-AMT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE DRPM-SHARES-AMT-YTD ROUNDED =
                       DRPT-REINVEST-AMT * DRPM-RATE / 200
               WHEN DRPM-SOURCE-CD = 'BT'
                   MOVE 12.00 TO DRPM-SHARES-AMT-YTD
               WHEN DRPM-SOURCE-CD = 'FL'
                   MOVE 12.00 TO DRPM-SHARES-AMT-YTD
               WHEN OTHER
                   COMPUTE DRPM-SHARES-AMT-YTD ROUNDED =
                       DRPT-REINVEST-AMT * DRPM-RATE / 100
           END-EVALUATE
           IF DRPM-SHARES-AMT-YTD > 0 AND DRPM-SHARES-AMT-YTD < 10.00
               MOVE 10.00 TO DRPM-SHARES-AMT-YTD
           END-IF
           IF DRPM-SHARES-AMT-YTD > 35.00
               MOVE 35.00 TO DRPM-SHARES-AMT-YTD
           END-IF
           ADD DRPM-SHARES-AMT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR HOLDER TYPE CD:
      * C -> H WHEN POST DATE IS PAST
      * H -> K WHEN POST DATE IS PAST
      * K -> L WHEN POST DATE IS PAST
      * L -> M WHEN POST DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3420-SET-HOLDER-TYPE.
           EVALUATE DRPM-HOLDER-TYPE-CD
               WHEN 'C'
                   IF DRPT-POST-DATE > 0 AND DRPT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO DRPM-HOLDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF DRPT-POST-DATE > 0 AND DRPT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'K' TO DRPM-HOLDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF DRPT-POST-DATE > 0 AND DRPT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO DRPM-HOLDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF DRPT-POST-DATE > 0 AND DRPT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO DRPM-HOLDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE WITHHOLD BAND TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3430-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-WITHHOLD-BAND-RATE(1) TO DRPM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF WS-WORK-AMT NOT < WS-WITHHOLD-BAND-LOW(WS-TX)
                   MOVE WS-WITHHOLD-BAND-RATE(WS-TX) TO DRPM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DIVIDEND BAND TIER FOR THE DIVIDEND AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3440-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-DIVIDEND-BAND-RATE(1) TO DRPM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF DRPT-DIVIDEND-AMT NOT < WS-DIVIDEND-BAND-LOW(WS-TX)
                   MOVE WS-DIVIDEND-BAND-RATE(WS-TX) TO DRPM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT REINVEST AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3450-SPLIT-REINVEST.
           COMPUTE WS-WORK-AMT = DRPT-REINVEST-AMT / 12
           COMPUTE WS-WORK-AMT-2 = DRPT-REINVEST-AMT - (WS-WORK-AMT *
               12)
           COMPUTE DRPM-DIVIDEND-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT REINVEST AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3460-SPLIT-REINVEST.
           COMPUTE WS-WORK-AMT = DRPM-REINVEST-AMT / 12
           COMPUTE WS-WORK-AMT-2 = DRPM-REINVEST-AMT - (WS-WORK-AMT *
               12)
           COMPUTE DRPM-WITHHOLD-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3470-DESCRIBE-CHANNEL.
           EVALUATE DRPM-CHANNEL-CD
               WHEN 'RM'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'VP'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'YS'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SHARES AMT AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3480-FLAG-SHARES-AMT.
           IF DRPM-SHARES-AMT-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE DRPM-SHARES-AMT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CASH PAY YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3490-ACCRUE-CASH-PAY.
           IF DRPM-CASH-PAY-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRPM-CASH-PAY-YTD * DRPM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO DRPM-SHARES-AMT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3500-FLAG-FEE.
           IF DRPT-FEE-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE DRPT-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3510-COUNT-EXCEPT.
           IF DRPM-EXCEPT-CNT < 9999999
               ADD 1 TO DRPM-EXCEPT-CNT
           END-IF
           IF DRPM-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO DRPM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3520-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF DRPM-LAST-MAINT-DATE > 0 AND DRPM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRPM-REINVEST-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD DRPM-REINVEST-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD DRPM-REINVEST-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD DRPM-REINVEST-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRPM-REINVEST-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3530-DESCRIBE-TRAN.
           EVALUATE DRPT-TRAN-CD
               WHEN 'DV'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'RI'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'CP'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'WH'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'SL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3540-COUNT-ITEM.
           IF DRPM-ITEM-CNT < 9999999
               ADD 1 TO DRPM-ITEM-CNT
           END-IF
           IF DRPM-ITEM-CNT NOT < 12
               MOVE 'Y' TO DRPT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SHARES AMT AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3550-SPLIT-SHARES-AMT.
           COMPUTE WS-WORK-AMT = DRPM-SHARES-AMT-AMT / 12
           COMPUTE WS-WORK-AMT-2 = DRPM-SHARES-AMT-AMT - (WS-WORK-AMT *
               12)
           COMPUTE DRPT-SHARES-AMT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 10 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3560-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF DRPT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO DRPM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3570-COUNT-SEQ.
           IF DRPT-SEQ < 9999999
               ADD 1 TO DRPT-SEQ
           END-IF
           IF DRPT-SEQ NOT < 5
               MOVE 'Y' TO DRPM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE WITHHOLD BAND TIER FOR THE SHARES AMT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3580-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-WITHHOLD-BAND-RATE(1) TO DRPM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF DRPM-SHARES-AMT-AMT NOT < WS-WITHHOLD-BAND-LOW(WS-TX)
                   MOVE WS-WITHHOLD-BAND-RATE(WS-TX) TO DRPM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3590-COUNT-EXCEPT.
           IF DRPM-EXCEPT-CNT < 9999999
               ADD 1 TO DRPM-EXCEPT-CNT
           END-IF
           IF DRPM-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO DRPM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CASH PAY AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DIVIDEND AMT.
      *----------------------------------------------------------------*
       3600-PRORATE-DIVIDEND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRPM-DIVIDEND-AMT ROUNDED =
                   DRPM-CASH-PAY-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO DRPM-DIVIDEND-AMT
           END-IF
           ADD DRPM-DIVIDEND-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * CASH PAY YTD IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3610-LIMIT-CASH-PAY.
           IF DRPM-CASH-PAY-YTD < 10.00
               MOVE 10.00 TO DRPM-CASH-PAY-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRPM-CASH-PAY-YTD > 1000.00
               MOVE 1000.00 TO DRPM-CASH-PAY-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E520 MEMO REQUIRED
      * E334 DIVIDEND AMT NEGATIVE
      * E772 DIVIDEND AMT OVER 10000
      * E243 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3620-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPT-MEMO = SPACES
                   MOVE 'E520' TO WS-REJECT-CODE
               WHEN DRPT-DIVIDEND-AMT < 0
                   MOVE 'E334' TO WS-REJECT-CODE
               WHEN DRPT-DIVIDEND-AMT > 10000.00
                   MOVE 'E772' TO WS-REJECT-CODE
               WHEN DRPM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E243' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CASH PAY AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3630-SPLIT-CASH-PAY.
           COMPUTE WS-WORK-AMT = DRPM-CASH-PAY-AMT / 4
           COMPUTE WS-WORK-AMT-2 = DRPM-CASH-PAY-AMT - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E523 REF NO REQUIRED
      * E653 REINVEST YTD NEGATIVE
      * E319 REINVEST YTD OVER 250000
      * E034 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3640-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPM-REF-NO = SPACES
                   MOVE 'E523' TO WS-REJECT-CODE
               WHEN DRPM-REINVEST-YTD < 0
                   MOVE 'E653' TO WS-REJECT-CODE
               WHEN DRPM-REINVEST-YTD > 250000.00
                   MOVE 'E319' TO WS-REJECT-CODE
               WHEN DRPM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E034' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REINVEST YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3650-ACCRUE-REINVEST.
           IF DRPM-REINVEST-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRPM-REINVEST-YTD * DRPM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO DRPM-DIVIDEND-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WITHHOLD YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FEE AMT.
      *----------------------------------------------------------------*
       3660-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRPT-FEE-AMT ROUNDED =
                   DRPM-WITHHOLD-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO DRPT-FEE-AMT
           END-IF
           ADD DRPT-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE REINVEST FEE ON THE WORK AMT 2.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3670-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN DRPT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO DRPM-REINVEST-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPM-SUSPEND-FLAG = 'Y'
                   COMPUTE DRPM-REINVEST-AMT ROUNDED = WS-WORK-AMT-2 *
                       DRPM-PENALTY-PCT / 200
               WHEN DRPM-CYCLE-CD = 'DE'
                   MOVE 15.00 TO DRPM-REINVEST-AMT
               WHEN DRPM-CYCLE-CD = 'FB'
                   MOVE 20.00 TO DRPM-REINVEST-AMT
               WHEN OTHER
                   COMPUTE DRPM-REINVEST-AMT ROUNDED = WS-WORK-AMT-2 *
                       DRPM-PENALTY-PCT / 100
           END-EVALUATE
           IF DRPM-REINVEST-AMT > 0 AND DRPM-REINVEST-AMT < 2.00
               MOVE 2.00 TO DRPM-REINVEST-AMT
           END-IF
           IF DRPM-REINVEST-AMT > 75.00
               MOVE 75.00 TO DRPM-REINVEST-AMT
           END-IF
           ADD DRPM-REINVEST-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3680-LIMIT-ITEM.
           IF WS-WORK-AMT < 0.00
               MOVE 0.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 500.00
               MOVE 500.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WITHHOLD AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3690-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPM-CYCLE-CD
                   ADD DRPM-WITHHOLD-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPM-WITHHOLD-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 45 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3700-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF DRPM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO DRPM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CASH PAY YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3710-SPLIT-CASH-PAY.
           COMPUTE WS-WORK-AMT = DRPM-CASH-PAY-YTD / 4
           COMPUTE WS-WORK-AMT-2 = DRPM-CASH-PAY-YTD - (WS-WORK-AMT * 4)
           COMPUTE DRPM-REINVEST-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE REINVEST YTD FEE ON THE SHARES AMT AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3720-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN DRPT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO DRPM-REINVEST-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPM-EXEMPT-FLAG = 'Y'
                   COMPUTE DRPM-REINVEST-YTD ROUNDED =
                       DRPT-SHARES-AMT-AMT * DRPM-RATE / 200
               WHEN DRPM-HOLDER-TYPE-CD = 'C'
                   MOVE 20.00 TO DRPM-REINVEST-YTD
               WHEN DRPM-HOLDER-TYPE-CD = 'H'
                   MOVE 15.00 TO DRPM-REINVEST-YTD
               WHEN OTHER
                   COMPUTE DRPM-REINVEST-YTD ROUNDED =
                       DRPT-SHARES-AMT-AMT * DRPM-RATE / 100
           END-EVALUATE
           IF DRPM-REINVEST-YTD > 0 AND DRPM-REINVEST-YTD < 5.00
               MOVE 5.00 TO DRPM-REINVEST-YTD
           END-IF
           IF DRPM-REINVEST-YTD > 100.00
               MOVE 100.00 TO DRPM-REINVEST-YTD
           END-IF
           ADD DRPM-REINVEST-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE WITHHOLD AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3730-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPM-SOURCE-CD
                   ADD DRPM-WITHHOLD-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPM-WITHHOLD-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WITHHOLD YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3740-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPM-SOURCE-CD
                   ADD DRPM-WITHHOLD-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPM-WITHHOLD-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E419 CONTACT REQUIRED
      * E878 WITHHOLD AMT NEGATIVE
      * E872 WITHHOLD AMT OVER 5000
      * E887 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3750-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPM-CONTACT = SPACES
                   MOVE 'E419' TO WS-REJECT-CODE
               WHEN DRPM-WITHHOLD-AMT < 0
                   MOVE 'E878' TO WS-REJECT-CODE
               WHEN DRPM-WITHHOLD-AMT > 5000.00
                   MOVE 'E872' TO WS-REJECT-CODE
               WHEN DRPM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E887' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SHARES AMT AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3760-ACCRUE-SHARES-AMT.
           IF DRPT-SHARES-AMT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRPT-SHARES-AMT-AMT * DRPM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO DRPM-REINVEST-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E641 MEMO REQUIRED
      * E763 REINVEST AMT NEGATIVE
      * E826 REINVEST AMT OVER 5000
      * E202 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3770-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPT-MEMO = SPACES
                   MOVE 'E641' TO WS-REJECT-CODE
               WHEN DRPM-REINVEST-AMT < 0
                   MOVE 'E763' TO WS-REJECT-CODE
               WHEN DRPM-REINVEST-AMT > 5000.00
                   MOVE 'E826' TO WS-REJECT-CODE
               WHEN DRPM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E202' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3780-LIMIT-FEE.
           IF DRPT-FEE-AMT < 0.00
               MOVE 0.00 TO DRPT-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRPT-FEE-AMT > 10000.00
               MOVE 10000.00 TO DRPT-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3790-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF DRPM-NEXT-DUE-DATE > 0 AND DRPM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRPM-SHARES-AMT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD DRPM-SHARES-AMT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD DRPM-SHARES-AMT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD DRPM-SHARES-AMT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRPM-SHARES-AMT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3800-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3810-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF DRPM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO DRPM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SHARES FEE ON THE FEE YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3820-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN DRPM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO DRPT-SHARES-AMT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPT-REVERSAL-FLAG = 'Y'
                   COMPUTE DRPT-SHARES-AMT-AMT ROUNDED = DRPM-FEE-YTD *
                       DRPM-RATE / 200
               WHEN DRPM-SOURCE-CD = 'BT'
                   MOVE 10.00 TO DRPT-SHARES-AMT-AMT
               WHEN DRPM-SOURCE-CD = 'FL'
                   MOVE 10.00 TO DRPT-SHARES-AMT-AMT
               WHEN OTHER
                   COMPUTE DRPT-SHARES-AMT-AMT ROUNDED = DRPM-FEE-YTD *
                       DRPM-RATE / 100
           END-EVALUATE
           IF DRPT-SHARES-AMT-AMT > 0 AND DRPT-SHARES-AMT-AMT < 2.00
               MOVE 2.00 TO DRPT-SHARES-AMT-AMT
           END-IF
           IF DRPT-SHARES-AMT-AMT > 150.00
               MOVE 150.00 TO DRPT-SHARES-AMT-AMT
           END-IF
           ADD DRPT-SHARES-AMT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DIVIDEND AMT.
      *----------------------------------------------------------------*
       3830-PRORATE-DIVIDEND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRPT-DIVIDEND-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO DRPT-DIVIDEND-AMT
           END-IF
           ADD DRPT-DIVIDEND-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E383 DESC REQUIRED
      * E433 REINVEST YTD NEGATIVE
      * E076 REINVEST YTD OVER 1000000
      * E102 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3840-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPM-DESC = SPACES
                   MOVE 'E383' TO WS-REJECT-CODE
               WHEN DRPM-REINVEST-YTD < 0
                   MOVE 'E433' TO WS-REJECT-CODE
               WHEN DRPM-REINVEST-YTD > 1000000.00
                   MOVE 'E076' TO WS-REJECT-CODE
               WHEN DRPM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E102' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3850-COUNT-EXCEPT.
           IF DRPM-EXCEPT-CNT < 9999999
               ADD 1 TO DRPM-EXCEPT-CNT
           END-IF
           IF DRPM-EXCEPT-CNT NOT < 3
               MOVE 'Y' TO DRPM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * BT -> FL WHEN LAST ACT DATE IS PAST
      * FL -> NK WHEN LAST ACT DATE IS PAST
      * NK -> YC WHEN LAST ACT DATE IS PAST
      * YC -> BT WHEN LAST ACT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3860-SET-SOURCE.
           EVALUATE DRPM-SOURCE-CD
               WHEN 'BT'
                   IF DRPM-LAST-ACT-DATE > 0 AND DRPM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FL' TO DRPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FL'
                   IF DRPM-LAST-ACT-DATE > 0 AND DRPM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NK' TO DRPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NK'
                   IF DRPM-LAST-ACT-DATE > 0 AND DRPM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YC' TO DRPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YC'
                   IF DRPM-LAST-ACT-DATE > 0 AND DRPM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BT' TO DRPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3870-COUNT-PAYMENT.
           IF DRPM-PAYMENT-CNT < 9999999
               ADD 1 TO DRPM-PAYMENT-CNT
           END-IF
           IF DRPM-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO DRPM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CASH PAY AMT BY PLAN OPTION CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3880-ACCUM-PLAN-OPTION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPM-PLAN-OPTION-CD
                   ADD DRPM-CASH-PAY-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPM-PLAN-OPTION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPM-CASH-PAY-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FEE YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3890-FLAG-FEE.
           IF DRPM-FEE-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE DRPM-FEE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3900-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = DRPM-FEE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = DRPM-FEE-AMT - (WS-WORK-AMT * 3)
           COMPUTE DRPM-WITHHOLD-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CASH PAY AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3910-ACCRUE-CASH-PAY.
           IF DRPM-CASH-PAY-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRPM-CASH-PAY-AMT * DRPM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO DRPM-REINVEST-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3920-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF DRPM-LAST-MAINT-DATE > 0 AND DRPM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRPM-WITHHOLD-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD DRPM-WITHHOLD-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD DRPM-WITHHOLD-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD DRPM-WITHHOLD-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRPM-WITHHOLD-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CASH PAY YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3930-ACCRUE-CASH-PAY.
           IF DRPM-CASH-PAY-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRPM-CASH-PAY-YTD * DRPM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO DRPM-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CASH PAY AMT BY HOLDER TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3940-ACCUM-HOLDER-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPM-HOLDER-TYPE-CD
                   ADD DRPM-CASH-PAY-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPM-HOLDER-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPM-CASH-PAY-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 60 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3950-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF DRPT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO DRPM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WITHHOLD AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3960-FLAG-WITHHOLD.
           IF DRPM-WITHHOLD-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE DRPM-WITHHOLD-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 180 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3970-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF DRPM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO DRPT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3980-ACCRUE-RATE.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * DRPM-RATE / 100 / 365
               ADD WS-WORK-AMT TO DRPM-DIVIDEND-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE WITHHOLD BAND TIER FOR THE DIVIDEND AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3990-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-WITHHOLD-BAND-RATE(1) TO DRPM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF DRPT-DIVIDEND-AMT NOT < WS-WITHHOLD-BAND-LOW(WS-TX)
                   MOVE WS-WITHHOLD-BAND-RATE(WS-TX) TO DRPM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       4000-COUNT-NOTICE.
           IF DRPM-NOTICE-CNT < 9999999
               ADD 1 TO DRPM-NOTICE-CNT
           END-IF
           IF DRPM-NOTICE-CNT NOT < 5
               MOVE 'Y' TO DRPM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4010-SPLIT-SHARES-AMT.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 2)
           COMPUTE DRPM-SHARES-AMT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4020-DESCRIBE-TRAN.
           EVALUATE DRPT-TRAN-CD
               WHEN 'DV'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'RI'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'CP'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'WH'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'SL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E145 NAME REQUIRED
      * E186 DIVIDEND AMT NEGATIVE
      * E104 DIVIDEND AMT OVER 50000
      * E618 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       4030-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPM-NAME = SPACES
                   MOVE 'E145' TO WS-REJECT-CODE
               WHEN DRPT-DIVIDEND-AMT < 0
                   MOVE 'E186' TO WS-REJECT-CODE
               WHEN DRPT-DIVIDEND-AMT > 50000.00
                   MOVE 'E104' TO WS-REJECT-CODE
               WHEN DRPM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E618' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4040-COUNT-ITEM.
           IF DRPM-ITEM-CNT < 9999999
               ADD 1 TO DRPM-ITEM-CNT
           END-IF
           IF DRPM-ITEM-CNT NOT < 5
               MOVE 'Y' TO DRPM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE WITHHOLD BAND TIER FOR THE FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4050-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-WITHHOLD-BAND-RATE(1) TO DRPM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF DRPM-FEE-YTD NOT < WS-WITHHOLD-BAND-LOW(WS-TX)
                   MOVE WS-WITHHOLD-BAND-RATE(WS-TX) TO DRPM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4060-DESCRIBE-REASON.
           EVALUATE DRPM-REASON-CD
               WHEN 'B'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'G'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'J'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'K'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'L'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'T'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'Y'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE WITHHOLD BAND TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4070-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-WITHHOLD-BAND-RATE(1) TO DRPM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF WS-WORK-AMT NOT < WS-WITHHOLD-BAND-LOW(WS-TX)
                   MOVE WS-WITHHOLD-BAND-RATE(WS-TX) TO DRPM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DIVIDEND YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WITHHOLD AMT.
      *----------------------------------------------------------------*
       4080-PRORATE-WITHHOLD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRPM-WITHHOLD-AMT ROUNDED =
                   DRPM-DIVIDEND-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO DRPM-WITHHOLD-AMT
           END-IF
           ADD DRPM-WITHHOLD-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE DIVIDEND FEE ON THE WORK AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4090-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN DRPM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO DRPM-DIVIDEND-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPT-REVERSAL-FLAG = 'Y'
                   COMPUTE DRPM-DIVIDEND-AMT ROUNDED = WS-WORK-AMT *
                       DRPM-RATE / 200
               WHEN DRPT-TRAN-CD = 'DV'
                   MOVE 12.00 TO DRPM-DIVIDEND-AMT
               WHEN DRPT-TRAN-CD = 'RI'
                   MOVE 5.00 TO DRPM-DIVIDEND-AMT
               WHEN OTHER
                   COMPUTE DRPM-DIVIDEND-AMT ROUNDED = WS-WORK-AMT *
                       DRPM-RATE / 100
           END-EVALUATE
           IF DRPM-DIVIDEND-AMT > 0 AND DRPM-DIVIDEND-AMT < 2.00
               MOVE 2.00 TO DRPM-DIVIDEND-AMT
           END-IF
           IF DRPM-DIVIDEND-AMT > 100.00
               MOVE 100.00 TO DRPM-DIVIDEND-AMT
           END-IF
           ADD DRPM-DIVIDEND-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E999 CUST ID REQUIRED
      * E592 REINVEST AMT NEGATIVE
      * E627 REINVEST AMT OVER 99999
      * E428 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       4100-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPM-CUST-ID = SPACES
                   MOVE 'E999' TO WS-REJECT-CODE
               WHEN DRPT-REINVEST-AMT < 0
                   MOVE 'E592' TO WS-REJECT-CODE
               WHEN DRPT-REINVEST-AMT > 99999.00
                   MOVE 'E627' TO WS-REJECT-CODE
               WHEN DRPM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E428' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4110-ACCRUE-FEE.
           IF DRPM-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRPM-FEE-AMT * DRPM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO DRPM-SHARES-AMT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DIVIDEND BAND TIER FOR THE REINVEST YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4120-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-DIVIDEND-BAND-RATE(1) TO DRPM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF DRPM-REINVEST-YTD NOT < WS-DIVIDEND-BAND-LOW(WS-TX)
                   MOVE WS-DIVIDEND-BAND-RATE(WS-TX) TO DRPM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CASH PAY AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4130-ACCRUE-CASH-PAY.
           IF DRPM-CASH-PAY-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRPM-CASH-PAY-AMT * DRPM-RATE / 100 / 360
               ADD WS-WORK-AMT TO DRPM-WITHHOLD-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4140-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF DRPM-NEXT-REVIEW-DATE > 0 AND DRPM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRPM-SHARES-AMT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD DRPM-SHARES-AMT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD DRPM-SHARES-AMT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD DRPM-SHARES-AMT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRPM-SHARES-AMT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E168 OPERATOR REQUIRED
      * E573 FEE AMT NEGATIVE
      * E865 FEE AMT OVER 1000000
      * E507 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       4150-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPT-OPERATOR = SPACES
                   MOVE 'E168' TO WS-REJECT-CODE
               WHEN DRPT-FEE-AMT < 0
                   MOVE 'E573' TO WS-REJECT-CODE
               WHEN DRPT-FEE-AMT > 1000000.00
                   MOVE 'E865' TO WS-REJECT-CODE
               WHEN DRPM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E507' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DIVIDEND BAND TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4160-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-DIVIDEND-BAND-RATE(1) TO DRPM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF DRPM-FEE-AMT NOT < WS-DIVIDEND-BAND-LOW(WS-TX)
                   MOVE WS-DIVIDEND-BAND-RATE(WS-TX) TO DRPM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4170-COUNT-NOTICE.
           IF DRPM-NOTICE-CNT < 9999999
               ADD 1 TO DRPM-NOTICE-CNT
           END-IF
           IF DRPM-NOTICE-CNT NOT < 10
               MOVE 'Y' TO DRPM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * REINVEST AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4180-FLAG-REINVEST.
           IF DRPM-REINVEST-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE DRPM-REINVEST-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4190-DESCRIBE-CYCLE.
           EVALUATE DRPM-CYCLE-CD
               WHEN 'DE'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'FB'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'PC'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'WY'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'XD'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'ZK'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'ZR'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4200-DESCRIBE-SOURCE.
           EVALUATE DRPM-SOURCE-CD
               WHEN 'BT'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'FL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'NK'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'YC'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE WITHHOLD BAND TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4210-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-WITHHOLD-BAND-RATE(1) TO DRPM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF DRPT-FEE-AMT NOT < WS-WITHHOLD-BAND-LOW(WS-TX)
                   MOVE WS-WITHHOLD-BAND-RATE(WS-TX) TO DRPM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 45 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4220-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF DRPT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO DRPM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE DRP FIELDS TO DRFEE AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7030-CALL-DRFEE.
           INITIALIZE DRFEE-PARMS
           MOVE DRPM-CASH-PAY-YTD
             TO DRFEE-CASH-PAY-YTD
           MOVE DRPM-WITHHOLD-YTD
             TO DRFEE-WITHHOLD-YTD
           MOVE DRPM-SHARES-AMT-AMT
             TO DRFEE-SHARES-AMT-AMT
           MOVE DRPM-FEE-AMT
             TO DRFEE-FEE-AMT
           MOVE DRPM-REINVEST-YTD
             TO DRFEE-REINVEST-YTD
           MOVE DRPM-SHARES-AMT-YTD
             TO DRFEE-SHARES-AMT-YTD
           MOVE DRPM-NOTICE-DATE
             TO DRFEE-NOTICE-DATE
           MOVE DRPM-LAST-PAY-DATE
             TO DRFEE-LAST-PAY-DATE
           MOVE DRPM-NEXT-REVIEW-DATE
             TO DRFEE-NEXT-REVIEW-DATE
           MOVE DRPM-OPEN-DATE
             TO DRFEE-OPEN-DATE
           MOVE DRPM-REASON-CD
             TO DRFEE-REASON-CD
           MOVE DRPM-CYCLE-CD
             TO DRFEE-CYCLE-CD
           MOVE DRPM-PLAN-OPTION-CD
             TO DRFEE-PLAN-OPTION-CD
           MOVE DRPM-HOLDER-TYPE-CD
             TO DRFEE-HOLDER-TYPE-CD
           MOVE DRPM-OVERRIDE-FLAG
             TO DRFEE-OVERRIDE-FLAG
           MOVE DRPM-SUSPEND-FLAG
             TO DRFEE-SUSPEND-FLAG
           MOVE DRPM-PREFERRED-FLAG
             TO DRFEE-PREFERRED-FLAG
           MOVE DRPM-PAYMENT-CNT
             TO DRFEE-PAYMENT-CNT
           MOVE DRPM-NOTICE-CNT
             TO DRFEE-NOTICE-CNT
           MOVE DRPM-DISCOUNT
             TO DRFEE-DISCOUNT
           MOVE DRPM-RATE
             TO DRFEE-RATE
           MOVE DRPM-DESC
             TO DRFEE-DESC
           MOVE DRPM-ADDR
             TO DRFEE-ADDR
           CALL 'DRFEE' USING DRFEE-PARMS
           EVALUATE DRFEE-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE DRFEE-FEE-AMT
                     TO DRPM-FEE-AMT
                   MOVE DRFEE-REINVEST-YTD
                     TO DRPM-REINVEST-YTD
                   MOVE DRFEE-SHARES-AMT-YTD
                     TO DRPM-SHARES-AMT-YTD
                   MOVE DRFEE-OVERRIDE-FLAG
                     TO DRPM-OVERRIDE-FLAG
                   MOVE DRFEE-PREFERRED-FLAG
                     TO DRPM-PREFERRED-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RFEE' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7030-CALL-DRFEE' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE DRP FIELDS TO DRSTAT AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7000-CALL-DRSTAT.
           INITIALIZE DRSTAT-PARMS
           MOVE DRPM-CASH-PAY-AMT
             TO DRSTAT-CASH-PAY-AMT
           MOVE DRPM-FEE-YTD
             TO DRSTAT-FEE-YTD
           MOVE DRPM-DIVIDEND-AMT
             TO DRSTAT-DIVIDEND-AMT
           MOVE DRPM-REINVEST-AMT
             TO DRSTAT-REINVEST-AMT
           MOVE DRPM-SHARES-AMT-AMT
             TO DRSTAT-SHARES-AMT-AMT
           MOVE DRPM-WITHHOLD-YTD
             TO DRSTAT-WITHHOLD-YTD
           MOVE DRPM-LAST-PAY-DATE
             TO DRSTAT-LAST-PAY-DATE
           MOVE DRPM-NEXT-REVIEW-DATE
             TO DRSTAT-NEXT-REVIEW-DATE
           MOVE DRPM-NEXT-DUE-DATE
             TO DRSTAT-NEXT-DUE-DATE
           MOVE DRPM-MATURITY-DATE
             TO DRSTAT-MATURITY-DATE
           MOVE DRPM-PLAN-OPTION-CD
             TO DRSTAT-PLAN-OPTION-CD
           MOVE DRPM-REASON-CD
             TO DRSTAT-REASON-CD
           MOVE DRPM-CHANNEL-CD
             TO DRSTAT-CHANNEL-CD
           MOVE DRPM-HOLDER-TYPE-CD
             TO DRSTAT-HOLDER-TYPE-CD
           MOVE DRPM-OVERRIDE-FLAG
             TO DRSTAT-OVERRIDE-FLAG
           MOVE DRPM-STOP-FLAG
             TO DRSTAT-STOP-FLAG
           MOVE DRPM-AUTO-RENEW-FLAG
             TO DRSTAT-AUTO-RENEW-FLAG
           MOVE DRPM-EXCEPT-CNT
             TO DRSTAT-EXCEPT-CNT
           MOVE DRPM-NOTICE-CNT
             TO DRSTAT-NOTICE-CNT
           MOVE DRPM-RATE
             TO DRSTAT-RATE
           MOVE DRPM-DISCOUNT
             TO DRSTAT-DISCOUNT
           MOVE DRPM-CUST-ID
             TO DRSTAT-CUST-ID
           MOVE DRPM-REF-NO
             TO DRSTAT-REF-NO
           CALL 'DRSTAT' USING DRSTAT-PARMS
           EVALUATE DRSTAT-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE DRSTAT-CASH-PAY-AMT
                     TO DRPM-CASH-PAY-AMT
                   MOVE DRSTAT-REINVEST-AMT
                     TO DRPM-REINVEST-AMT
                   MOVE DRSTAT-SHARES-AMT-AMT
                     TO DRPM-SHARES-AMT-AMT
                   MOVE DRSTAT-WITHHOLD-YTD
                     TO DRPM-WITHHOLD-YTD
                   MOVE DRSTAT-OVERRIDE-FLAG
                     TO DRPM-OVERRIDE-FLAG
                   MOVE DRSTAT-STOP-FLAG
                     TO DRPM-STOP-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RTAT' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-DRSTAT' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE DRP FIELDS TO DRELG AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7020-CALL-DRELG.
           INITIALIZE DRELG-PARMS
           MOVE DRPM-REINVEST-AMT
             TO DRELG-REINVEST-AMT
           MOVE DRPM-WITHHOLD-AMT
             TO DRELG-WITHHOLD-AMT
           MOVE DRPM-FEE-YTD
             TO DRELG-FEE-YTD
           MOVE DRPM-SHARES-AMT-AMT
             TO DRELG-SHARES-AMT-AMT
           MOVE DRPM-CASH-PAY-AMT
             TO DRELG-CASH-PAY-AMT
           MOVE DRPM-CASH-PAY-YTD
             TO DRELG-CASH-PAY-YTD
           MOVE DRPM-LAST-MAINT-DATE
             TO DRELG-LAST-MAINT-DATE
           MOVE DRPM-MATURITY-DATE
             TO DRELG-MATURITY-DATE
           MOVE DRPM-OPEN-DATE
             TO DRELG-OPEN-DATE
           MOVE DRPM-EFFECTIVE-DATE
             TO DRELG-EFFECTIVE-DATE
           MOVE DRPM-SOURCE-CD
             TO DRELG-SOURCE-CD
           MOVE DRPM-PLAN-OPTION-CD
             TO DRELG-PLAN-OPTION-CD
           MOVE DRPM-CYCLE-CD
             TO DRELG-CYCLE-CD
           MOVE DRPM-CHANNEL-CD
             TO DRELG-CHANNEL-CD
           MOVE DRPM-STOP-FLAG
             TO DRELG-STOP-FLAG
           MOVE DRPM-SUSPEND-FLAG
             TO DRELG-SUSPEND-FLAG
           MOVE DRPM-PREFERRED-FLAG
             TO DRELG-PREFERRED-FLAG
           MOVE DRPM-EXCEPT-CNT
             TO DRELG-EXCEPT-CNT
           MOVE DRPM-ITEM-CNT
             TO DRELG-ITEM-CNT
           MOVE DRPM-DISCOUNT
             TO DRELG-DISCOUNT
           MOVE DRPM-RATE
             TO DRELG-RATE
           MOVE DRPM-REF-NO
             TO DRELG-REF-NO
           MOVE DRPM-CUST-ID
             TO DRELG-CUST-ID
           CALL 'DRELG' USING DRELG-PARMS
           EVALUATE DRELG-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE DRELG-REINVEST-AMT
                     TO DRPM-REINVEST-AMT
                   MOVE DRELG-CASH-PAY-YTD
                     TO DRPM-CASH-PAY-YTD
                   MOVE DRELG-STOP-FLAG
                     TO DRPM-STOP-FLAG
                   MOVE DRELG-SUSPEND-FLAG
                     TO DRPM-SUSPEND-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RELG' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-DRELG' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE DRPT-KEY TO RD-KEY
           MOVE DRPT-TRAN-CD TO RD-CODE
           MOVE DRPT-FEE-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE DRPT-KEY TO RD-KEY
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
           DISPLAY 'DRPEDIT READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'DRPEDIT' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
