      *================================================================*
      * PROGRAM  : YKBACCR                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : DATE WINDOW BRIDGE - DAILY ACCRUAL. READS THE YDB   *
      *            TRANSACTIONS, UPDATES THE YDB MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. TWO-DIGIT YEAR WINDOWING BRIDGE *
      *            FOR CENTURY DATE CONVERSION                         *
      * INPUT    : YDBMST (KSDS), YDBTRN, BANKCTL                      *
      * OUTPUT   : YDBOUT, YKBACRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, YKLIM, YKPRC, YKTIER    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-07-10 BHS  ORIGINAL                                       *
      * 1992-01-02 RJT  CONTROL ROUNDING - HALF UP REQ2282             *
      * 1994-02-19 PQR  CONTROL ROUNDING - HALF UP CHG7675             *
      * 1995-10-12 TWH  CONTROL TABLE EXPANDED CHG8393                 *
      * 1997-02-01 EWS  AMOUNT-1 CUTOFF MOVED REQ8841                  *
      * 1999-02-18 RJT  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-07-16 EWS  AMOUNT-1 FEE SCHEDULE UPDATE REQ5619           *
      * 2000-08-23 PQR  AUDIT FINDING - CONTROL CONTROL TOTALS CHG3205 *
      * 2000-04-12 CRP  AUDIT FINDING - CONTROL CONTROL TOTALS PRB5673 *
      * 2004-08-26 EWS  AMOUNT-2 CUTOFF MOVED REQ4263                  *
      * 2007-08-02 PQR  CONTROL CUTOFF MOVED REQ4134                   *
      * 2008-11-11 LMB  AMOUNT-1 ROUNDING - HALF UP PRB0184            *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YKBACCR.
       AUTHOR. M K LOWE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "YDBMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS YDBM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "YDBTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "YDBOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "YKBACRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY YDBMST.
       FD  TRAN-FILE.
       COPY YDBTRN.
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
                                           VALUE 8.000000.
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
      * WS-CONTROL-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CONTROL-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000286000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000275500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001050000000317000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001150000000559000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006150000000511000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016150000000818500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016200000000052500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021200000000178000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00031200000000144500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00031300000000082000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00032300000000595500'.
       01  WS-CONTROL-TIER-TBL REDEFINES WS-CONTROL-TIER-VALUES.
           05  WS-CONTROL-TIER-ROW OCCURS 11 TIMES.
               10  WS-CONTROL-TIER-LOW     PIC 9(09)V99.
               10  WS-CONTROL-TIER-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-CONTROL-TIERX - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CONTROL-TIERX-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000550500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000820000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015000000000299500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017500000000760000'.
       01  WS-CONTROL-TIERX-TBL REDEFINES WS-CONTROL-TIERX-VALUES.
           05  WS-CONTROL-TIERX-ROW OCCURS 4 TIMES.
               10  WS-CONTROL-TIERX-LOW    PIC 9(09)V99.
               10  WS-CONTROL-TIERX-RATE   PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  YKBACCR  DATE WINDOW BRIDGE'.
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
       COPY YKLIMP.
       COPY YKPRCP.
       COPY YKTIERP.

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
               MOVE 'YDBMST' TO ABND-FILE-NAME
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
           MOVE YDBT-KEY TO YDBM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-ASSESS-EXEMPT
               PERFORM 3010-ACCRUE-DISCOUNT
               PERFORM 3020-ACCRUE-AMOUNT-2
               PERFORM 3030-DESCRIBE-CHANNEL
               PERFORM 3040-ACCRUE-DISCOUNT
               PERFORM 3050-PRORATE-CONTROL
               PERFORM 3060-LIMIT-ITEM
               PERFORM 3070-ASSESS-REVERSAL
               PERFORM 3080-ACCUM-REGION
               PERFORM 3090-SPLIT-AMOUNT-1
               PERFORM 3100-LIMIT-AMOUNT-1
               PERFORM 3110-COUNT-CYCLE
               PERFORM 3120-FLAG-ITEM
               PERFORM 3130-COUNT-ITEM
               PERFORM 3140-LIMIT-AMOUNT-1
               PERFORM 3150-SET-SOURCE
               PERFORM 3160-EDIT-REF-NO
               PERFORM 3170-SELECT-PENALTY-PCT
               PERFORM 3180-SET-SOURCE
               PERFORM 3190-ACCUM-REASON
               PERFORM 3200-LIMIT-ITEM
               PERFORM 3210-COUNT-ITEM
               PERFORM 3220-ACCUM-REGION
               PERFORM 3230-COUNT-ITEM
               PERFORM 3240-EDIT-DESC
               PERFORM 3250-ACCUM-REGION
               PERFORM 3260-PRORATE-AMOUNT-1
               PERFORM 3270-CHECK-NEXT-DUE
               PERFORM 3280-SELECT-PENALTY-PCT
               PERFORM 3290-AGE-OPEN
               PERFORM 3300-ACCRUE-AMOUNT-2
               PERFORM 3310-EDIT-MEMO
               PERFORM 3320-SET-REGION
               PERFORM 3330-AGE-LAST-PAY
               PERFORM 3340-SET-PIVOT
               PERFORM 3350-AGE-POST
               PERFORM 3360-FLAG-AMOUNT-1
               PERFORM 3370-AGE-LAST-MAINT
               PERFORM 3380-AGE-LAST-MAINT
               PERFORM 3390-LIMIT-CONTROL
               PERFORM 3400-AGE-OPEN
               PERFORM 3410-SELECT-MARGIN
               PERFORM 3420-LIMIT-CONTROL
               PERFORM 3430-LIMIT-ITEM
               PERFORM 3440-PRORATE-AMOUNT-1
               PERFORM 3450-ACCUM-CYCLE
               PERFORM 3460-EDIT-MEMO
               PERFORM 3470-SET-SOURCE
               PERFORM 3480-ACCUM-CYCLE
               PERFORM 3490-ACCRUE-AMOUNT-2
               PERFORM 3500-AGE-LAST-MAINT
               PERFORM 3510-COUNT-RETRY
               PERFORM 3520-PRORATE-CONTROL
               PERFORM 3530-ACCRUE-AMOUNT-1
               PERFORM 3540-SPLIT-CONTROL
               PERFORM 3550-SPLIT-CONTROL
               PERFORM 3560-PRORATE-AMOUNT-2
               PERFORM 3570-COUNT-PAYMENT
               PERFORM 3580-LIMIT-CONTROL
               PERFORM 3590-CHECK-EXPIRY
               PERFORM 3600-AGE-POST
               PERFORM 3610-EDIT-REJECT
               PERFORM 3620-LIMIT-AMOUNT-2
               PERFORM 3630-SPLIT-CONTROL
               PERFORM 3640-FLAG-AMOUNT-2
               PERFORM 3650-SELECT-PENALTY-PCT
               PERFORM 3660-ACCRUE-AMOUNT-2
               PERFORM 3670-AGE-NEXT-DUE
               PERFORM 3680-EDIT-NAME
               PERFORM 3690-ASSESS-AUTO-RENEW
               PERFORM 3700-ACCUM-REASON
               PERFORM 3710-ASSESS-NOTICE-SENT
               PERFORM 3720-ASSESS-REVIEW
               PERFORM 3730-FLAG-AMOUNT-1
               PERFORM 3740-SET-CHANNEL
               PERFORM 3750-EDIT-REJECT
               PERFORM 3760-ASSESS-OVERRIDE
               PERFORM 3770-SELECT-DISCOUNT
               PERFORM 3780-SELECT-DISCOUNT
               PERFORM 3790-COUNT-SEQ
               PERFORM 3800-PRORATE-AMOUNT-2
               PERFORM 3810-CHECK-EFF
               PERFORM 3820-DESCRIBE-WINDOW-RULE
               PERFORM 3830-PRORATE-AMOUNT-2
               PERFORM 3840-FLAG-AMOUNT-1
               PERFORM 3850-CHECK-EFF
               PERFORM 3860-SET-WINDOW-RULE
               PERFORM 3870-AGE-LAST-STMT
               PERFORM 3880-SELECT-DISCOUNT
               PERFORM 3890-COUNT-SEQ
               PERFORM 3900-DESCRIBE-TRAN
               PERFORM 3910-COUNT-ITEM
               PERFORM 3920-SELECT-PENALTY-PCT
               PERFORM 3930-ACCUM-CHANNEL
               PERFORM 3940-ACCRUE-AMOUNT-1
               PERFORM 3950-DESCRIBE-CYCLE
               PERFORM 3960-AGE-EXPIRY
               PERFORM 3970-EDIT-ADDR
               PERFORM 3980-LIMIT-AMOUNT-2
               PERFORM 3990-DESCRIBE-CHANNEL
               PERFORM 4000-DESCRIBE-REASON
               PERFORM 4010-SELECT-MARGIN
               PERFORM 4020-ACCUM-REGION
               PERFORM 4030-ACCUM-CYCLE
               PERFORM 4040-SET-TRAN
               PERFORM 4050-SELECT-MARGIN
               PERFORM 4060-SET-REGION
               PERFORM 4070-EDIT-REF-NO
               PERFORM 4080-COUNT-CYCLE
               PERFORM 4090-SELECT-MARGIN
               PERFORM 4100-ACCUM-SOURCE
               PERFORM 4110-SET-PIVOT
               PERFORM 4120-EDIT-REJECT
               PERFORM 4130-SPLIT-AMOUNT-1
               PERFORM 4140-AGE-POST
               PERFORM 4150-EDIT-MEMO
               PERFORM 4160-FLAG-ITEM
               PERFORM 4170-SELECT-MARGIN
               PERFORM 4180-ACCRUE-AMOUNT-1
               PERFORM 4190-PRORATE-AMOUNT-2
               PERFORM 4200-SPLIT-CONTROL
               PERFORM 4210-SELECT-PENALTY-PCT
               PERFORM 4220-ACCUM-PIVOT
               PERFORM 4230-FLAG-AMOUNT-2
               PERFORM 4240-ASSESS-EXEMPT
               PERFORM 4250-ACCRUE-AMOUNT-1
               PERFORM 4260-COUNT-ITEM
               PERFORM 4270-ACCUM-SOURCE
               PERFORM 4280-FLAG-CONTROL
               PERFORM 4290-CHECK-POST
               PERFORM 7040-CALL-YKLIM
               PERFORM 7000-CALL-YKPRC
               PERFORM 7010-CALL-YKTIER
               REWRITE YDB-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'YDBMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM YDB-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * ASSESS THE AMOUNT 1 YTD FEE ON THE CONTROL AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3000-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN YDBM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO YDBM-AMOUNT-1-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YDBT-REVERSAL-FLAG = 'Y'
                   COMPUTE YDBM-AMOUNT-1-YTD ROUNDED = YDBT-CONTROL-AMT
                       * YDBM-MARGIN / 200
               WHEN YDBM-WINDOW-RULE-CD = 'A'
                   MOVE 10.00 TO YDBM-AMOUNT-1-YTD
               WHEN YDBM-WINDOW-RULE-CD = 'D'
                   MOVE 15.00 TO YDBM-AMOUNT-1-YTD
               WHEN OTHER
                   COMPUTE YDBM-AMOUNT-1-YTD ROUNDED = YDBT-CONTROL-AMT
                       * YDBM-MARGIN / 100
           END-EVALUATE
           IF YDBM-AMOUNT-1-YTD > 0 AND YDBM-AMOUNT-1-YTD < 5.00
               MOVE 5.00 TO YDBM-AMOUNT-1-YTD
           END-IF
           IF YDBM-AMOUNT-1-YTD > 35.00
               MOVE 35.00 TO YDBM-AMOUNT-1-YTD
           END-IF
           ADD YDBM-AMOUNT-1-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3010-ACCRUE-DISCOUNT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * YDBM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO YDBM-AMOUNT-1-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON AMOUNT 2 YTD AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3020-ACCRUE-AMOUNT-2.
           IF YDBM-AMOUNT-2-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   YDBM-AMOUNT-2-YTD * YDBM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO YDBT-AMOUNT-1-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3030-DESCRIBE-CHANNEL.
           EVALUATE YDBT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3040-ACCRUE-DISCOUNT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * YDBM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO YDBM-AMOUNT-1-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CONTROL YTD.
      *----------------------------------------------------------------*
       3050-PRORATE-CONTROL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE YDBM-CONTROL-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO YDBM-CONTROL-YTD
           END-IF
           ADD YDBM-CONTROL-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3060-LIMIT-ITEM.
           IF WS-WORK-AMT < 0.00
               MOVE 0.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 5000.00
               MOVE 5000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CONTROL YTD FEE ON THE AMOUNT 1 AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3070-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN YDBT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO YDBM-CONTROL-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YDBM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE YDBM-CONTROL-YTD ROUNDED = YDBM-AMOUNT-1-AMT
                       * YDBM-PENALTY-PCT / 200
               WHEN YDBM-SOURCE-CD = 'BNX'
                   MOVE 3.00 TO YDBM-CONTROL-YTD
               WHEN YDBM-SOURCE-CD = 'FTB'
                   MOVE 20.00 TO YDBM-CONTROL-YTD
               WHEN OTHER
                   COMPUTE YDBM-CONTROL-YTD ROUNDED = YDBM-AMOUNT-1-AMT
                       * YDBM-PENALTY-PCT / 100
           END-EVALUATE
           IF YDBM-CONTROL-YTD > 0 AND YDBM-CONTROL-YTD < 10.00
               MOVE 10.00 TO YDBM-CONTROL-YTD
           END-IF
           IF YDBM-CONTROL-YTD > 250.00
               MOVE 250.00 TO YDBM-CONTROL-YTD
           END-IF
           ADD YDBM-CONTROL-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3080-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YDBM-REGION-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YDBM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT AMOUNT 1 AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3090-SPLIT-AMOUNT-1.
           COMPUTE WS-WORK-AMT = YDBM-AMOUNT-1-AMT / 12
           COMPUTE WS-WORK-AMT-2 = YDBM-AMOUNT-1-AMT - (WS-WORK-AMT *
               12)
           COMPUTE YDBM-AMOUNT-2-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT 1 AMT IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3100-LIMIT-AMOUNT-1.
           IF YDBT-AMOUNT-1-AMT < 10.00
               MOVE 10.00 TO YDBT-AMOUNT-1-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YDBT-AMOUNT-1-AMT > 500.00
               MOVE 500.00 TO YDBT-AMOUNT-1-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3110-COUNT-CYCLE.
           IF YDBM-CYCLE-CNT < 9999999
               ADD 1 TO YDBM-CYCLE-CNT
           END-IF
           IF YDBM-CYCLE-CNT NOT < 12
               MOVE 'Y' TO YDBM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3120-FLAG-ITEM.
           IF WS-WORK-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3130-COUNT-ITEM.
           IF YDBM-ITEM-CNT < 9999999
               ADD 1 TO YDBM-ITEM-CNT
           END-IF
           IF YDBM-ITEM-CNT NOT < 10
               MOVE 'Y' TO YDBM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT 1 AMT IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3140-LIMIT-AMOUNT-1.
           IF YDBM-AMOUNT-1-AMT < 0.00
               MOVE 0.00 TO YDBM-AMOUNT-1-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YDBM-AMOUNT-1-AMT > 10000.00
               MOVE 10000.00 TO YDBM-AMOUNT-1-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * BNX -> FTB WHEN LAST MAINT DATE IS PAST
      * FTB -> GAP WHEN LAST MAINT DATE IS PAST
      * GAP -> PAP WHEN LAST MAINT DATE IS PAST
      * PAP -> VGC WHEN LAST MAINT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3150-SET-SOURCE.
           EVALUATE YDBM-SOURCE-CD
               WHEN 'BNX'
                   IF YDBM-LAST-MAINT-DATE > 0 AND YDBM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'FTB' TO YDBM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FTB'
                   IF YDBM-LAST-MAINT-DATE > 0 AND YDBM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'GAP' TO YDBM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GAP'
                   IF YDBM-LAST-MAINT-DATE > 0 AND YDBM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'PAP' TO YDBM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PAP'
                   IF YDBM-LAST-MAINT-DATE > 0 AND YDBM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'VGC' TO YDBM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E017 REF NO REQUIRED
      * E415 AMOUNT 2 YTD NEGATIVE
      * E783 AMOUNT 2 YTD OVER 10000
      * E542 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3160-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YDBM-REF-NO = SPACES
                   MOVE 'E017' TO WS-REJECT-CODE
               WHEN YDBM-AMOUNT-2-YTD < 0
                   MOVE 'E415' TO WS-REJECT-CODE
               WHEN YDBM-AMOUNT-2-YTD > 10000.00
                   MOVE 'E783' TO WS-REJECT-CODE
               WHEN YDBT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E542' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CONTROL TIER TIER FOR THE CONTROL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3170-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CONTROL-TIER-RATE(1) TO YDBM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF YDBT-CONTROL-AMT NOT < WS-CONTROL-TIER-LOW(WS-TX)
                   MOVE WS-CONTROL-TIER-RATE(WS-TX) TO YDBM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF YDBM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO YDBM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * BNX -> FTB WHEN NOTICE DATE IS PAST
      * FTB -> GAP WHEN NOTICE DATE IS PAST
      * GAP -> PAP WHEN NOTICE DATE IS PAST
      * PAP -> VGC WHEN NOTICE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3180-SET-SOURCE.
           EVALUATE YDBM-SOURCE-CD
               WHEN 'BNX'
                   IF YDBM-NOTICE-DATE > 0 AND YDBM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FTB' TO YDBM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FTB'
                   IF YDBM-NOTICE-DATE > 0 AND YDBM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GAP' TO YDBM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GAP'
                   IF YDBM-NOTICE-DATE > 0 AND YDBM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PAP' TO YDBM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PAP'
                   IF YDBM-NOTICE-DATE > 0 AND YDBM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VGC' TO YDBM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3190-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YDBM-REASON-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YDBM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3200-LIMIT-ITEM.
           IF WS-WORK-AMT < 50.00
               MOVE 50.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 10000.00
               MOVE 10000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3210-COUNT-ITEM.
           IF YDBM-ITEM-CNT < 9999999
               ADD 1 TO YDBM-ITEM-CNT
           END-IF
           IF YDBM-ITEM-CNT NOT < 6
               MOVE 'Y' TO YDBM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE AMOUNT 2 AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3220-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YDBM-REGION-CD
                   ADD YDBM-AMOUNT-2-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YDBM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE YDBM-AMOUNT-2-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3230-COUNT-ITEM.
           IF YDBM-ITEM-CNT < 9999999
               ADD 1 TO YDBM-ITEM-CNT
           END-IF
           IF YDBM-ITEM-CNT NOT < 12
               MOVE 'Y' TO YDBM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E969 DESC REQUIRED
      * E590 AMOUNT 1 AMT NEGATIVE
      * E754 AMOUNT 1 AMT OVER 10000
      * E291 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3240-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YDBM-DESC = SPACES
                   MOVE 'E969' TO WS-REJECT-CODE
               WHEN YDBM-AMOUNT-1-AMT < 0
                   MOVE 'E590' TO WS-REJECT-CODE
               WHEN YDBM-AMOUNT-1-AMT > 10000.00
                   MOVE 'E754' TO WS-REJECT-CODE
               WHEN YDBT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E291' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3250-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YDBM-REGION-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YDBM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO AMOUNT 1 AMT.
      *----------------------------------------------------------------*
       3260-PRORATE-AMOUNT-1.
           IF WS-PERIOD-DAYS > 0
               COMPUTE YDBT-AMOUNT-1-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO YDBT-AMOUNT-1-AMT
           END-IF
           ADD YDBT-AMOUNT-1-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3270-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF YDBM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YDBM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO YDBT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CONTROL TIER TIER FOR THE CONTROL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3280-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CONTROL-TIER-RATE(1) TO YDBM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF YDBT-CONTROL-AMT NOT < WS-CONTROL-TIER-LOW(WS-TX)
                   MOVE WS-CONTROL-TIER-RATE(WS-TX) TO YDBM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF YDBM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO YDBM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3290-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF YDBM-OPEN-DATE > 0 AND YDBM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YDBM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD YDBM-AMOUNT-2-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD YDBM-AMOUNT-2-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD YDBM-AMOUNT-2-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD YDBM-AMOUNT-2-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD YDBM-AMOUNT-2-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON AMOUNT 2 AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3300-ACCRUE-AMOUNT-2.
           IF YDBT-AMOUNT-2-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   YDBT-AMOUNT-2-AMT * YDBM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO YDBM-CONTROL-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E475 MEMO REQUIRED
      * E284 CONTROL AMT NEGATIVE
      * E091 CONTROL AMT OVER 250000
      * E536 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3310-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YDBT-MEMO = SPACES
                   MOVE 'E475' TO WS-REJECT-CODE
               WHEN YDBM-CONTROL-AMT < 0
                   MOVE 'E284' TO WS-REJECT-CODE
               WHEN YDBM-CONTROL-AMT > 250000.00
                   MOVE 'E091' TO WS-REJECT-CODE
               WHEN YDBM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E536' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * C -> H WHEN EXPIRY DATE IS PAST
      * H -> K WHEN EXPIRY DATE IS PAST
      * K -> S WHEN EXPIRY DATE IS PAST
      * S -> V WHEN EXPIRY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3320-SET-REGION.
           EVALUATE YDBM-REGION-CD
               WHEN 'C'
                   IF YDBM-EXPIRY-DATE > 0 AND YDBM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO YDBM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF YDBM-EXPIRY-DATE > 0 AND YDBM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'K' TO YDBM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF YDBM-EXPIRY-DATE > 0 AND YDBM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO YDBM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF YDBM-EXPIRY-DATE > 0 AND YDBM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'V' TO YDBM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-120, 121-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3330-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF YDBM-LAST-PAY-DATE > 0 AND YDBM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YDBM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD YDBM-AMOUNT-1-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 120
                   ADD YDBM-AMOUNT-1-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD YDBM-AMOUNT-1-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD YDBM-AMOUNT-1-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD YDBM-AMOUNT-1-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PIVOT CD:
      * KGT -> NNG WHEN LAST STMT DATE IS PAST
      * NNG -> PTC WHEN LAST STMT DATE IS PAST
      * PTC -> WZW WHEN LAST STMT DATE IS PAST
      * WZW -> KGT WHEN LAST STMT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3340-SET-PIVOT.
           EVALUATE YDBM-PIVOT-CD
               WHEN 'KGT'
                   IF YDBM-LAST-STMT-DATE > 0 AND YDBM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NNG' TO YDBM-PIVOT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NNG'
                   IF YDBM-LAST-STMT-DATE > 0 AND YDBM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PTC' TO YDBM-PIVOT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PTC'
                   IF YDBM-LAST-STMT-DATE > 0 AND YDBM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WZW' TO YDBM-PIVOT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WZW'
                   IF YDBM-LAST-STMT-DATE > 0 AND YDBM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KGT' TO YDBM-PIVOT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3350-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF YDBT-POST-DATE > 0 AND YDBT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YDBT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD YDBT-AMOUNT-2-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD YDBT-AMOUNT-2-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD YDBT-AMOUNT-2-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD YDBT-AMOUNT-2-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD YDBT-AMOUNT-2-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AMOUNT 1 AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3360-FLAG-AMOUNT-1.
           IF YDBM-AMOUNT-1-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE YDBM-AMOUNT-1-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3370-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF YDBM-LAST-MAINT-DATE > 0 AND YDBM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YDBM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD YDBM-CONTROL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD YDBM-CONTROL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD YDBM-CONTROL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD YDBM-CONTROL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD YDBM-CONTROL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3380-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF YDBM-LAST-MAINT-DATE > 0 AND YDBM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YDBM-LAST-MAINT-DATE TO DTU-DATE-1
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
      * CONTROL AMT IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3390-LIMIT-CONTROL.
           IF YDBM-CONTROL-AMT < 25.00
               MOVE 25.00 TO YDBM-CONTROL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YDBM-CONTROL-AMT > 2500.00
               MOVE 2500.00 TO YDBM-CONTROL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3400-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF YDBM-OPEN-DATE > 0 AND YDBM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YDBM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD YDBM-AMOUNT-2-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD YDBM-AMOUNT-2-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD YDBM-AMOUNT-2-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD YDBM-AMOUNT-2-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD YDBM-AMOUNT-2-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE CONTROL TIERX TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3410-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-CONTROL-TIERX-RATE(1) TO YDBM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF WS-WORK-AMT NOT < WS-CONTROL-TIERX-LOW(WS-TX)
                   MOVE WS-CONTROL-TIERX-RATE(WS-TX) TO YDBM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF YDBM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO YDBM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CONTROL AMT IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3420-LIMIT-CONTROL.
           IF YDBM-CONTROL-AMT < 50.00
               MOVE 50.00 TO YDBM-CONTROL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YDBM-CONTROL-AMT > 1000.00
               MOVE 1000.00 TO YDBM-CONTROL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3430-LIMIT-ITEM.
           IF WS-WORK-AMT < 100.00
               MOVE 100.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 5000.00
               MOVE 5000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO AMOUNT 1 YTD.
      *----------------------------------------------------------------*
       3440-PRORATE-AMOUNT-1.
           IF WS-PERIOD-DAYS > 0
               COMPUTE YDBM-AMOUNT-1-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO YDBM-AMOUNT-1-YTD
           END-IF
           ADD YDBM-AMOUNT-1-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3450-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YDBM-CYCLE-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YDBM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E437 MEMO REQUIRED
      * E947 CONTROL AMT NEGATIVE
      * E193 CONTROL AMT OVER 50000
      * E695 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3460-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YDBT-MEMO = SPACES
                   MOVE 'E437' TO WS-REJECT-CODE
               WHEN YDBM-CONTROL-AMT < 0
                   MOVE 'E947' TO WS-REJECT-CODE
               WHEN YDBM-CONTROL-AMT > 50000.00
                   MOVE 'E193' TO WS-REJECT-CODE
               WHEN YDBM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E695' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * BNX -> FTB WHEN LAST MAINT DATE IS PAST
      * FTB -> GAP WHEN LAST MAINT DATE IS PAST
      * GAP -> PAP WHEN LAST MAINT DATE IS PAST
      * PAP -> VGC WHEN LAST MAINT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3470-SET-SOURCE.
           EVALUATE YDBM-SOURCE-CD
               WHEN 'BNX'
                   IF YDBM-LAST-MAINT-DATE > 0 AND YDBM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'FTB' TO YDBM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FTB'
                   IF YDBM-LAST-MAINT-DATE > 0 AND YDBM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'GAP' TO YDBM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GAP'
                   IF YDBM-LAST-MAINT-DATE > 0 AND YDBM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'PAP' TO YDBM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PAP'
                   IF YDBM-LAST-MAINT-DATE > 0 AND YDBM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'VGC' TO YDBM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE AMOUNT 2 AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3480-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YDBM-CYCLE-CD
                   ADD YDBT-AMOUNT-2-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YDBM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE YDBT-AMOUNT-2-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON AMOUNT 2 YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3490-ACCRUE-AMOUNT-2.
           IF YDBM-AMOUNT-2-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   YDBM-AMOUNT-2-YTD * YDBM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3500-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF YDBM-LAST-MAINT-DATE > 0 AND YDBM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YDBM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD YDBM-AMOUNT-1-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD YDBM-AMOUNT-1-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD YDBM-AMOUNT-1-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD YDBM-AMOUNT-1-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD YDBM-AMOUNT-1-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3510-COUNT-RETRY.
           IF YDBM-RETRY-CNT < 9999999
               ADD 1 TO YDBM-RETRY-CNT
           END-IF
           IF YDBM-RETRY-CNT NOT < 6
               MOVE 'Y' TO YDBT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE AMOUNT 1 AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CONTROL AMT.
      *----------------------------------------------------------------*
       3520-PRORATE-CONTROL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE YDBM-CONTROL-AMT ROUNDED =
                   YDBM-AMOUNT-1-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO YDBM-CONTROL-AMT
           END-IF
           ADD YDBM-CONTROL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON AMOUNT 1 AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3530-ACCRUE-AMOUNT-1.
           IF YDBT-AMOUNT-1-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   YDBT-AMOUNT-1-AMT * YDBM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO YDBM-AMOUNT-2-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CONTROL AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3540-SPLIT-CONTROL.
           COMPUTE WS-WORK-AMT = YDBM-CONTROL-AMT / 4
           COMPUTE WS-WORK-AMT-2 = YDBM-CONTROL-AMT - (WS-WORK-AMT * 4)
           COMPUTE YDBT-AMOUNT-2-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CONTROL AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3550-SPLIT-CONTROL.
           COMPUTE WS-WORK-AMT = YDBT-CONTROL-AMT / 2
           COMPUTE WS-WORK-AMT-2 = YDBT-CONTROL-AMT - (WS-WORK-AMT * 2)
           COMPUTE YDBM-CONTROL-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO AMOUNT 2 AMT.
      *----------------------------------------------------------------*
       3560-PRORATE-AMOUNT-2.
           IF WS-PERIOD-DAYS > 0
               COMPUTE YDBM-AMOUNT-2-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO YDBM-AMOUNT-2-AMT
           END-IF
           ADD YDBM-AMOUNT-2-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3570-COUNT-PAYMENT.
           IF YDBM-PAYMENT-CNT < 9999999
               ADD 1 TO YDBM-PAYMENT-CNT
           END-IF
           IF YDBM-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO YDBM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CONTROL AMT IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3580-LIMIT-CONTROL.
           IF YDBT-CONTROL-AMT < 100.00
               MOVE 100.00 TO YDBT-CONTROL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YDBT-CONTROL-AMT > 10000.00
               MOVE 10000.00 TO YDBT-CONTROL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3590-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF YDBM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YDBM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO YDBM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3600-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF YDBT-POST-DATE > 0 AND YDBT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YDBT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD YDBM-CONTROL-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD YDBM-CONTROL-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD YDBM-CONTROL-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD YDBM-CONTROL-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD YDBM-CONTROL-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E470 REJECT CD REQUIRED
      * E461 CONTROL YTD NEGATIVE
      * E174 CONTROL YTD OVER 25000
      * E482 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3610-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YDBT-REJECT-CD = SPACES
                   MOVE 'E470' TO WS-REJECT-CODE
               WHEN YDBM-CONTROL-YTD < 0
                   MOVE 'E461' TO WS-REJECT-CODE
               WHEN YDBM-CONTROL-YTD > 25000.00
                   MOVE 'E174' TO WS-REJECT-CODE
               WHEN YDBM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E482' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT 2 AMT IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3620-LIMIT-AMOUNT-2.
           IF YDBT-AMOUNT-2-AMT < 10.00
               MOVE 10.00 TO YDBT-AMOUNT-2-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YDBT-AMOUNT-2-AMT > 5000.00
               MOVE 5000.00 TO YDBT-AMOUNT-2-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CONTROL AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3630-SPLIT-CONTROL.
           COMPUTE WS-WORK-AMT = YDBM-CONTROL-AMT / 2
           COMPUTE WS-WORK-AMT-2 = YDBM-CONTROL-AMT - (WS-WORK-AMT * 2)
           COMPUTE YDBT-AMOUNT-2-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT 2 AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3640-FLAG-AMOUNT-2.
           IF YDBM-AMOUNT-2-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE YDBM-AMOUNT-2-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CONTROL TIERX TIER FOR THE AMOUNT 2 AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3650-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CONTROL-TIERX-RATE(1) TO YDBM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF YDBM-AMOUNT-2-AMT NOT < WS-CONTROL-TIERX-LOW(WS-TX)
                   MOVE WS-CONTROL-TIERX-RATE(WS-TX) TO YDBM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF YDBM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO YDBM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON AMOUNT 2 YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3660-ACCRUE-AMOUNT-2.
           IF YDBM-AMOUNT-2-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   YDBM-AMOUNT-2-YTD * YDBM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO YDBM-CONTROL-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3670-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF YDBM-NEXT-DUE-DATE > 0 AND YDBM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YDBM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD YDBM-AMOUNT-2-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD YDBM-AMOUNT-2-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD YDBM-AMOUNT-2-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD YDBM-AMOUNT-2-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD YDBM-AMOUNT-2-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E707 NAME REQUIRED
      * E482 WORK AMT NEGATIVE
      * E746 WORK AMT OVER 99999
      * E445 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3680-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YDBM-NAME = SPACES
                   MOVE 'E707' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E482' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 99999.00
                   MOVE 'E746' TO WS-REJECT-CODE
               WHEN YDBM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E445' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CONTROL YTD FEE ON THE AMOUNT 2 AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3690-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN YDBM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO YDBM-CONTROL-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YDBM-PREFERRED-FLAG = 'Y'
                   COMPUTE YDBM-CONTROL-YTD ROUNDED = YDBT-AMOUNT-2-AMT
                       * YDBM-MARGIN / 200
               WHEN YDBM-REGION-CD = 'C'
                   MOVE 10.00 TO YDBM-CONTROL-YTD
               WHEN YDBM-REGION-CD = 'H'
                   MOVE 7.00 TO YDBM-CONTROL-YTD
               WHEN OTHER
                   COMPUTE YDBM-CONTROL-YTD ROUNDED = YDBT-AMOUNT-2-AMT
                       * YDBM-MARGIN / 100
           END-EVALUATE
           IF YDBM-CONTROL-YTD > 0 AND YDBM-CONTROL-YTD < 10.00
               MOVE 10.00 TO YDBM-CONTROL-YTD
           END-IF
           IF YDBM-CONTROL-YTD > 35.00
               MOVE 35.00 TO YDBM-CONTROL-YTD
           END-IF
           ADD YDBM-CONTROL-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3700-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YDBM-REASON-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YDBM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE AMOUNT 1 FEE ON THE AMOUNT 2 AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3710-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN YDBM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO YDBM-AMOUNT-1-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YDBM-OVERRIDE-FLAG = 'Y'
                   COMPUTE YDBM-AMOUNT-1-AMT ROUNDED = YDBM-AMOUNT-2-AMT
                       * YDBM-DISCOUNT / 200
               WHEN YDBT-CHANNEL-CD = 'BRN'
                   MOVE 5.00 TO YDBM-AMOUNT-1-AMT
               WHEN YDBT-CHANNEL-CD = 'BAT'
                   MOVE 7.00 TO YDBM-AMOUNT-1-AMT
               WHEN OTHER
                   COMPUTE YDBM-AMOUNT-1-AMT ROUNDED = YDBM-AMOUNT-2-AMT
                       * YDBM-DISCOUNT / 100
           END-EVALUATE
           IF YDBM-AMOUNT-1-AMT > 0 AND YDBM-AMOUNT-1-AMT < 10.00
               MOVE 10.00 TO YDBM-AMOUNT-1-AMT
           END-IF
           IF YDBM-AMOUNT-1-AMT > 50.00
               MOVE 50.00 TO YDBM-AMOUNT-1-AMT
           END-IF
           ADD YDBM-AMOUNT-1-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE AMOUNT 1 FEE ON THE CONTROL AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3720-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN YDBM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO YDBT-AMOUNT-1-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YDBT-REVERSAL-FLAG = 'Y'
                   COMPUTE YDBT-AMOUNT-1-AMT ROUNDED = YDBT-CONTROL-AMT
                       * YDBM-DISCOUNT / 200
               WHEN YDBM-REGION-CD = 'C'
                   MOVE 15.00 TO YDBT-AMOUNT-1-AMT
               WHEN YDBM-REGION-CD = 'H'
                   MOVE 5.00 TO YDBT-AMOUNT-1-AMT
               WHEN OTHER
                   COMPUTE YDBT-AMOUNT-1-AMT ROUNDED = YDBT-CONTROL-AMT
                       * YDBM-DISCOUNT / 100
           END-EVALUATE
           IF YDBT-AMOUNT-1-AMT > 0 AND YDBT-AMOUNT-1-AMT < 10.00
               MOVE 10.00 TO YDBT-AMOUNT-1-AMT
           END-IF
           IF YDBT-AMOUNT-1-AMT > 25.00
               MOVE 25.00 TO YDBT-AMOUNT-1-AMT
           END-IF
           ADD YDBT-AMOUNT-1-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AMOUNT 1 AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3730-FLAG-AMOUNT-1.
           IF YDBT-AMOUNT-1-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE YDBT-AMOUNT-1-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN EXPIRY DATE IS PAST
      * BAT -> MAL WHEN EXPIRY DATE IS PAST
      * MAL -> TEL WHEN EXPIRY DATE IS PAST
      * TEL -> ONL WHEN EXPIRY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3740-SET-CHANNEL.
           EVALUATE YDBT-CHANNEL-CD
               WHEN 'BRN'
                   IF YDBM-EXPIRY-DATE > 0 AND YDBM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO YDBT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF YDBM-EXPIRY-DATE > 0 AND YDBM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO YDBT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF YDBM-EXPIRY-DATE > 0 AND YDBM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO YDBT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF YDBM-EXPIRY-DATE > 0 AND YDBM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO YDBT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E775 REJECT CD REQUIRED
      * E171 CONTROL AMT NEGATIVE
      * E508 CONTROL AMT OVER 1000000
      * E564 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3750-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YDBT-REJECT-CD = SPACES
                   MOVE 'E775' TO WS-REJECT-CODE
               WHEN YDBM-CONTROL-AMT < 0
                   MOVE 'E171' TO WS-REJECT-CODE
               WHEN YDBM-CONTROL-AMT > 1000000.00
                   MOVE 'E508' TO WS-REJECT-CODE
               WHEN YDBM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E564' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CONTROL FEE ON THE AMOUNT 2 YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3760-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN YDBM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO YDBT-CONTROL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YDBM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE YDBT-CONTROL-AMT ROUNDED = YDBM-AMOUNT-2-YTD
                       * YDBM-MARGIN / 200
               WHEN YDBM-CYCLE-CD = 'FJ'
                   MOVE 10.00 TO YDBT-CONTROL-AMT
               WHEN YDBM-CYCLE-CD = 'LA'
                   MOVE 10.00 TO YDBT-CONTROL-AMT
               WHEN OTHER
                   COMPUTE YDBT-CONTROL-AMT ROUNDED = YDBM-AMOUNT-2-YTD
                       * YDBM-MARGIN / 100
           END-EVALUATE
           IF YDBT-CONTROL-AMT > 0 AND YDBT-CONTROL-AMT < 10.00
               MOVE 10.00 TO YDBT-CONTROL-AMT
           END-IF
           IF YDBT-CONTROL-AMT > 150.00
               MOVE 150.00 TO YDBT-CONTROL-AMT
           END-IF
           ADD YDBT-CONTROL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE CONTROL TIER TIER FOR THE CONTROL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3770-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CONTROL-TIER-RATE(1) TO YDBM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF YDBM-CONTROL-AMT NOT < WS-CONTROL-TIER-LOW(WS-TX)
                   MOVE WS-CONTROL-TIER-RATE(WS-TX) TO YDBM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF YDBM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO YDBM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CONTROL TIERX TIER FOR THE AMOUNT 1 AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3780-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CONTROL-TIERX-RATE(1) TO YDBM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF YDBM-AMOUNT-1-AMT NOT < WS-CONTROL-TIERX-LOW(WS-TX)
                   MOVE WS-CONTROL-TIERX-RATE(WS-TX) TO YDBM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF YDBM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO YDBM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3790-COUNT-SEQ.
           IF YDBT-SEQ < 9999999
               ADD 1 TO YDBT-SEQ
           END-IF
           IF YDBT-SEQ NOT < 10
               MOVE 'Y' TO YDBM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO AMOUNT 2 AMT.
      *----------------------------------------------------------------*
       3800-PRORATE-AMOUNT-2.
           IF WS-PERIOD-DAYS > 0
               COMPUTE YDBM-AMOUNT-2-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO YDBM-AMOUNT-2-AMT
           END-IF
           ADD YDBM-AMOUNT-2-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 180 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3810-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF YDBT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YDBT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO YDBM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE WINDOW RULE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3820-DESCRIBE-WINDOW-RULE.
           EVALUATE YDBM-WINDOW-RULE-CD
               WHEN 'A'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'D'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'S'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'T'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE AMOUNT 2 AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO AMOUNT 2 YTD.
      *----------------------------------------------------------------*
       3830-PRORATE-AMOUNT-2.
           IF WS-PERIOD-DAYS > 0
               COMPUTE YDBM-AMOUNT-2-YTD ROUNDED =
                   YDBM-AMOUNT-2-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO YDBM-AMOUNT-2-YTD
           END-IF
           ADD YDBM-AMOUNT-2-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AMOUNT 1 AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3840-FLAG-AMOUNT-1.
           IF YDBT-AMOUNT-1-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE YDBT-AMOUNT-1-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 180 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3850-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF YDBT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YDBT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO YDBM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR WINDOW RULE CD:
      * A -> D WHEN POST DATE IS PAST
      * D -> S WHEN POST DATE IS PAST
      * S -> T WHEN POST DATE IS PAST
      * T -> A WHEN POST DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3860-SET-WINDOW-RULE.
           EVALUATE YDBM-WINDOW-RULE-CD
               WHEN 'A'
                   IF YDBT-POST-DATE > 0 AND YDBT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO YDBM-WINDOW-RULE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF YDBT-POST-DATE > 0 AND YDBT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO YDBM-WINDOW-RULE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF YDBT-POST-DATE > 0 AND YDBT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO YDBM-WINDOW-RULE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF YDBT-POST-DATE > 0 AND YDBT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'A' TO YDBM-WINDOW-RULE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3870-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF YDBM-LAST-STMT-DATE > 0 AND YDBM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YDBM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD YDBM-CONTROL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD YDBM-CONTROL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD YDBM-CONTROL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD YDBM-CONTROL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD YDBM-CONTROL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE CONTROL TIER TIER FOR THE CONTROL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3880-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CONTROL-TIER-RATE(1) TO YDBM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF YDBM-CONTROL-AMT NOT < WS-CONTROL-TIER-LOW(WS-TX)
                   MOVE WS-CONTROL-TIER-RATE(WS-TX) TO YDBM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF YDBM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO YDBM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3890-COUNT-SEQ.
           IF YDBT-SEQ < 9999999
               ADD 1 TO YDBT-SEQ
           END-IF
           IF YDBT-SEQ NOT < 3
               MOVE 'Y' TO YDBM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3900-DESCRIBE-TRAN.
           EVALUATE YDBT-TRAN-CD
               WHEN 'CV'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'RV'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'TS'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3910-COUNT-ITEM.
           IF YDBM-ITEM-CNT < 9999999
               ADD 1 TO YDBM-ITEM-CNT
           END-IF
           IF YDBM-ITEM-CNT NOT < 3
               MOVE 'Y' TO YDBM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CONTROL TIERX TIER FOR THE CONTROL YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3920-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CONTROL-TIERX-RATE(1) TO YDBM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF YDBM-CONTROL-YTD NOT < WS-CONTROL-TIERX-LOW(WS-TX)
                   MOVE WS-CONTROL-TIERX-RATE(WS-TX) TO YDBM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF YDBM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO YDBM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE AMOUNT 2 AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3930-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YDBT-CHANNEL-CD
                   ADD YDBT-AMOUNT-2-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YDBT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE YDBT-AMOUNT-2-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON AMOUNT 1 AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3940-ACCRUE-AMOUNT-1.
           IF YDBT-AMOUNT-1-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   YDBT-AMOUNT-1-AMT * YDBM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO YDBM-AMOUNT-1-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3950-DESCRIBE-CYCLE.
           EVALUATE YDBM-CYCLE-CD
               WHEN 'FJ'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'LA'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'SP'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'UC'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3960-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF YDBM-EXPIRY-DATE > 0 AND YDBM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YDBM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD YDBM-AMOUNT-1-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD YDBM-AMOUNT-1-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD YDBM-AMOUNT-1-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD YDBM-AMOUNT-1-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD YDBM-AMOUNT-1-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E609 ADDR REQUIRED
      * E070 WORK AMT 2 NEGATIVE
      * E152 WORK AMT 2 OVER 50000
      * E296 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3970-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YDBM-ADDR = SPACES
                   MOVE 'E609' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E070' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 50000.00
                   MOVE 'E152' TO WS-REJECT-CODE
               WHEN YDBM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E296' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT 2 YTD IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3980-LIMIT-AMOUNT-2.
           IF YDBM-AMOUNT-2-YTD < 0.00
               MOVE 0.00 TO YDBM-AMOUNT-2-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YDBM-AMOUNT-2-YTD > 500.00
               MOVE 500.00 TO YDBM-AMOUNT-2-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3990-DESCRIBE-CHANNEL.
           EVALUATE YDBT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4000-DESCRIBE-REASON.
           EVALUATE YDBM-REASON-CD
               WHEN 'AX'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'LK'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'NT'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'UW'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE CONTROL TIER TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4010-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-CONTROL-TIER-RATE(1) TO YDBM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF WS-WORK-AMT-2 NOT < WS-CONTROL-TIER-LOW(WS-TX)
                   MOVE WS-CONTROL-TIER-RATE(WS-TX) TO YDBM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF YDBM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO YDBM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CONTROL YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4020-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YDBM-REGION-CD
                   ADD YDBM-CONTROL-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YDBM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE YDBM-CONTROL-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CONTROL YTD BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4030-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YDBM-CYCLE-CD
                   ADD YDBM-CONTROL-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YDBM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE YDBM-CONTROL-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * CV -> RV WHEN NOTICE DATE IS PAST
      * RV -> TS WHEN NOTICE DATE IS PAST
      * TS -> CV WHEN NOTICE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4040-SET-TRAN.
           EVALUATE YDBT-TRAN-CD
               WHEN 'CV'
                   IF YDBM-NOTICE-DATE > 0 AND YDBM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RV' TO YDBT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RV'
                   IF YDBM-NOTICE-DATE > 0 AND YDBM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TS' TO YDBT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TS'
                   IF YDBM-NOTICE-DATE > 0 AND YDBM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CV' TO YDBT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE CONTROL TIER TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4050-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-CONTROL-TIER-RATE(1) TO YDBM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF WS-WORK-AMT NOT < WS-CONTROL-TIER-LOW(WS-TX)
                   MOVE WS-CONTROL-TIER-RATE(WS-TX) TO YDBM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF YDBM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO YDBM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * C -> H WHEN EFF DATE IS PAST
      * H -> K WHEN EFF DATE IS PAST
      * K -> S WHEN EFF DATE IS PAST
      * S -> V WHEN EFF DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4060-SET-REGION.
           EVALUATE YDBM-REGION-CD
               WHEN 'C'
                   IF YDBT-EFF-DATE > 0 AND YDBT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO YDBM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF YDBT-EFF-DATE > 0 AND YDBT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'K' TO YDBM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF YDBT-EFF-DATE > 0 AND YDBT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO YDBM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF YDBT-EFF-DATE > 0 AND YDBT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'V' TO YDBM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E509 REF NO REQUIRED
      * E066 CONTROL AMT NEGATIVE
      * E318 CONTROL AMT OVER 99999
      * E782 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       4070-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YDBM-REF-NO = SPACES
                   MOVE 'E509' TO WS-REJECT-CODE
               WHEN YDBT-CONTROL-AMT < 0
                   MOVE 'E066' TO WS-REJECT-CODE
               WHEN YDBT-CONTROL-AMT > 99999.00
                   MOVE 'E318' TO WS-REJECT-CODE
               WHEN YDBM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E782' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4080-COUNT-CYCLE.
           IF YDBM-CYCLE-CNT < 9999999
               ADD 1 TO YDBM-CYCLE-CNT
           END-IF
           IF YDBM-CYCLE-CNT NOT < 6
               MOVE 'Y' TO YDBM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CONTROL TIERX TIER FOR THE AMOUNT 1 AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4090-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-CONTROL-TIERX-RATE(1) TO YDBM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF YDBT-AMOUNT-1-AMT NOT < WS-CONTROL-TIERX-LOW(WS-TX)
                   MOVE WS-CONTROL-TIERX-RATE(WS-TX) TO YDBM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF YDBM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO YDBM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4100-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YDBM-SOURCE-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YDBM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PIVOT CD:
      * KGT -> NNG WHEN LAST PAY DATE IS PAST
      * NNG -> PTC WHEN LAST PAY DATE IS PAST
      * PTC -> WZW WHEN LAST PAY DATE IS PAST
      * WZW -> KGT WHEN LAST PAY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4110-SET-PIVOT.
           EVALUATE YDBM-PIVOT-CD
               WHEN 'KGT'
                   IF YDBM-LAST-PAY-DATE > 0 AND YDBM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NNG' TO YDBM-PIVOT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NNG'
                   IF YDBM-LAST-PAY-DATE > 0 AND YDBM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PTC' TO YDBM-PIVOT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PTC'
                   IF YDBM-LAST-PAY-DATE > 0 AND YDBM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WZW' TO YDBM-PIVOT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WZW'
                   IF YDBM-LAST-PAY-DATE > 0 AND YDBM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KGT' TO YDBM-PIVOT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E881 REJECT CD REQUIRED
      * E670 AMOUNT 2 AMT NEGATIVE
      * E028 AMOUNT 2 AMT OVER 25000
      * E990 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       4120-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YDBT-REJECT-CD = SPACES
                   MOVE 'E881' TO WS-REJECT-CODE
               WHEN YDBM-AMOUNT-2-AMT < 0
                   MOVE 'E670' TO WS-REJECT-CODE
               WHEN YDBM-AMOUNT-2-AMT > 25000.00
                   MOVE 'E028' TO WS-REJECT-CODE
               WHEN YDBM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E990' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT AMOUNT 1 YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4130-SPLIT-AMOUNT-1.
           COMPUTE WS-WORK-AMT = YDBM-AMOUNT-1-YTD / 12
           COMPUTE WS-WORK-AMT-2 = YDBM-AMOUNT-1-YTD - (WS-WORK-AMT *
               12)
           COMPUTE YDBT-AMOUNT-1-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       4140-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF YDBT-POST-DATE > 0 AND YDBT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YDBT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD YDBM-AMOUNT-2-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD YDBM-AMOUNT-2-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD YDBM-AMOUNT-2-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD YDBM-AMOUNT-2-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD YDBM-AMOUNT-2-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E528 MEMO REQUIRED
      * E167 AMOUNT 1 AMT NEGATIVE
      * E996 AMOUNT 1 AMT OVER 1000000
      * E380 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       4150-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YDBT-MEMO = SPACES
                   MOVE 'E528' TO WS-REJECT-CODE
               WHEN YDBT-AMOUNT-1-AMT < 0
                   MOVE 'E167' TO WS-REJECT-CODE
               WHEN YDBT-AMOUNT-1-AMT > 1000000.00
                   MOVE 'E996' TO WS-REJECT-CODE
               WHEN YDBM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E380' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4160-FLAG-ITEM.
           IF WS-WORK-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CONTROL TIERX TIER FOR THE AMOUNT 2 AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4170-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-CONTROL-TIERX-RATE(1) TO YDBM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF YDBM-AMOUNT-2-AMT NOT < WS-CONTROL-TIERX-LOW(WS-TX)
                   MOVE WS-CONTROL-TIERX-RATE(WS-TX) TO YDBM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF YDBM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO YDBM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON AMOUNT 1 YTD AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4180-ACCRUE-AMOUNT-1.
           IF YDBM-AMOUNT-1-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   YDBM-AMOUNT-1-YTD * YDBM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO YDBM-AMOUNT-1-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CONTROL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO AMOUNT 2 AMT.
      *----------------------------------------------------------------*
       4190-PRORATE-AMOUNT-2.
           IF WS-PERIOD-DAYS > 0
               COMPUTE YDBM-AMOUNT-2-AMT ROUNDED =
                   YDBM-CONTROL-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO YDBM-AMOUNT-2-AMT
           END-IF
           ADD YDBM-AMOUNT-2-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT CONTROL AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4200-SPLIT-CONTROL.
           COMPUTE WS-WORK-AMT = YDBM-CONTROL-AMT / 2
           COMPUTE WS-WORK-AMT-2 = YDBM-CONTROL-AMT - (WS-WORK-AMT * 2)
           COMPUTE YDBT-AMOUNT-2-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CONTROL TIERX TIER FOR THE AMOUNT 2 AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4210-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CONTROL-TIERX-RATE(1) TO YDBM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF YDBM-AMOUNT-2-AMT NOT < WS-CONTROL-TIERX-LOW(WS-TX)
                   MOVE WS-CONTROL-TIERX-RATE(WS-TX) TO YDBM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF YDBM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO YDBM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE AMOUNT 2 AMT BY PIVOT CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4220-ACCUM-PIVOT.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YDBM-PIVOT-CD
                   ADD YDBT-AMOUNT-2-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YDBM-PIVOT-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE YDBT-AMOUNT-2-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT 2 AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4230-FLAG-AMOUNT-2.
           IF YDBT-AMOUNT-2-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE YDBT-AMOUNT-2-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE AMOUNT 1 FEE ON THE AMOUNT 2 AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4240-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN YDBM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO YDBT-AMOUNT-1-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YDBM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE YDBT-AMOUNT-1-AMT ROUNDED = YDBM-AMOUNT-2-AMT
                       * YDBM-MARGIN / 200
               WHEN YDBM-REASON-CD = 'AX'
                   MOVE 3.00 TO YDBT-AMOUNT-1-AMT
               WHEN YDBM-REASON-CD = 'LK'
                   MOVE 5.00 TO YDBT-AMOUNT-1-AMT
               WHEN OTHER
                   COMPUTE YDBT-AMOUNT-1-AMT ROUNDED = YDBM-AMOUNT-2-AMT
                       * YDBM-MARGIN / 100
           END-EVALUATE
           IF YDBT-AMOUNT-1-AMT > 0 AND YDBT-AMOUNT-1-AMT < 1.00
               MOVE 1.00 TO YDBT-AMOUNT-1-AMT
           END-IF
           IF YDBT-AMOUNT-1-AMT > 35.00
               MOVE 35.00 TO YDBT-AMOUNT-1-AMT
           END-IF
           ADD YDBT-AMOUNT-1-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON AMOUNT 1 AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4250-ACCRUE-AMOUNT-1.
           IF YDBT-AMOUNT-1-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   YDBT-AMOUNT-1-AMT * YDBM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       4260-COUNT-ITEM.
           IF YDBM-ITEM-CNT < 9999999
               ADD 1 TO YDBM-ITEM-CNT
           END-IF
           IF YDBM-ITEM-CNT NOT < 6
               MOVE 'Y' TO YDBM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE AMOUNT 1 YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4270-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YDBM-SOURCE-CD
                   ADD YDBM-AMOUNT-1-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YDBM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE YDBM-AMOUNT-1-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * CONTROL AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4280-FLAG-CONTROL.
           IF YDBM-CONTROL-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE YDBM-CONTROL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 90 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4290-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF YDBT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YDBT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO YDBM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE YDB FIELDS TO YKLIM AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7040-CALL-YKLIM.
           INITIALIZE YKLIM-PARMS
           MOVE YDBM-AMOUNT-2-AMT
             TO YKLIM-AMOUNT-2-AMT
           MOVE YDBM-AMOUNT-1-YTD
             TO YKLIM-AMOUNT-1-YTD
           MOVE YDBM-CONTROL-AMT
             TO YKLIM-CONTROL-AMT
           MOVE YDBM-AMOUNT-1-AMT
             TO YKLIM-AMOUNT-1-AMT
           MOVE YDBM-AMOUNT-2-YTD
             TO YKLIM-AMOUNT-2-YTD
           MOVE YDBM-CONTROL-YTD
             TO YKLIM-CONTROL-YTD
           MOVE YDBM-LAST-MAINT-DATE
             TO YKLIM-LAST-MAINT-DATE
           MOVE YDBM-OPEN-DATE
             TO YKLIM-OPEN-DATE
           MOVE YDBM-LAST-PAY-DATE
             TO YKLIM-LAST-PAY-DATE
           MOVE YDBM-LAST-STMT-DATE
             TO YKLIM-LAST-STMT-DATE
           MOVE YDBM-PIVOT-CD
             TO YKLIM-PIVOT-CD
           MOVE YDBM-REGION-CD
             TO YKLIM-REGION-CD
           MOVE YDBM-SOURCE-CD
             TO YKLIM-SOURCE-CD
           MOVE YDBM-WINDOW-RULE-CD
             TO YKLIM-WINDOW-RULE-CD
           MOVE YDBM-PREFERRED-FLAG
             TO YKLIM-PREFERRED-FLAG
           MOVE YDBM-OVERRIDE-FLAG
             TO YKLIM-OVERRIDE-FLAG
           MOVE YDBM-NOTICE-SENT-FLAG
             TO YKLIM-NOTICE-SENT-FLAG
           MOVE YDBM-CYCLE-CNT
             TO YKLIM-CYCLE-CNT
           MOVE YDBM-PAYMENT-CNT
             TO YKLIM-PAYMENT-CNT
           MOVE YDBM-PENALTY-PCT
             TO YKLIM-PENALTY-PCT
           MOVE YDBM-DISCOUNT
             TO YKLIM-DISCOUNT
           MOVE YDBM-DESC
             TO YKLIM-DESC
           MOVE YDBM-CONTACT
             TO YKLIM-CONTACT
           CALL 'YKLIM' USING YKLIM-PARMS
           EVALUATE YKLIM-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE YKLIM-AMOUNT-1-YTD
                     TO YDBM-AMOUNT-1-YTD
                   MOVE YKLIM-AMOUNT-2-YTD
                     TO YDBM-AMOUNT-2-YTD
                   MOVE YKLIM-OVERRIDE-FLAG
                     TO YDBM-OVERRIDE-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RLIM' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7040-CALL-YKLIM' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE YDB FIELDS TO YKPRC AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7000-CALL-YKPRC.
           INITIALIZE YKPRC-PARMS
           MOVE YDBM-AMOUNT-1-AMT
             TO YKPRC-AMOUNT-1-AMT
           MOVE YDBM-CONTROL-YTD
             TO YKPRC-CONTROL-YTD
           MOVE YDBM-AMOUNT-1-YTD
             TO YKPRC-AMOUNT-1-YTD
           MOVE YDBM-AMOUNT-2-AMT
             TO YKPRC-AMOUNT-2-AMT
           MOVE YDBM-CONTROL-AMT
             TO YKPRC-CONTROL-AMT
           MOVE YDBM-AMOUNT-2-YTD
             TO YKPRC-AMOUNT-2-YTD
           MOVE YDBM-EXPIRY-DATE
             TO YKPRC-EXPIRY-DATE
           MOVE YDBM-LAST-STMT-DATE
             TO YKPRC-LAST-STMT-DATE
           MOVE YDBM-MATURITY-DATE
             TO YKPRC-MATURITY-DATE
           MOVE YDBM-LAST-ACT-DATE
             TO YKPRC-LAST-ACT-DATE
           MOVE YDBM-CYCLE-CD
             TO YKPRC-CYCLE-CD
           MOVE YDBM-SOURCE-CD
             TO YKPRC-SOURCE-CD
           MOVE YDBM-PIVOT-CD
             TO YKPRC-PIVOT-CD
           MOVE YDBM-REGION-CD
             TO YKPRC-REGION-CD
           MOVE YDBM-NOTICE-SENT-FLAG
             TO YKPRC-NOTICE-SENT-FLAG
           MOVE YDBM-AUTO-RENEW-FLAG
             TO YKPRC-AUTO-RENEW-FLAG
           MOVE YDBM-PREFERRED-FLAG
             TO YKPRC-PREFERRED-FLAG
           MOVE YDBM-ITEM-CNT
             TO YKPRC-ITEM-CNT
           MOVE YDBM-PAYMENT-CNT
             TO YKPRC-PAYMENT-CNT
           MOVE YDBM-MARGIN
             TO YKPRC-MARGIN
           MOVE YDBM-DISCOUNT
             TO YKPRC-DISCOUNT
           MOVE YDBM-NAME
             TO YKPRC-NAME
           MOVE YDBM-CONTACT
             TO YKPRC-CONTACT
           CALL 'YKPRC' USING YKPRC-PARMS
           EVALUATE YKPRC-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE YKPRC-CONTROL-YTD
                     TO YDBM-CONTROL-YTD
                   MOVE YKPRC-AMOUNT-2-AMT
                     TO YDBM-AMOUNT-2-AMT
                   MOVE YKPRC-CONTROL-AMT
                     TO YDBM-CONTROL-AMT
                   MOVE YKPRC-AMOUNT-2-YTD
                     TO YDBM-AMOUNT-2-YTD
                   MOVE YKPRC-NOTICE-SENT-FLAG
                     TO YDBM-NOTICE-SENT-FLAG
                   MOVE YKPRC-AUTO-RENEW-FLAG
                     TO YDBM-AUTO-RENEW-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RPRC' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-YKPRC' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE YDB FIELDS TO YKTIER AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7010-CALL-YKTIER.
           INITIALIZE YKTIER-PARMS
           MOVE YDBM-AMOUNT-2-YTD
             TO YKTIER-AMOUNT-2-YTD
           MOVE YDBM-CONTROL-YTD
             TO YKTIER-CONTROL-YTD
           MOVE YDBM-AMOUNT-1-YTD
             TO YKTIER-AMOUNT-1-YTD
           MOVE YDBM-AMOUNT-2-AMT
             TO YKTIER-AMOUNT-2-AMT
           MOVE YDBM-AMOUNT-1-AMT
             TO YKTIER-AMOUNT-1-AMT
           MOVE YDBM-CONTROL-AMT
             TO YKTIER-CONTROL-AMT
           MOVE YDBM-LAST-ACT-DATE
             TO YKTIER-LAST-ACT-DATE
           MOVE YDBM-MATURITY-DATE
             TO YKTIER-MATURITY-DATE
           MOVE YDBM-LAST-MAINT-DATE
             TO YKTIER-LAST-MAINT-DATE
           MOVE YDBM-EXPIRY-DATE
             TO YKTIER-EXPIRY-DATE
           MOVE YDBM-CYCLE-CD
             TO YKTIER-CYCLE-CD
           MOVE YDBM-SOURCE-CD
             TO YKTIER-SOURCE-CD
           MOVE YDBM-REGION-CD
             TO YKTIER-REGION-CD
           MOVE YDBM-PIVOT-CD
             TO YKTIER-PIVOT-CD
           MOVE YDBM-PREFERRED-FLAG
             TO YKTIER-PREFERRED-FLAG
           MOVE YDBM-AUTO-RENEW-FLAG
             TO YKTIER-AUTO-RENEW-FLAG
           MOVE YDBM-REVIEW-FLAG
             TO YKTIER-REVIEW-FLAG
           MOVE YDBM-ITEM-CNT
             TO YKTIER-ITEM-CNT
           MOVE YDBM-RETRY-CNT
             TO YKTIER-RETRY-CNT
           MOVE YDBM-DISCOUNT
             TO YKTIER-DISCOUNT
           MOVE YDBM-PENALTY-PCT
             TO YKTIER-PENALTY-PCT
           MOVE YDBM-CUST-ID
             TO YKTIER-CUST-ID
           MOVE YDBM-CONTACT
             TO YKTIER-CONTACT
           CALL 'YKTIER' USING YKTIER-PARMS
           EVALUATE YKTIER-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE YKTIER-AMOUNT-2-AMT
                     TO YDBM-AMOUNT-2-AMT
                   MOVE YKTIER-PREFERRED-FLAG
                     TO YDBM-PREFERRED-FLAG
                   MOVE YKTIER-AUTO-RENEW-FLAG
                     TO YDBM-AUTO-RENEW-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RIER' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-YKTIER' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE YDBT-KEY TO RD-KEY
           MOVE YDBT-TRAN-CD TO RD-CODE
           MOVE YDBT-AMOUNT-1-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE YDBT-KEY TO RD-KEY
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
           DISPLAY 'YKBACCR READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'YKBACCR' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
