      *================================================================*
      * PROGRAM  : DRPCONV                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : DIVIDEND REINVESTMENT - FILE CONVERSION. READS THE  *
      *            DRP TRANSACTIONS, UPDATES THE DRP MASTER AND        *
      *            PRODUCES THE CONTROL REPORT. BANK HOLDING COMPANY   *
      *            STOCK DIVIDEND REINVESTMENT PLAN                    *
      * INPUT    : DRPMST (KSDS), DRPTRN, BANKCTL                      *
      * OUTPUT   : DRPOUT, DRPCORPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, DRSTAT, DRELG           *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1984-08-25 TWH  ORIGINAL                                       *
      * 1992-10-01 RJT  CASH-PAY TABLE EXPANDED PRB1758                *
      * 1993-09-24 JLM  SHARES-AMT EXCEPTIONS TO OPS QUEUE REQ8071     *
      * 1993-09-21 SKN  CASH-PAY ROUNDING - HALF UP REQ3070            *
      * 1995-06-14 LMB  FEE FEE SCHEDULE UPDATE REQ7288                *
      * 1998-12-09 JLM  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1998-09-18 LMB  WITHHOLD LIMIT REVISED CHG7518                 *
      * 1999-06-18 LMB  WITHHOLD FEE SCHEDULE UPDATE PRB4616           *
      * 2002-04-25 PQR  FEE LIMIT REVISED REQ4821                      *
      * 2002-08-18 BHS  SHARES-AMT LIMIT REVISED REQ3859               *
      * 2005-03-21 PQR  AUDIT FINDING - DIVIDEND CONTROL TOTALS PRB9536*
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DRPCONV.
       AUTHOR. D P SHAW.

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
           SELECT REPORT-FILE ASSIGN TO "DRPCORPT"
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
                                           VALUE 12.000000.
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
      * WS-SHARES-A-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-SHARES-A-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000557000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000204000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000118000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000200000000046000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000700000000623000'.
       01  WS-SHARES-A-SCHED-TBL REDEFINES WS-SHARES-A-SCHED-VALUES.
           05  WS-SHARES-A-SCHED-ROW OCCURS 5 TIMES.
               10  WS-SHARES-A-SCHED-LOW   PIC 9(09)V99.
               10  WS-SHARES-A-SCHED-RATE  PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-DIVIDEND-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-DIVIDEND-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000536000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000521500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001500000000871000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000071000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012500000000238000'.
       01  WS-DIVIDEND-TIER-TBL REDEFINES WS-DIVIDEND-TIER-VALUES.
           05  WS-DIVIDEND-TIER-ROW OCCURS 5 TIMES.
               10  WS-DIVIDEND-TIER-LOW    PIC 9(09)V99.
               10  WS-DIVIDEND-TIER-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-CASH-PAY-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CASH-PAY-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000355000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000370000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000600000000634500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000700000000361000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000950000000854500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005950000000219000'.
       01  WS-CASH-PAY-TIER-TBL REDEFINES WS-CASH-PAY-TIER-VALUES.
           05  WS-CASH-PAY-TIER-ROW OCCURS 6 TIMES.
               10  WS-CASH-PAY-TIER-LOW    PIC 9(09)V99.
               10  WS-CASH-PAY-TIER-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-SHARES-A-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-SHARES-A-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000253000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000447000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012500000000589000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022500000000337000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023000000000744500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00024000000000371000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025000000000379000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00035000000000863000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00035050000000472500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00035300000000397000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00035800000000205000'.
       01  WS-SHARES-A-BAND-TBL REDEFINES WS-SHARES-A-BAND-VALUES.
           05  WS-SHARES-A-BAND-ROW OCCURS 11 TIMES.
               10  WS-SHARES-A-BAND-LOW    PIC 9(09)V99.
               10  WS-SHARES-A-BAND-RATE   PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  DRPCONV  DIVIDEND REINVESTMENT'.
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
               PERFORM 3000-ACCRUE-DIVIDEND
               PERFORM 3010-ASSESS-SUSPEND
               PERFORM 3020-FLAG-REINVEST
               PERFORM 3030-SPLIT-CASH-PAY
               PERFORM 3040-ASSESS-REVERSAL
               PERFORM 3050-CHECK-LAST-PAY
               PERFORM 3060-SELECT-DISCOUNT
               PERFORM 3070-ACCUM-REASON
               PERFORM 3080-PRORATE-FEE
               PERFORM 3090-COUNT-ITEM
               PERFORM 3100-SET-SOURCE
               PERFORM 3110-DESCRIBE-CHANNEL
               PERFORM 3120-SELECT-RATE
               PERFORM 3130-PRORATE-SHARES-AMT
               PERFORM 3140-AGE-POST
               PERFORM 3150-AGE-LAST-PAY
               PERFORM 3160-AGE-NEXT-DUE
               PERFORM 3170-FLAG-FEE
               PERFORM 3180-FLAG-ITEM
               PERFORM 3190-CHECK-NEXT-REVIEW
               PERFORM 3200-LIMIT-DIVIDEND
               PERFORM 3210-SPLIT-FEE
               PERFORM 3220-LIMIT-DIVIDEND
               PERFORM 3230-LIMIT-ITEM
               PERFORM 3240-FLAG-DIVIDEND
               PERFORM 3250-CHECK-POST
               PERFORM 3260-SET-CHANNEL
               PERFORM 3270-LIMIT-FEE
               PERFORM 3280-ACCUM-CYCLE
               PERFORM 3290-ACCRUE-DISCOUNT
               PERFORM 3300-AGE-EFF
               PERFORM 3310-AGE-NEXT-DUE
               PERFORM 3320-AGE-EFF
               PERFORM 3330-PRORATE-SHARES-AMT
               PERFORM 3340-DESCRIBE-CYCLE
               PERFORM 3350-ACCUM-CHANNEL
               PERFORM 3360-PRORATE-DIVIDEND
               PERFORM 3370-ACCUM-CYCLE
               PERFORM 3380-AGE-EFF
               PERFORM 3390-SPLIT-WITHHOLD
               PERFORM 3400-SET-HOLDER-TYPE
               PERFORM 3410-SELECT-PENALTY-PCT
               PERFORM 3420-CHECK-LAST-PAY
               PERFORM 3430-AGE-EFFECTIVE
               PERFORM 3440-ACCUM-TRAN
               PERFORM 3450-EDIT-MEMO
               PERFORM 3460-AGE-LAST-MAINT
               PERFORM 3470-LIMIT-CASH-PAY
               PERFORM 3480-SELECT-RATE
               PERFORM 3490-ACCRUE-REINVEST
               PERFORM 3500-FLAG-FEE
               PERFORM 3510-ASSESS-OVERRIDE
               PERFORM 3520-PRORATE-REINVEST
               PERFORM 3530-DESCRIBE-CYCLE
               PERFORM 3540-PRORATE-DIVIDEND
               PERFORM 3550-CHECK-NOTICE
               PERFORM 3560-DESCRIBE-PLAN-OPTION
               PERFORM 3570-SET-CHANNEL
               PERFORM 3580-ACCRUE-WITHHOLD
               PERFORM 3590-DESCRIBE-SOURCE
               PERFORM 3600-SELECT-PENALTY-PCT
               PERFORM 3610-EDIT-CUST-ID
               PERFORM 3620-COUNT-ITEM
               PERFORM 3630-DESCRIBE-TRAN
               PERFORM 3640-ACCRUE-DISCOUNT
               PERFORM 3650-LIMIT-DIVIDEND
               PERFORM 3660-ACCRUE-REINVEST
               PERFORM 3670-AGE-EFFECTIVE
               PERFORM 3680-AGE-LAST-ACT
               PERFORM 3690-LIMIT-FEE
               PERFORM 3700-ACCRUE-RATE
               PERFORM 3710-SPLIT-REINVEST
               PERFORM 3720-DESCRIBE-HOLDER-TYPE
               PERFORM 3730-ACCRUE-FEE
               PERFORM 3740-PRORATE-DIVIDEND
               PERFORM 3750-ASSESS-REVERSAL
               PERFORM 3760-CHECK-NEXT-REVIEW
               PERFORM 3770-ASSESS-SUSPEND
               PERFORM 3780-AGE-NOTICE
               PERFORM 3790-CHECK-NEXT-DUE
               PERFORM 3800-ACCUM-PLAN-OPTION
               PERFORM 3810-AGE-LAST-MAINT
               PERFORM 3820-ACCRUE-FEE
               PERFORM 3830-PRORATE-WITHHOLD
               PERFORM 3840-SPLIT-REINVEST
               PERFORM 3850-CHECK-LAST-MAINT
               PERFORM 3860-AGE-OPEN
               PERFORM 3870-AGE-POST
               PERFORM 3880-COUNT-ITEM
               PERFORM 3890-ACCUM-HOLDER-TYPE
               PERFORM 3900-SET-HOLDER-TYPE
               PERFORM 3910-EDIT-CONTACT
               PERFORM 3920-ACCUM-CYCLE
               PERFORM 3930-EDIT-DESC
               PERFORM 3940-PRORATE-FEE
               PERFORM 3950-COUNT-EXCEPT
               PERFORM 3960-AGE-EFFECTIVE
               PERFORM 3970-SET-SOURCE
               PERFORM 3980-SET-CYCLE
               PERFORM 3990-ACCRUE-CASH-PAY
               PERFORM 4000-ACCUM-CHANNEL
               PERFORM 4010-SELECT-RATE
               PERFORM 4020-COUNT-ITEM
               PERFORM 4030-CHECK-EFF
               PERFORM 4040-SET-CHANNEL
               PERFORM 4050-SPLIT-REINVEST
               PERFORM 4060-COUNT-SEQ
               PERFORM 4070-DESCRIBE-CHANNEL
               PERFORM 4080-ACCUM-CHANNEL
               PERFORM 4090-ACCRUE-CASH-PAY
               PERFORM 4100-ASSESS-PREFERRED
               PERFORM 4110-SET-HOLDER-TYPE
               PERFORM 4120-ASSESS-STOP
               PERFORM 4130-FLAG-ITEM
               PERFORM 4140-CHECK-EFFECTIVE
               PERFORM 4150-DESCRIBE-PLAN-OPTION
               PERFORM 4160-ASSESS-PREFERRED
               PERFORM 4170-EDIT-REF-NO
               PERFORM 4180-SPLIT-REINVEST
               PERFORM 4190-AGE-LAST-PAY
               PERFORM 4200-CHECK-LAST-ACT
               PERFORM 4210-SELECT-RATE
               PERFORM 4220-SELECT-DISCOUNT
               PERFORM 4230-PRORATE-CASH-PAY
               PERFORM 4240-LIMIT-SHARES-AMT
               PERFORM 4250-CHECK-LAST-MAINT
               PERFORM 4260-SELECT-DISCOUNT
               PERFORM 4270-LIMIT-FEE
               PERFORM 4280-SET-TRAN
               PERFORM 4290-SELECT-DISCOUNT
               PERFORM 4300-SET-CYCLE
               PERFORM 4310-SET-TRAN
               PERFORM 4320-SPLIT-SHARES-AMT
               PERFORM 4330-AGE-NEXT-DUE
               PERFORM 4340-SET-TRAN
               PERFORM 4350-ASSESS-PREFERRED
               PERFORM 4360-SPLIT-SHARES-AMT
               PERFORM 4370-SELECT-RATE
               PERFORM 4380-COUNT-EXCEPT
               PERFORM 4390-SET-REASON
               PERFORM 4400-COUNT-ITEM
               PERFORM 4410-DESCRIBE-CHANNEL
               PERFORM 4420-LIMIT-REINVEST
               PERFORM 4430-ACCUM-PLAN-OPTION
               PERFORM 4440-SET-CHANNEL
               PERFORM 4450-LIMIT-FEE
               PERFORM 4460-PRORATE-SHARES-AMT
               PERFORM 4470-ACCUM-HOLDER-TYPE
               PERFORM 4480-COUNT-NOTICE
               PERFORM 4490-FLAG-DIVIDEND
               PERFORM 4500-EDIT-CONTACT
               PERFORM 4510-COUNT-ITEM
               PERFORM 4520-COUNT-NOTICE
               PERFORM 4530-LIMIT-ITEM
               PERFORM 4540-EDIT-OPERATOR
               PERFORM 4550-FLAG-REINVEST
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
      * DAILY INTEREST ON DIVIDEND AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3000-ACCRUE-DIVIDEND.
           IF DRPM-DIVIDEND-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRPM-DIVIDEND-AMT * DRPM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO DRPT-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SHARES FEE ON THE WITHHOLD AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3010-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN DRPM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO DRPM-SHARES-AMT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE DRPM-SHARES-AMT-AMT ROUNDED =
                       DRPM-WITHHOLD-AMT * DRPM-DISCOUNT / 200
               WHEN DRPT-TRAN-CD = 'DV'
                   MOVE 5.00 TO DRPM-SHARES-AMT-AMT
               WHEN DRPT-TRAN-CD = 'RI'
                   MOVE 12.00 TO DRPM-SHARES-AMT-AMT
               WHEN OTHER
                   COMPUTE DRPM-SHARES-AMT-AMT ROUNDED =
                       DRPM-WITHHOLD-AMT * DRPM-DISCOUNT / 100
           END-EVALUATE
           IF DRPM-SHARES-AMT-AMT > 0 AND DRPM-SHARES-AMT-AMT < 10.00
               MOVE 10.00 TO DRPM-SHARES-AMT-AMT
           END-IF
           IF DRPM-SHARES-AMT-AMT > 75.00
               MOVE 75.00 TO DRPM-SHARES-AMT-AMT
           END-IF
           ADD DRPM-SHARES-AMT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * REINVEST YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3020-FLAG-REINVEST.
           IF DRPM-REINVEST-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE DRPM-REINVEST-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CASH PAY AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3030-SPLIT-CASH-PAY.
           COMPUTE WS-WORK-AMT = DRPM-CASH-PAY-AMT / 3
           COMPUTE WS-WORK-AMT-2 = DRPM-CASH-PAY-AMT - (WS-WORK-AMT * 3)
           COMPUTE DRPM-DIVIDEND-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FEE YTD FEE ON THE DIVIDEND AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3040-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN DRPT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO DRPM-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE DRPM-FEE-YTD ROUNDED = DRPT-DIVIDEND-AMT *
                       DRPM-DISCOUNT / 200
               WHEN DRPM-CHANNEL-CD = 'RM'
                   MOVE 3.00 TO DRPM-FEE-YTD
               WHEN DRPM-CHANNEL-CD = 'VP'
                   MOVE 20.00 TO DRPM-FEE-YTD
               WHEN OTHER
                   COMPUTE DRPM-FEE-YTD ROUNDED = DRPT-DIVIDEND-AMT *
                       DRPM-DISCOUNT / 100
           END-EVALUATE
           IF DRPM-FEE-YTD > 0 AND DRPM-FEE-YTD < 1.00
               MOVE 1.00 TO DRPM-FEE-YTD
           END-IF
           IF DRPM-FEE-YTD > 35.00
               MOVE 35.00 TO DRPM-FEE-YTD
           END-IF
           ADD DRPM-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3050-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF DRPM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO DRPT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SHARES A BAND TIER FOR THE WITHHOLD YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3060-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-SHARES-A-BAND-RATE(1) TO DRPM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF DRPM-WITHHOLD-YTD NOT < WS-SHARES-A-BAND-LOW(WS-TX)
                   MOVE WS-SHARES-A-BAND-RATE(WS-TX) TO DRPM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE REINVEST YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3070-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPM-REASON-CD
                   ADD DRPM-REINVEST-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPM-REINVEST-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3080-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRPM-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3090-COUNT-ITEM.
           IF DRPM-ITEM-CNT < 9999999
               ADD 1 TO DRPM-ITEM-CNT
           END-IF
           IF DRPM-ITEM-CNT NOT < 3
               MOVE 'Y' TO DRPM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * BT -> FL WHEN EFF DATE IS PAST
      * FL -> NK WHEN EFF DATE IS PAST
      * NK -> YC WHEN EFF DATE IS PAST
      * YC -> BT WHEN EFF DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3100-SET-SOURCE.
           EVALUATE DRPM-SOURCE-CD
               WHEN 'BT'
                   IF DRPT-EFF-DATE > 0 AND DRPT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FL' TO DRPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FL'
                   IF DRPT-EFF-DATE > 0 AND DRPT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NK' TO DRPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NK'
                   IF DRPT-EFF-DATE > 0 AND DRPT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YC' TO DRPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YC'
                   IF DRPT-EFF-DATE > 0 AND DRPT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BT' TO DRPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3110-DESCRIBE-CHANNEL.
           EVALUATE DRPT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE DIVIDEND TIER TIER FOR THE DIVIDEND AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3120-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-DIVIDEND-TIER-RATE(1) TO DRPM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF DRPT-DIVIDEND-AMT NOT < WS-DIVIDEND-TIER-LOW(WS-TX)
                   MOVE WS-DIVIDEND-TIER-RATE(WS-TX) TO DRPM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO SHARES AMT AMT.
      *----------------------------------------------------------------*
       3130-PRORATE-SHARES-AMT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRPM-SHARES-AMT-AMT ROUNDED =
                   DRPM-FEE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO DRPM-SHARES-AMT-AMT
           END-IF
           ADD DRPM-SHARES-AMT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3140-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF DRPT-POST-DATE > 0 AND DRPT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRPM-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD DRPM-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD DRPM-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD DRPM-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRPM-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3150-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF DRPM-LAST-PAY-DATE > 0 AND DRPM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRPM-SHARES-AMT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD DRPM-SHARES-AMT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD DRPM-SHARES-AMT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD DRPM-SHARES-AMT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRPM-SHARES-AMT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-45 AND OVER 45 DAYS.
      *----------------------------------------------------------------*
       3160-AGE-NEXT-DUE.
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
                   ADD DRPT-DIVIDEND-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD DRPT-DIVIDEND-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD DRPT-DIVIDEND-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 45
                   ADD DRPT-DIVIDEND-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRPT-DIVIDEND-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FEE YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3170-FLAG-FEE.
           IF DRPM-FEE-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE DRPM-FEE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3180-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 30 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3190-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF DRPM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO DRPM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DIVIDEND YTD IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3200-LIMIT-DIVIDEND.
           IF DRPM-DIVIDEND-YTD < 50.00
               MOVE 50.00 TO DRPM-DIVIDEND-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRPM-DIVIDEND-YTD > 10000.00
               MOVE 10000.00 TO DRPM-DIVIDEND-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3210-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = DRPM-FEE-YTD / 3
           COMPUTE WS-WORK-AMT-2 = DRPM-FEE-YTD - (WS-WORK-AMT * 3)
           COMPUTE DRPT-SHARES-AMT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DIVIDEND AMT IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3220-LIMIT-DIVIDEND.
           IF DRPT-DIVIDEND-AMT < 50.00
               MOVE 50.00 TO DRPT-DIVIDEND-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRPT-DIVIDEND-AMT > 10000.00
               MOVE 10000.00 TO DRPT-DIVIDEND-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3230-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 0.00
               MOVE 0.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 5000.00
               MOVE 5000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DIVIDEND AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3240-FLAG-DIVIDEND.
           IF DRPT-DIVIDEND-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE DRPT-DIVIDEND-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 45 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3250-CHECK-POST.
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
               MOVE 'Y' TO DRPM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN LAST MAINT DATE IS PAST
      * BAT -> MAL WHEN LAST MAINT DATE IS PAST
      * MAL -> TEL WHEN LAST MAINT DATE IS PAST
      * TEL -> ONL WHEN LAST MAINT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3260-SET-CHANNEL.
           EVALUATE DRPT-CHANNEL-CD
               WHEN 'BRN'
                   IF DRPM-LAST-MAINT-DATE > 0 AND DRPM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'BAT' TO DRPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF DRPM-LAST-MAINT-DATE > 0 AND DRPM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'MAL' TO DRPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF DRPM-LAST-MAINT-DATE > 0 AND DRPM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'TEL' TO DRPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF DRPM-LAST-MAINT-DATE > 0 AND DRPM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'ONL' TO DRPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FEE YTD IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3270-LIMIT-FEE.
           IF DRPM-FEE-YTD < 100.00
               MOVE 100.00 TO DRPM-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRPM-FEE-YTD > 500.00
               MOVE 500.00 TO DRPM-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE REINVEST AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3280-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPM-CYCLE-CD
                   ADD DRPM-REINVEST-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPM-REINVEST-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3290-ACCRUE-DISCOUNT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * DRPM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO DRPM-CASH-PAY-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3300-AGE-EFF.
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
                   ADD DRPM-REINVEST-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD DRPM-REINVEST-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD DRPM-REINVEST-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD DRPM-REINVEST-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRPM-REINVEST-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3310-AGE-NEXT-DUE.
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
                   ADD DRPM-CASH-PAY-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD DRPM-CASH-PAY-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD DRPM-CASH-PAY-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD DRPM-CASH-PAY-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRPM-CASH-PAY-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3320-AGE-EFF.
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
                   ADD DRPM-DIVIDEND-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD DRPM-DIVIDEND-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD DRPM-DIVIDEND-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD DRPM-DIVIDEND-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRPM-DIVIDEND-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE DIVIDEND AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO SHARES AMT YTD.
      *----------------------------------------------------------------*
       3330-PRORATE-SHARES-AMT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRPM-SHARES-AMT-YTD ROUNDED =
                   DRPM-DIVIDEND-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO DRPM-SHARES-AMT-YTD
           END-IF
           ADD DRPM-SHARES-AMT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3340-DESCRIBE-CYCLE.
           EVALUATE DRPM-CYCLE-CD
               WHEN 'DE'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'FB'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'PC'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'WY'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'XD'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'ZK'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ZR'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE REINVEST AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3350-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPT-CHANNEL-CD
                   ADD DRPM-REINVEST-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPM-REINVEST-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE REINVEST AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DIVIDEND AMT.
      *----------------------------------------------------------------*
       3360-PRORATE-DIVIDEND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRPM-DIVIDEND-AMT ROUNDED =
                   DRPT-REINVEST-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO DRPM-DIVIDEND-AMT
           END-IF
           ADD DRPM-DIVIDEND-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE DIVIDEND YTD BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3370-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPM-CYCLE-CD
                   ADD DRPM-DIVIDEND-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPM-DIVIDEND-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3380-AGE-EFF.
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
                   ADD DRPT-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD DRPT-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD DRPT-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD DRPT-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRPT-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WITHHOLD YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3390-SPLIT-WITHHOLD.
           COMPUTE WS-WORK-AMT = DRPM-WITHHOLD-YTD / 2
           COMPUTE WS-WORK-AMT-2 = DRPM-WITHHOLD-YTD - (WS-WORK-AMT * 2)
           COMPUTE DRPM-DIVIDEND-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR HOLDER TYPE CD:
      * C -> H WHEN POST DATE IS PAST
      * H -> K WHEN POST DATE IS PAST
      * K -> L WHEN POST DATE IS PAST
      * L -> M WHEN POST DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3400-SET-HOLDER-TYPE.
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
      * SELECT THE SHARES A BAND TIER FOR THE DIVIDEND AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3410-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-SHARES-A-BAND-RATE(1) TO DRPM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF DRPT-DIVIDEND-AMT NOT < WS-SHARES-A-BAND-LOW(WS-TX)
                   MOVE WS-SHARES-A-BAND-RATE(WS-TX) TO DRPM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 45 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3420-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF DRPM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO DRPM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3430-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF DRPM-EFFECTIVE-DATE > 0 AND DRPM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRPM-REINVEST-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD DRPM-REINVEST-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD DRPM-REINVEST-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD DRPM-REINVEST-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRPM-REINVEST-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE CASH PAY AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3440-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPT-TRAN-CD
                   ADD DRPM-CASH-PAY-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPM-CASH-PAY-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E972 MEMO REQUIRED
      * E443 CASH PAY YTD NEGATIVE
      * E110 CASH PAY YTD OVER 5000
      * E251 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3450-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPT-MEMO = SPACES
                   MOVE 'E972' TO WS-REJECT-CODE
               WHEN DRPM-CASH-PAY-YTD < 0
                   MOVE 'E443' TO WS-REJECT-CODE
               WHEN DRPM-CASH-PAY-YTD > 5000.00
                   MOVE 'E110' TO WS-REJECT-CODE
               WHEN DRPM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E251' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3460-AGE-LAST-MAINT.
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
                   ADD DRPM-WITHHOLD-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD DRPM-WITHHOLD-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD DRPM-WITHHOLD-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD DRPM-WITHHOLD-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRPM-WITHHOLD-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CASH PAY YTD IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3470-LIMIT-CASH-PAY.
           IF DRPM-CASH-PAY-YTD < 0.00
               MOVE 0.00 TO DRPM-CASH-PAY-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRPM-CASH-PAY-YTD > 500.00
               MOVE 500.00 TO DRPM-CASH-PAY-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SHARES A BAND TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3480-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-SHARES-A-BAND-RATE(1) TO DRPM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF DRPM-FEE-AMT NOT < WS-SHARES-A-BAND-LOW(WS-TX)
                   MOVE WS-SHARES-A-BAND-RATE(WS-TX) TO DRPM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REINVEST AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3490-ACCRUE-REINVEST.
           IF DRPM-REINVEST-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRPM-REINVEST-AMT * DRPM-RATE / 100 / 365
               ADD WS-WORK-AMT TO DRPM-WITHHOLD-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3500-FLAG-FEE.
           IF DRPM-FEE-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE DRPM-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CASH PAY FEE ON THE FEE YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3510-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN DRPM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO DRPM-CASH-PAY-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPT-REVERSAL-FLAG = 'Y'
                   COMPUTE DRPM-CASH-PAY-AMT ROUNDED = DRPM-FEE-YTD *
                       DRPM-DISCOUNT / 200
               WHEN DRPM-CHANNEL-CD = 'RM'
                   MOVE 3.00 TO DRPM-CASH-PAY-AMT
               WHEN DRPM-CHANNEL-CD = 'VP'
                   MOVE 3.00 TO DRPM-CASH-PAY-AMT
               WHEN OTHER
                   COMPUTE DRPM-CASH-PAY-AMT ROUNDED = DRPM-FEE-YTD *
                       DRPM-DISCOUNT / 100
           END-EVALUATE
           IF DRPM-CASH-PAY-AMT > 0 AND DRPM-CASH-PAY-AMT < 2.00
               MOVE 2.00 TO DRPM-CASH-PAY-AMT
           END-IF
           IF DRPM-CASH-PAY-AMT > 150.00
               MOVE 150.00 TO DRPM-CASH-PAY-AMT
           END-IF
           ADD DRPM-CASH-PAY-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO REINVEST YTD.
      *----------------------------------------------------------------*
       3520-PRORATE-REINVEST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRPM-REINVEST-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO DRPM-REINVEST-YTD
           END-IF
           ADD DRPM-REINVEST-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3530-DESCRIBE-CYCLE.
           EVALUATE DRPM-CYCLE-CD
               WHEN 'DE'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'FB'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'PC'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'WY'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'XD'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'ZK'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'ZR'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE REINVEST AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DIVIDEND YTD.
      *----------------------------------------------------------------*
       3540-PRORATE-DIVIDEND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRPM-DIVIDEND-YTD ROUNDED =
                   DRPT-REINVEST-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO DRPM-DIVIDEND-YTD
           END-IF
           ADD DRPM-DIVIDEND-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3550-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF DRPM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO DRPM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PLAN OPTION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3560-DESCRIBE-PLAN-OPTION.
           EVALUATE DRPM-PLAN-OPTION-CD
               WHEN 'E'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'R'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'T'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'Z'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * RM -> VP WHEN POST DATE IS PAST
      * VP -> YS WHEN POST DATE IS PAST
      * YS -> RM WHEN POST DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3570-SET-CHANNEL.
           EVALUATE DRPM-CHANNEL-CD
               WHEN 'RM'
                   IF DRPT-POST-DATE > 0 AND DRPT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VP' TO DRPM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VP'
                   IF DRPT-POST-DATE > 0 AND DRPT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YS' TO DRPM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YS'
                   IF DRPT-POST-DATE > 0 AND DRPT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RM' TO DRPM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WITHHOLD AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3580-ACCRUE-WITHHOLD.
           IF DRPM-WITHHOLD-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRPM-WITHHOLD-AMT * DRPM-RATE / 100 / 360
               ADD WS-WORK-AMT TO DRPM-DIVIDEND-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3590-DESCRIBE-SOURCE.
           EVALUATE DRPM-SOURCE-CD
               WHEN 'BT'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'FL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'NK'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'YC'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE SHARES A BAND TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3600-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-SHARES-A-BAND-RATE(1) TO DRPM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF DRPM-FEE-AMT NOT < WS-SHARES-A-BAND-LOW(WS-TX)
                   MOVE WS-SHARES-A-BAND-RATE(WS-TX) TO DRPM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E560 CUST ID REQUIRED
      * E910 FEE AMT NEGATIVE
      * E365 FEE AMT OVER 250000
      * E662 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3610-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPM-CUST-ID = SPACES
                   MOVE 'E560' TO WS-REJECT-CODE
               WHEN DRPM-FEE-AMT < 0
                   MOVE 'E910' TO WS-REJECT-CODE
               WHEN DRPM-FEE-AMT > 250000.00
                   MOVE 'E365' TO WS-REJECT-CODE
               WHEN DRPM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E662' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3620-COUNT-ITEM.
           IF DRPM-ITEM-CNT < 9999999
               ADD 1 TO DRPM-ITEM-CNT
           END-IF
           IF DRPM-ITEM-CNT NOT < 3
               MOVE 'Y' TO DRPM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3630-DESCRIBE-TRAN.
           EVALUATE DRPT-TRAN-CD
               WHEN 'DV'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'RI'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'CP'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'WH'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'SL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3640-ACCRUE-DISCOUNT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * DRPM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO DRPM-SHARES-AMT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DIVIDEND AMT IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3650-LIMIT-DIVIDEND.
           IF DRPT-DIVIDEND-AMT < 100.00
               MOVE 100.00 TO DRPT-DIVIDEND-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRPT-DIVIDEND-AMT > 10000.00
               MOVE 10000.00 TO DRPT-DIVIDEND-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REINVEST AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3660-ACCRUE-REINVEST.
           IF DRPM-REINVEST-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRPM-REINVEST-AMT * DRPM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO DRPM-WITHHOLD-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3670-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF DRPM-EFFECTIVE-DATE > 0 AND DRPM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRPM-REINVEST-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD DRPM-REINVEST-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD DRPM-REINVEST-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD DRPM-REINVEST-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRPM-REINVEST-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3680-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF DRPM-LAST-ACT-DATE > 0 AND DRPM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRPM-REINVEST-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD DRPM-REINVEST-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD DRPM-REINVEST-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD DRPM-REINVEST-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRPM-REINVEST-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FEE YTD IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3690-LIMIT-FEE.
           IF DRPM-FEE-YTD < 25.00
               MOVE 25.00 TO DRPM-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRPM-FEE-YTD > 5000.00
               MOVE 5000.00 TO DRPM-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3700-ACCRUE-RATE.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * DRPM-RATE / 100 / 365
               ADD WS-WORK-AMT TO DRPM-WITHHOLD-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT REINVEST AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3710-SPLIT-REINVEST.
           COMPUTE WS-WORK-AMT = DRPT-REINVEST-AMT / 12
           COMPUTE WS-WORK-AMT-2 = DRPT-REINVEST-AMT - (WS-WORK-AMT *
               12)
           COMPUTE DRPM-REINVEST-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE HOLDER TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3720-DESCRIBE-HOLDER-TYPE.
           EVALUATE DRPM-HOLDER-TYPE-CD
               WHEN 'C'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'H'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'K'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'L'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'M'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'X'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3730-ACCRUE-FEE.
           IF DRPT-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRPT-FEE-AMT * DRPM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO DRPM-WITHHOLD-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CASH PAY AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO DIVIDEND AMT.
      *----------------------------------------------------------------*
       3740-PRORATE-DIVIDEND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRPT-DIVIDEND-AMT ROUNDED =
                   DRPM-CASH-PAY-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO DRPT-DIVIDEND-AMT
           END-IF
           ADD DRPT-DIVIDEND-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE SHARES YTD FEE ON THE WITHHOLD YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3750-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN DRPT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO DRPM-SHARES-AMT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPM-SUSPEND-FLAG = 'Y'
                   COMPUTE DRPM-SHARES-AMT-YTD ROUNDED =
                       DRPM-WITHHOLD-YTD * DRPM-RATE / 200
               WHEN DRPM-PLAN-OPTION-CD = 'E'
                   MOVE 3.00 TO DRPM-SHARES-AMT-YTD
               WHEN DRPM-PLAN-OPTION-CD = 'R'
                   MOVE 7.00 TO DRPM-SHARES-AMT-YTD
               WHEN OTHER
                   COMPUTE DRPM-SHARES-AMT-YTD ROUNDED =
                       DRPM-WITHHOLD-YTD * DRPM-RATE / 100
           END-EVALUATE
           IF DRPM-SHARES-AMT-YTD > 0 AND DRPM-SHARES-AMT-YTD < 5.00
               MOVE 5.00 TO DRPM-SHARES-AMT-YTD
           END-IF
           IF DRPM-SHARES-AMT-YTD > 50.00
               MOVE 50.00 TO DRPM-SHARES-AMT-YTD
           END-IF
           ADD DRPM-SHARES-AMT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 10 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3760-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF DRPM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO DRPM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FEE FEE ON THE REINVEST AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3770-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN DRPM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO DRPM-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPM-OVERRIDE-FLAG = 'Y'
                   COMPUTE DRPM-FEE-AMT ROUNDED = DRPT-REINVEST-AMT *
                       DRPM-PENALTY-PCT / 200
               WHEN DRPM-CHANNEL-CD = 'RM'
                   MOVE 5.00 TO DRPM-FEE-AMT
               WHEN DRPM-CHANNEL-CD = 'VP'
                   MOVE 10.00 TO DRPM-FEE-AMT
               WHEN OTHER
                   COMPUTE DRPM-FEE-AMT ROUNDED = DRPT-REINVEST-AMT *
                       DRPM-PENALTY-PCT / 100
           END-EVALUATE
           IF DRPM-FEE-AMT > 0 AND DRPM-FEE-AMT < 2.00
               MOVE 2.00 TO DRPM-FEE-AMT
           END-IF
           IF DRPM-FEE-AMT > 100.00
               MOVE 100.00 TO DRPM-FEE-AMT
           END-IF
           ADD DRPM-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3780-AGE-NOTICE.
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
                   ADD DRPM-SHARES-AMT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD DRPM-SHARES-AMT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD DRPM-SHARES-AMT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD DRPM-SHARES-AMT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRPM-SHARES-AMT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3790-CHECK-NEXT-DUE.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO DRPM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CASH PAY YTD BY PLAN OPTION CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3800-ACCUM-PLAN-OPTION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPM-PLAN-OPTION-CD
                   ADD DRPM-CASH-PAY-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPM-PLAN-OPTION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPM-CASH-PAY-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3810-AGE-LAST-MAINT.
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
                   ADD DRPM-WITHHOLD-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD DRPM-WITHHOLD-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD DRPM-WITHHOLD-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD DRPM-WITHHOLD-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRPM-WITHHOLD-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3820-ACCRUE-FEE.
           IF DRPM-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRPM-FEE-YTD * DRPM-RATE / 100 / 360
               ADD WS-WORK-AMT TO DRPM-DIVIDEND-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE REINVEST YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WITHHOLD AMT.
      *----------------------------------------------------------------*
       3830-PRORATE-WITHHOLD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRPM-WITHHOLD-AMT ROUNDED =
                   DRPM-REINVEST-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO DRPM-WITHHOLD-AMT
           END-IF
           ADD DRPM-WITHHOLD-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT REINVEST AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3840-SPLIT-REINVEST.
           COMPUTE WS-WORK-AMT = DRPM-REINVEST-AMT / 2
           COMPUTE WS-WORK-AMT-2 = DRPM-REINVEST-AMT - (WS-WORK-AMT * 2)
           COMPUTE DRPT-REINVEST-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 365 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3850-CHECK-LAST-MAINT.
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
           IF WS-DAYS > 365
               MOVE 'Y' TO DRPM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3860-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF DRPM-OPEN-DATE > 0 AND DRPM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRPM-DIVIDEND-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD DRPM-DIVIDEND-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD DRPM-DIVIDEND-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD DRPM-DIVIDEND-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRPM-DIVIDEND-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3870-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF DRPT-POST-DATE > 0 AND DRPT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPT-POST-DATE TO DTU-DATE-1
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
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3880-COUNT-ITEM.
           IF DRPM-ITEM-CNT < 9999999
               ADD 1 TO DRPM-ITEM-CNT
           END-IF
           IF DRPM-ITEM-CNT NOT < 6
               MOVE 'Y' TO DRPM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY HOLDER TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3890-ACCUM-HOLDER-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPM-HOLDER-TYPE-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPM-HOLDER-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR HOLDER TYPE CD:
      * C -> H WHEN OPEN DATE IS PAST
      * H -> K WHEN OPEN DATE IS PAST
      * K -> L WHEN OPEN DATE IS PAST
      * L -> M WHEN OPEN DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3900-SET-HOLDER-TYPE.
           EVALUATE DRPM-HOLDER-TYPE-CD
               WHEN 'C'
                   IF DRPM-OPEN-DATE > 0 AND DRPM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO DRPM-HOLDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF DRPM-OPEN-DATE > 0 AND DRPM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'K' TO DRPM-HOLDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF DRPM-OPEN-DATE > 0 AND DRPM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO DRPM-HOLDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF DRPM-OPEN-DATE > 0 AND DRPM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO DRPM-HOLDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E455 CONTACT REQUIRED
      * E240 SHARES AMT YTD NEGATIVE
      * E313 SHARES AMT YTD OVER 250000
      * E275 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3910-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPM-CONTACT = SPACES
                   MOVE 'E455' TO WS-REJECT-CODE
               WHEN DRPM-SHARES-AMT-YTD < 0
                   MOVE 'E240' TO WS-REJECT-CODE
               WHEN DRPM-SHARES-AMT-YTD > 250000.00
                   MOVE 'E313' TO WS-REJECT-CODE
               WHEN DRPM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E275' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE REINVEST AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3920-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPM-CYCLE-CD
                   ADD DRPM-REINVEST-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPM-REINVEST-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E872 DESC REQUIRED
      * E288 WORK AMT 2 NEGATIVE
      * E739 WORK AMT 2 OVER 10000
      * E932 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3930-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPM-DESC = SPACES
                   MOVE 'E872' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E288' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 10000.00
                   MOVE 'E739' TO WS-REJECT-CODE
               WHEN DRPT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E932' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE REINVEST YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FEE AMT.
      *----------------------------------------------------------------*
       3940-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRPM-FEE-AMT ROUNDED =
                   DRPM-REINVEST-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO DRPM-FEE-AMT
           END-IF
           ADD DRPM-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3950-COUNT-EXCEPT.
           IF DRPM-EXCEPT-CNT < 9999999
               ADD 1 TO DRPM-EXCEPT-CNT
           END-IF
           IF DRPM-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO DRPM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3960-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF DRPM-EFFECTIVE-DATE > 0 AND DRPM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * BT -> FL WHEN NEXT REVIEW DATE IS PAST
      * FL -> NK WHEN NEXT REVIEW DATE IS PAST
      * NK -> YC WHEN NEXT REVIEW DATE IS PAST
      * YC -> BT WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3970-SET-SOURCE.
           EVALUATE DRPM-SOURCE-CD
               WHEN 'BT'
                   IF DRPM-NEXT-REVIEW-DATE > 0 AND
                       DRPM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'FL' TO DRPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FL'
                   IF DRPM-NEXT-REVIEW-DATE > 0 AND
                       DRPM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'NK' TO DRPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NK'
                   IF DRPM-NEXT-REVIEW-DATE > 0 AND
                       DRPM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'YC' TO DRPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YC'
                   IF DRPM-NEXT-REVIEW-DATE > 0 AND
                       DRPM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'BT' TO DRPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * DE -> FB WHEN OPEN DATE IS PAST
      * FB -> PC WHEN OPEN DATE IS PAST
      * PC -> WY WHEN OPEN DATE IS PAST
      * WY -> XD WHEN OPEN DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3980-SET-CYCLE.
           EVALUATE DRPM-CYCLE-CD
               WHEN 'DE'
                   IF DRPM-OPEN-DATE > 0 AND DRPM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FB' TO DRPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FB'
                   IF DRPM-OPEN-DATE > 0 AND DRPM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PC' TO DRPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PC'
                   IF DRPM-OPEN-DATE > 0 AND DRPM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WY' TO DRPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WY'
                   IF DRPM-OPEN-DATE > 0 AND DRPM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XD' TO DRPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CASH PAY YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3990-ACCRUE-CASH-PAY.
           IF DRPM-CASH-PAY-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRPM-CASH-PAY-YTD * DRPM-RATE / 100 / 360
               ADD WS-WORK-AMT TO DRPM-SHARES-AMT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4000-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPM-CHANNEL-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DIVIDEND TIER TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4010-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-DIVIDEND-TIER-RATE(1) TO DRPM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF WS-WORK-AMT NOT < WS-DIVIDEND-TIER-LOW(WS-TX)
                   MOVE WS-DIVIDEND-TIER-RATE(WS-TX) TO DRPM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4020-COUNT-ITEM.
           IF DRPM-ITEM-CNT < 9999999
               ADD 1 TO DRPM-ITEM-CNT
           END-IF
           IF DRPM-ITEM-CNT NOT < 5
               MOVE 'Y' TO DRPM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 120 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4030-CHECK-EFF.
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
           IF WS-DAYS > 120
               MOVE 'Y' TO DRPM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * RM -> VP WHEN LAST MAINT DATE IS PAST
      * VP -> YS WHEN LAST MAINT DATE IS PAST
      * YS -> RM WHEN LAST MAINT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4040-SET-CHANNEL.
           EVALUATE DRPM-CHANNEL-CD
               WHEN 'RM'
                   IF DRPM-LAST-MAINT-DATE > 0 AND DRPM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'VP' TO DRPM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VP'
                   IF DRPM-LAST-MAINT-DATE > 0 AND DRPM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'YS' TO DRPM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YS'
                   IF DRPM-LAST-MAINT-DATE > 0 AND DRPM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'RM' TO DRPM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT REINVEST YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4050-SPLIT-REINVEST.
           COMPUTE WS-WORK-AMT = DRPM-REINVEST-YTD / 3
           COMPUTE WS-WORK-AMT-2 = DRPM-REINVEST-YTD - (WS-WORK-AMT * 3)
           COMPUTE DRPM-WITHHOLD-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       4060-COUNT-SEQ.
           IF DRPT-SEQ < 9999999
               ADD 1 TO DRPT-SEQ
           END-IF
           IF DRPT-SEQ NOT < 3
               MOVE 'Y' TO DRPM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4070-DESCRIBE-CHANNEL.
           EVALUATE DRPT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE DIVIDEND YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4080-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPT-CHANNEL-CD
                   ADD DRPM-DIVIDEND-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPM-DIVIDEND-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CASH PAY YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4090-ACCRUE-CASH-PAY.
           IF DRPM-CASH-PAY-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRPM-CASH-PAY-YTD * DRPM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO DRPM-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CASH PAY YTD FEE ON THE FEE AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4100-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN DRPM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO DRPM-CASH-PAY-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPM-EXEMPT-FLAG = 'Y'
                   COMPUTE DRPM-CASH-PAY-YTD ROUNDED = DRPM-FEE-AMT *
                       DRPM-PENALTY-PCT / 200
               WHEN DRPM-PLAN-OPTION-CD = 'E'
                   MOVE 15.00 TO DRPM-CASH-PAY-YTD
               WHEN DRPM-PLAN-OPTION-CD = 'R'
                   MOVE 7.00 TO DRPM-CASH-PAY-YTD
               WHEN OTHER
                   COMPUTE DRPM-CASH-PAY-YTD ROUNDED = DRPM-FEE-AMT *
                       DRPM-PENALTY-PCT / 100
           END-EVALUATE
           IF DRPM-CASH-PAY-YTD > 0 AND DRPM-CASH-PAY-YTD < 10.00
               MOVE 10.00 TO DRPM-CASH-PAY-YTD
           END-IF
           IF DRPM-CASH-PAY-YTD > 50.00
               MOVE 50.00 TO DRPM-CASH-PAY-YTD
           END-IF
           ADD DRPM-CASH-PAY-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR HOLDER TYPE CD:
      * C -> H WHEN NEXT REVIEW DATE IS PAST
      * H -> K WHEN NEXT REVIEW DATE IS PAST
      * K -> L WHEN NEXT REVIEW DATE IS PAST
      * L -> M WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4110-SET-HOLDER-TYPE.
           EVALUATE DRPM-HOLDER-TYPE-CD
               WHEN 'C'
                   IF DRPM-NEXT-REVIEW-DATE > 0 AND
                       DRPM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'H' TO DRPM-HOLDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF DRPM-NEXT-REVIEW-DATE > 0 AND
                       DRPM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'K' TO DRPM-HOLDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF DRPM-NEXT-REVIEW-DATE > 0 AND
                       DRPM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'L' TO DRPM-HOLDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF DRPM-NEXT-REVIEW-DATE > 0 AND
                       DRPM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'M' TO DRPM-HOLDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE CASH PAY FEE ON THE FEE AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4120-ASSESS-STOP.
           EVALUATE TRUE
               WHEN DRPM-STOP-FLAG = 'Y'
                   MOVE ZERO TO DRPM-CASH-PAY-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPM-SUSPEND-FLAG = 'Y'
                   COMPUTE DRPM-CASH-PAY-AMT ROUNDED = DRPT-FEE-AMT *
                       DRPM-DISCOUNT / 200
               WHEN DRPT-TRAN-CD = 'DV'
                   MOVE 10.00 TO DRPM-CASH-PAY-AMT
               WHEN DRPT-TRAN-CD = 'RI'
                   MOVE 5.00 TO DRPM-CASH-PAY-AMT
               WHEN OTHER
                   COMPUTE DRPM-CASH-PAY-AMT ROUNDED = DRPT-FEE-AMT *
                       DRPM-DISCOUNT / 100
           END-EVALUATE
           IF DRPM-CASH-PAY-AMT > 0 AND DRPM-CASH-PAY-AMT < 5.00
               MOVE 5.00 TO DRPM-CASH-PAY-AMT
           END-IF
           IF DRPM-CASH-PAY-AMT > 75.00
               MOVE 75.00 TO DRPM-CASH-PAY-AMT
           END-IF
           ADD DRPM-CASH-PAY-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4130-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4140-CHECK-EFFECTIVE.
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
           IF WS-DAYS > 30
               MOVE 'Y' TO DRPM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PLAN OPTION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4150-DESCRIBE-PLAN-OPTION.
           EVALUATE DRPM-PLAN-OPTION-CD
               WHEN 'E'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'R'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'T'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'Z'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE REINVEST YTD FEE ON THE FEE YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4160-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN DRPM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO DRPM-REINVEST-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPM-SUSPEND-FLAG = 'Y'
                   COMPUTE DRPM-REINVEST-YTD ROUNDED = DRPM-FEE-YTD *
                       DRPM-RATE / 200
               WHEN DRPM-PLAN-OPTION-CD = 'E'
                   MOVE 20.00 TO DRPM-REINVEST-YTD
               WHEN DRPM-PLAN-OPTION-CD = 'R'
                   MOVE 10.00 TO DRPM-REINVEST-YTD
               WHEN OTHER
                   COMPUTE DRPM-REINVEST-YTD ROUNDED = DRPM-FEE-YTD *
                       DRPM-RATE / 100
           END-EVALUATE
           IF DRPM-REINVEST-YTD > 0 AND DRPM-REINVEST-YTD < 1.00
               MOVE 1.00 TO DRPM-REINVEST-YTD
           END-IF
           IF DRPM-REINVEST-YTD > 25.00
               MOVE 25.00 TO DRPM-REINVEST-YTD
           END-IF
           ADD DRPM-REINVEST-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E832 REF NO REQUIRED
      * E767 DIVIDEND AMT NEGATIVE
      * E272 DIVIDEND AMT OVER 99999
      * E674 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       4170-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPM-REF-NO = SPACES
                   MOVE 'E832' TO WS-REJECT-CODE
               WHEN DRPM-DIVIDEND-AMT < 0
                   MOVE 'E767' TO WS-REJECT-CODE
               WHEN DRPM-DIVIDEND-AMT > 99999.00
                   MOVE 'E272' TO WS-REJECT-CODE
               WHEN DRPM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E674' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT REINVEST AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4180-SPLIT-REINVEST.
           COMPUTE WS-WORK-AMT = DRPT-REINVEST-AMT / 12
           COMPUTE WS-WORK-AMT-2 = DRPT-REINVEST-AMT - (WS-WORK-AMT *
               12)
           COMPUTE DRPM-WITHHOLD-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4190-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF DRPM-LAST-PAY-DATE > 0 AND DRPM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRPM-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD DRPM-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD DRPM-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD DRPM-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRPM-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 30 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4200-CHECK-LAST-ACT.
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
           IF WS-DAYS > 30
               MOVE 'Y' TO DRPM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DIVIDEND TIER TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4210-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-DIVIDEND-TIER-RATE(1) TO DRPM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF DRPM-FEE-AMT NOT < WS-DIVIDEND-TIER-LOW(WS-TX)
                   MOVE WS-DIVIDEND-TIER-RATE(WS-TX) TO DRPM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DIVIDEND TIER TIER FOR THE REINVEST AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4220-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-DIVIDEND-TIER-RATE(1) TO DRPM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF DRPM-REINVEST-AMT NOT < WS-DIVIDEND-TIER-LOW(WS-TX)
                   MOVE WS-DIVIDEND-TIER-RATE(WS-TX) TO DRPM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DIVIDEND AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CASH PAY YTD.
      *----------------------------------------------------------------*
       4230-PRORATE-CASH-PAY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRPM-CASH-PAY-YTD ROUNDED =
                   DRPM-DIVIDEND-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO DRPM-CASH-PAY-YTD
           END-IF
           ADD DRPM-CASH-PAY-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SHARES AMT AMT IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4240-LIMIT-SHARES-AMT.
           IF DRPT-SHARES-AMT-AMT < 100.00
               MOVE 100.00 TO DRPT-SHARES-AMT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRPT-SHARES-AMT-AMT > 500.00
               MOVE 500.00 TO DRPT-SHARES-AMT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 45 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4250-CHECK-LAST-MAINT.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO DRPM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SHARES A BAND TIER FOR THE SHARES AMT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4260-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-SHARES-A-BAND-RATE(1) TO DRPM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF DRPT-SHARES-AMT-AMT NOT < WS-SHARES-A-BAND-LOW(WS-TX)
                   MOVE WS-SHARES-A-BAND-RATE(WS-TX) TO DRPM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4270-LIMIT-FEE.
           IF DRPM-FEE-AMT < 50.00
               MOVE 50.00 TO DRPM-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRPM-FEE-AMT > 2500.00
               MOVE 2500.00 TO DRPM-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * DV -> RI WHEN NOTICE DATE IS PAST
      * RI -> CP WHEN NOTICE DATE IS PAST
      * CP -> WH WHEN NOTICE DATE IS PAST
      * WH -> SL WHEN NOTICE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4280-SET-TRAN.
           EVALUATE DRPT-TRAN-CD
               WHEN 'DV'
                   IF DRPM-NOTICE-DATE > 0 AND DRPM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RI' TO DRPT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RI'
                   IF DRPM-NOTICE-DATE > 0 AND DRPM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CP' TO DRPT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CP'
                   IF DRPM-NOTICE-DATE > 0 AND DRPM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WH' TO DRPT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WH'
                   IF DRPM-NOTICE-DATE > 0 AND DRPM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SL' TO DRPT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE SHARES A BAND TIER FOR THE WITHHOLD YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4290-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-SHARES-A-BAND-RATE(1) TO DRPM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF DRPM-WITHHOLD-YTD NOT < WS-SHARES-A-BAND-LOW(WS-TX)
                   MOVE WS-SHARES-A-BAND-RATE(WS-TX) TO DRPM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * DE -> FB WHEN NEXT DUE DATE IS PAST
      * FB -> PC WHEN NEXT DUE DATE IS PAST
      * PC -> WY WHEN NEXT DUE DATE IS PAST
      * WY -> XD WHEN NEXT DUE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4300-SET-CYCLE.
           EVALUATE DRPM-CYCLE-CD
               WHEN 'DE'
                   IF DRPM-NEXT-DUE-DATE > 0 AND DRPM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FB' TO DRPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FB'
                   IF DRPM-NEXT-DUE-DATE > 0 AND DRPM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PC' TO DRPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PC'
                   IF DRPM-NEXT-DUE-DATE > 0 AND DRPM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WY' TO DRPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WY'
                   IF DRPM-NEXT-DUE-DATE > 0 AND DRPM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XD' TO DRPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * DV -> RI WHEN OPEN DATE IS PAST
      * RI -> CP WHEN OPEN DATE IS PAST
      * CP -> WH WHEN OPEN DATE IS PAST
      * WH -> SL WHEN OPEN DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4310-SET-TRAN.
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
      * SPLIT SHARES AMT AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4320-SPLIT-SHARES-AMT.
           COMPUTE WS-WORK-AMT = DRPT-SHARES-AMT-AMT / 12
           COMPUTE WS-WORK-AMT-2 = DRPT-SHARES-AMT-AMT - (WS-WORK-AMT *
               12)
           COMPUTE DRPM-WITHHOLD-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4330-AGE-NEXT-DUE.
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
                   ADD DRPM-REINVEST-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD DRPM-REINVEST-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD DRPM-REINVEST-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD DRPM-REINVEST-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRPM-REINVEST-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * DV -> RI WHEN EFFECTIVE DATE IS PAST
      * RI -> CP WHEN EFFECTIVE DATE IS PAST
      * CP -> WH WHEN EFFECTIVE DATE IS PAST
      * WH -> SL WHEN EFFECTIVE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4340-SET-TRAN.
           EVALUATE DRPT-TRAN-CD
               WHEN 'DV'
                   IF DRPM-EFFECTIVE-DATE > 0 AND DRPM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RI' TO DRPT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RI'
                   IF DRPM-EFFECTIVE-DATE > 0 AND DRPM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CP' TO DRPT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CP'
                   IF DRPM-EFFECTIVE-DATE > 0 AND DRPM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WH' TO DRPT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WH'
                   IF DRPM-EFFECTIVE-DATE > 0 AND DRPM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SL' TO DRPT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE DIVIDEND FEE ON THE WORK AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       4350-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN DRPM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO DRPM-DIVIDEND-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPM-SUSPEND-FLAG = 'Y'
                   COMPUTE DRPM-DIVIDEND-AMT ROUNDED = WS-WORK-AMT *
                       DRPM-RATE / 200
               WHEN DRPM-CHANNEL-CD = 'RM'
                   MOVE 15.00 TO DRPM-DIVIDEND-AMT
               WHEN DRPM-CHANNEL-CD = 'VP'
                   MOVE 12.00 TO DRPM-DIVIDEND-AMT
               WHEN OTHER
                   COMPUTE DRPM-DIVIDEND-AMT ROUNDED = WS-WORK-AMT *
                       DRPM-RATE / 100
           END-EVALUATE
           IF DRPM-DIVIDEND-AMT > 0 AND DRPM-DIVIDEND-AMT < 2.00
               MOVE 2.00 TO DRPM-DIVIDEND-AMT
           END-IF
           IF DRPM-DIVIDEND-AMT > 250.00
               MOVE 250.00 TO DRPM-DIVIDEND-AMT
           END-IF
           ADD DRPM-DIVIDEND-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT SHARES AMT AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4360-SPLIT-SHARES-AMT.
           COMPUTE WS-WORK-AMT = DRPM-SHARES-AMT-AMT / 12
           COMPUTE WS-WORK-AMT-2 = DRPM-SHARES-AMT-AMT - (WS-WORK-AMT *
               12)
           COMPUTE DRPM-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CASH PAY TIER TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4370-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-PAY-TIER-RATE(1) TO DRPM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF WS-WORK-AMT NOT < WS-CASH-PAY-TIER-LOW(WS-TX)
                   MOVE WS-CASH-PAY-TIER-RATE(WS-TX) TO DRPM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       4380-COUNT-EXCEPT.
           IF DRPM-EXCEPT-CNT < 9999999
               ADD 1 TO DRPM-EXCEPT-CNT
           END-IF
           IF DRPM-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO DRPM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * B -> G WHEN LAST ACT DATE IS PAST
      * G -> J WHEN LAST ACT DATE IS PAST
      * J -> K WHEN LAST ACT DATE IS PAST
      * K -> L WHEN LAST ACT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4390-SET-REASON.
           EVALUATE DRPM-REASON-CD
               WHEN 'B'
                   IF DRPM-LAST-ACT-DATE > 0 AND DRPM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'G' TO DRPM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'G'
                   IF DRPM-LAST-ACT-DATE > 0 AND DRPM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'J' TO DRPM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'J'
                   IF DRPM-LAST-ACT-DATE > 0 AND DRPM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'K' TO DRPM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF DRPM-LAST-ACT-DATE > 0 AND DRPM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO DRPM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4400-COUNT-ITEM.
           IF DRPM-ITEM-CNT < 9999999
               ADD 1 TO DRPM-ITEM-CNT
           END-IF
           IF DRPM-ITEM-CNT NOT < 12
               MOVE 'Y' TO DRPM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4410-DESCRIBE-CHANNEL.
           EVALUATE DRPM-CHANNEL-CD
               WHEN 'RM'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'VP'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'YS'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * REINVEST AMT IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4420-LIMIT-REINVEST.
           IF DRPM-REINVEST-AMT < 0.00
               MOVE 0.00 TO DRPM-REINVEST-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRPM-REINVEST-AMT > 10000.00
               MOVE 10000.00 TO DRPM-REINVEST-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE REINVEST AMT BY PLAN OPTION CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       4430-ACCUM-PLAN-OPTION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPM-PLAN-OPTION-CD
                   ADD DRPM-REINVEST-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPM-PLAN-OPTION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPM-REINVEST-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN LAST ACT DATE IS PAST
      * BAT -> MAL WHEN LAST ACT DATE IS PAST
      * MAL -> TEL WHEN LAST ACT DATE IS PAST
      * TEL -> ONL WHEN LAST ACT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4440-SET-CHANNEL.
           EVALUATE DRPT-CHANNEL-CD
               WHEN 'BRN'
                   IF DRPM-LAST-ACT-DATE > 0 AND DRPM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO DRPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF DRPM-LAST-ACT-DATE > 0 AND DRPM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO DRPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF DRPM-LAST-ACT-DATE > 0 AND DRPM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO DRPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF DRPM-LAST-ACT-DATE > 0 AND DRPM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO DRPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4450-LIMIT-FEE.
           IF DRPM-FEE-AMT < 50.00
               MOVE 50.00 TO DRPM-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRPM-FEE-AMT > 1000.00
               MOVE 1000.00 TO DRPM-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO SHARES AMT YTD.
      *----------------------------------------------------------------*
       4460-PRORATE-SHARES-AMT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRPM-SHARES-AMT-YTD ROUNDED =
                   DRPM-FEE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO DRPM-SHARES-AMT-YTD
           END-IF
           ADD DRPM-SHARES-AMT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE WITHHOLD YTD BY HOLDER TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       4470-ACCUM-HOLDER-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPM-HOLDER-TYPE-CD
                   ADD DRPM-WITHHOLD-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPM-HOLDER-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPM-WITHHOLD-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       4480-COUNT-NOTICE.
           IF DRPM-NOTICE-CNT < 9999999
               ADD 1 TO DRPM-NOTICE-CNT
           END-IF
           IF DRPM-NOTICE-CNT NOT < 3
               MOVE 'Y' TO DRPM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DIVIDEND AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4490-FLAG-DIVIDEND.
           IF DRPM-DIVIDEND-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE DRPM-DIVIDEND-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E088 CONTACT REQUIRED
      * E345 SHARES AMT AMT NEGATIVE
      * E206 SHARES AMT AMT OVER 250000
      * E200 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       4500-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPM-CONTACT = SPACES
                   MOVE 'E088' TO WS-REJECT-CODE
               WHEN DRPT-SHARES-AMT-AMT < 0
                   MOVE 'E345' TO WS-REJECT-CODE
               WHEN DRPT-SHARES-AMT-AMT > 250000.00
                   MOVE 'E206' TO WS-REJECT-CODE
               WHEN DRPT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E200' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4510-COUNT-ITEM.
           IF DRPM-ITEM-CNT < 9999999
               ADD 1 TO DRPM-ITEM-CNT
           END-IF
           IF DRPM-ITEM-CNT NOT < 3
               MOVE 'Y' TO DRPM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4520-COUNT-NOTICE.
           IF DRPM-NOTICE-CNT < 9999999
               ADD 1 TO DRPM-NOTICE-CNT
           END-IF
           IF DRPM-NOTICE-CNT NOT < 6
               MOVE 'Y' TO DRPM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4530-LIMIT-ITEM.
           IF WS-WORK-AMT < 25.00
               MOVE 25.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 50000.00
               MOVE 50000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E642 OPERATOR REQUIRED
      * E101 DIVIDEND AMT NEGATIVE
      * E926 DIVIDEND AMT OVER 50000
      * E836 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       4540-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPT-OPERATOR = SPACES
                   MOVE 'E642' TO WS-REJECT-CODE
               WHEN DRPT-DIVIDEND-AMT < 0
                   MOVE 'E101' TO WS-REJECT-CODE
               WHEN DRPT-DIVIDEND-AMT > 50000.00
                   MOVE 'E926' TO WS-REJECT-CODE
               WHEN DRPM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E836' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * REINVEST YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4550-FLAG-REINVEST.
           IF DRPM-REINVEST-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE DRPM-REINVEST-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

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
                   MOVE DRSTAT-DIVIDEND-AMT
                     TO DRPM-DIVIDEND-AMT
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
                   MOVE DRELG-CASH-PAY-AMT
                     TO DRPM-CASH-PAY-AMT
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
           DISPLAY 'DRPCONV READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'DRPCONV' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
