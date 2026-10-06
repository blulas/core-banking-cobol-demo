      *================================================================*
      * PROGRAM  : XCLYREN                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : CHRISTMAS CLUB SAVINGS - YEAR-END. READS THE XCL    *
      *            TRANSACTIONS, UPDATES THE XCL MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. HOLIDAY CLUB SAVINGS PLANS WITH *
      *            WEEKLY DEPOSITS AND NOVEMBER PAYOUT                 *
      * INPUT    : XCLMST (KSDS), XCLTRN, BANKCTL                      *
      * OUTPUT   : XCLOUT, XCLYRRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, XCFEE, XCLIM, XCTIER    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-05-05 RJT  ORIGINAL                                       *
      * 1998-06-09 SKN  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1998-03-19 PQR  NEW CONTRIB REPORT COLUMNS CHG2449             *
      * 2000-03-18 DPS  AUDIT FINDING - PENALTY CONTROL TOTALS CHG5385 *
      * 2002-05-11 PQR  BONUS RERUN PROTECTION CHG1788                 *
      * 2002-04-28 DPS  GOAL NOTICE TEXT PER LEGAL REQ1438             *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. XCLYREN.
       AUTHOR. R J TRAVERS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "XCLMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS XCLM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "XCLTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "XCLOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "XCLYRRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY XCLMST.
       FD  TRAN-FILE.
       COPY XCLTRN.
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
      * WS-CONTRIB-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CONTRIB-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000787000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000883500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011000000000509500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013500000000711500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018500000000341500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021000000000601000'.
       01  WS-CONTRIB-TIER-TBL REDEFINES WS-CONTRIB-TIER-VALUES.
           05  WS-CONTRIB-TIER-ROW OCCURS 6 TIMES.
               10  WS-CONTRIB-TIER-LOW     PIC 9(09)V99.
               10  WS-CONTRIB-TIER-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-MISSED-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-MISSED-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000767000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000151500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002750000000250000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007750000000462000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007800000000697000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008800000000318000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018800000000776000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023800000000826000'.
       01  WS-MISSED-GRID-TBL REDEFINES WS-MISSED-GRID-VALUES.
           05  WS-MISSED-GRID-ROW OCCURS 8 TIMES.
               10  WS-MISSED-GRID-LOW      PIC 9(09)V99.
               10  WS-MISSED-GRID-RATE     PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-ACCRUED-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-ACCRUED-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000610000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000550000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000350000000453500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000450000000436500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010450000000589500'.
       01  WS-ACCRUED-BAND-TBL REDEFINES WS-ACCRUED-BAND-VALUES.
           05  WS-ACCRUED-BAND-ROW OCCURS 5 TIMES.
               10  WS-ACCRUED-BAND-LOW     PIC 9(09)V99.
               10  WS-ACCRUED-BAND-RATE    PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  XCLYREN  CHRISTMAS CLUB SAVINGS'.
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
       COPY XCFEEP.
       COPY XCLIMP.
       COPY XCTIERP.

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
               MOVE 'XCLMST' TO ABND-FILE-NAME
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
           MOVE XCLT-KEY TO XCLM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-SET-TYPE
               PERFORM 3010-SPLIT-GOAL
               PERFORM 3020-ACCUM-REGION
               PERFORM 3030-AGE-EXPIRY
               PERFORM 3040-ASSESS-SUSPEND
               PERFORM 3050-LIMIT-BONUS
               PERFORM 3060-ASSESS-STOP
               PERFORM 3070-SET-REASON
               PERFORM 3080-DESCRIBE-REASON
               PERFORM 3090-CHECK-NOTICE
               PERFORM 3100-LIMIT-BONUS
               PERFORM 3110-COUNT-SEQ
               PERFORM 3120-ACCUM-REGION
               PERFORM 3130-PRORATE-CONTRIB
               PERFORM 3140-FLAG-WEEKLY
               PERFORM 3150-SELECT-RATE
               PERFORM 3160-PRORATE-PENALTY
               PERFORM 3170-AGE-NEXT-REVIEW
               PERFORM 3180-EDIT-OPERATOR
               PERFORM 3190-CHECK-POST
               PERFORM 3200-AGE-LAST-ACT
               PERFORM 3210-ASSESS-PREFERRED
               PERFORM 3220-FLAG-MISSED
               PERFORM 3230-SET-REASON
               PERFORM 3240-SET-TYPE
               PERFORM 3250-SELECT-DISCOUNT
               PERFORM 3260-PRORATE-ACCRUED
               PERFORM 3270-FLAG-CONTRIB
               PERFORM 3280-COUNT-CYCLE
               PERFORM 3290-SELECT-DISCOUNT
               PERFORM 3300-EDIT-OPERATOR
               PERFORM 3310-COUNT-PAYMENT
               PERFORM 3320-SET-CYCLE
               PERFORM 3330-CHECK-EXPIRY
               PERFORM 3340-EDIT-NAME
               PERFORM 3350-PRORATE-BONUS
               PERFORM 3360-LIMIT-PAYOUT
               PERFORM 3370-SET-REASON
               PERFORM 3380-EDIT-CUST-ID
               PERFORM 3390-ACCRUE-GOAL
               PERFORM 3400-SPLIT-ACCRUED
               PERFORM 3410-CHECK-MATURITY
               PERFORM 3420-EDIT-NAME
               PERFORM 3430-ACCRUE-CONTRIB
               PERFORM 3440-ACCUM-CHANNEL
               PERFORM 3450-CHECK-POST
               PERFORM 3460-COUNT-NOTICE
               PERFORM 3470-EDIT-DESC
               PERFORM 3480-LIMIT-GOAL
               PERFORM 3490-CHECK-NEXT-REVIEW
               PERFORM 3500-ACCRUE-GOAL
               PERFORM 3510-LIMIT-CONTRIB
               PERFORM 3520-FLAG-BONUS
               PERFORM 3530-PRORATE-BONUS
               PERFORM 3540-SET-CHANNEL
               PERFORM 3550-CHECK-POST
               PERFORM 3560-FLAG-GOAL
               PERFORM 3570-ASSESS-PREFERRED
               PERFORM 3580-SELECT-DISCOUNT
               PERFORM 3590-SET-PAYOUT-METHOD
               PERFORM 3600-FLAG-PAYOUT
               PERFORM 3610-PRORATE-CONTRIB
               PERFORM 3620-SPLIT-GOAL
               PERFORM 3630-SET-TYPE
               PERFORM 3640-ASSESS-STOP
               PERFORM 3650-ACCRUE-BONUS
               PERFORM 3660-CHECK-POST
               PERFORM 3670-SPLIT-PAYOUT
               PERFORM 3680-CHECK-OPEN
               PERFORM 3690-PRORATE-GOAL
               PERFORM 3700-ASSESS-PREFERRED
               PERFORM 3710-SELECT-DISCOUNT
               PERFORM 3720-PRORATE-PAYOUT
               PERFORM 3730-ACCRUE-CONTRIB
               PERFORM 3740-ACCRUE-PENALTY
               PERFORM 3750-SPLIT-PENALTY
               PERFORM 3760-SET-PAYOUT-METHOD
               PERFORM 3770-ACCUM-REGION
               PERFORM 3780-ACCRUE-ACCRUED
               PERFORM 3790-COUNT-RETRY
               PERFORM 3800-SPLIT-WEEKLY
               PERFORM 3810-SELECT-FEE-PCT
               PERFORM 3820-FLAG-GOAL
               PERFORM 3830-COUNT-NOTICE
               PERFORM 3840-ASSESS-STOP
               PERFORM 3850-SET-CHANNEL
               PERFORM 3860-ACCUM-CHANNEL
               PERFORM 3870-ACCRUE-CONTRIB
               PERFORM 3880-CHECK-EFF
               PERFORM 3890-ACCUM-TRAN
               PERFORM 3900-CHECK-OPEN
               PERFORM 3910-ACCUM-REASON
               PERFORM 3920-EDIT-CONTACT
               PERFORM 3930-SPLIT-PAYOUT
               PERFORM 3940-ASSESS-SUSPEND
               PERFORM 3950-SELECT-FEE-PCT
               PERFORM 3960-SPLIT-BONUS
               PERFORM 3970-SPLIT-PAYOUT
               PERFORM 3980-PRORATE-BONUS
               PERFORM 3990-FLAG-GOAL
               PERFORM 4000-FLAG-CONTRIB
               PERFORM 4010-SPLIT-WEEKLY
               PERFORM 4020-DESCRIBE-REASON
               PERFORM 4030-CHECK-NEXT-REVIEW
               PERFORM 4040-AGE-NEXT-DUE
               PERFORM 4050-AGE-NEXT-REVIEW
               PERFORM 4060-SELECT-FEE-PCT
               PERFORM 4070-FLAG-ITEM
               PERFORM 4080-PRORATE-MISSED
               PERFORM 4090-ACCUM-TRAN
               PERFORM 4100-DESCRIBE-TRAN
               PERFORM 4110-COUNT-NOTICE
               PERFORM 4120-PRORATE-CONTRIB
               PERFORM 4130-AGE-NEXT-DUE
               PERFORM 4140-ACCRUE-WEEKLY
               PERFORM 4150-ACCRUE-ACCRUED
               PERFORM 4160-EDIT-ADDR
               PERFORM 4170-LIMIT-PENALTY
               PERFORM 4180-SET-TRAN
               PERFORM 4190-CHECK-EXPIRY
               PERFORM 4200-EDIT-REF-NO
               PERFORM 4210-AGE-NEXT-REVIEW
               PERFORM 4220-ACCRUE-MISSED
               PERFORM 4230-LIMIT-BONUS
               PERFORM 4240-LIMIT-PENALTY
               PERFORM 4250-AGE-LAST-PAY
               PERFORM 4260-SET-REGION
               PERFORM 4270-ASSESS-OVERRIDE
               PERFORM 4280-SPLIT-PAYOUT
               PERFORM 4290-ACCUM-TYPE
               PERFORM 4300-SELECT-DISCOUNT
               PERFORM 4310-ACCRUE-BONUS
               PERFORM 4320-CHECK-POST
               PERFORM 4330-SELECT-FEE-PCT
               PERFORM 4340-AGE-LAST-PAY
               PERFORM 4350-ACCRUE-CONTRIB
               PERFORM 7020-CALL-XCFEE
               PERFORM 7010-CALL-XCLIM
               PERFORM 7000-CALL-XCTIER
               REWRITE XCL-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'XCLMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM XCL-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * A -> B WHEN LAST PAY DATE IS PAST
      * B -> D WHEN LAST PAY DATE IS PAST
      * D -> E WHEN LAST PAY DATE IS PAST
      * E -> T WHEN LAST PAY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3000-SET-TYPE.
           EVALUATE XCLM-TYPE-CD
               WHEN 'A'
                   IF XCLM-LAST-PAY-DATE > 0 AND XCLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'B' TO XCLM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'B'
                   IF XCLM-LAST-PAY-DATE > 0 AND XCLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO XCLM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF XCLM-LAST-PAY-DATE > 0 AND XCLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'E' TO XCLM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'E'
                   IF XCLM-LAST-PAY-DATE > 0 AND XCLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO XCLM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT GOAL AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3010-SPLIT-GOAL.
           COMPUTE WS-WORK-AMT = XCLT-GOAL-AMT / 3
           COMPUTE WS-WORK-AMT-2 = XCLT-GOAL-AMT - (WS-WORK-AMT * 3)
           COMPUTE XCLT-BONUS-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WEEKLY YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3020-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCLM-REGION-CD
                   ADD XCLM-WEEKLY-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCLM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCLM-WEEKLY-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3030-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF XCLM-EXPIRY-DATE > 0 AND XCLM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCLM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD XCLM-CONTRIB-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD XCLM-CONTRIB-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD XCLM-CONTRIB-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD XCLM-CONTRIB-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCLM-CONTRIB-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE CONTRIB YTD FEE ON THE PENALTY YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3040-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN XCLM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO XCLM-CONTRIB-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-EXEMPT-FLAG = 'Y'
                   COMPUTE XCLM-CONTRIB-YTD ROUNDED = XCLM-PENALTY-YTD *
                       XCLM-RATE / 200
               WHEN XCLM-REGION-CD = 'FLM'
                   MOVE 15.00 TO XCLM-CONTRIB-YTD
               WHEN XCLM-REGION-CD = 'HLS'
                   MOVE 10.00 TO XCLM-CONTRIB-YTD
               WHEN OTHER
                   COMPUTE XCLM-CONTRIB-YTD ROUNDED = XCLM-PENALTY-YTD *
                       XCLM-RATE / 100
           END-EVALUATE
           IF XCLM-CONTRIB-YTD > 0 AND XCLM-CONTRIB-YTD < 10.00
               MOVE 10.00 TO XCLM-CONTRIB-YTD
           END-IF
           IF XCLM-CONTRIB-YTD > 75.00
               MOVE 75.00 TO XCLM-CONTRIB-YTD
           END-IF
           ADD XCLM-CONTRIB-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * BONUS AMT IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3050-LIMIT-BONUS.
           IF XCLT-BONUS-AMT < 100.00
               MOVE 100.00 TO XCLT-BONUS-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCLT-BONUS-AMT > 10000.00
               MOVE 10000.00 TO XCLT-BONUS-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE CONTRIB YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3060-ASSESS-STOP.
           EVALUATE TRUE
               WHEN XCLM-STOP-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLT-REVERSAL-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = XCLM-CONTRIB-YTD *
                       XCLM-FEE-PCT / 200
               WHEN XCLM-PLAN-CD = 'GN'
                   MOVE 7.00 TO WS-WORK-AMT
               WHEN XCLM-PLAN-CD = 'JT'
                   MOVE 20.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = XCLM-CONTRIB-YTD *
                       XCLM-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 2.00
               MOVE 2.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 250.00
               MOVE 250.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * AL -> BT WHEN MATURITY DATE IS PAST
      * BT -> CY WHEN MATURITY DATE IS PAST
      * CY -> KM WHEN MATURITY DATE IS PAST
      * KM -> MV WHEN MATURITY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3070-SET-REASON.
           EVALUATE XCLM-REASON-CD
               WHEN 'AL'
                   IF XCLM-MATURITY-DATE > 0 AND XCLM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BT' TO XCLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BT'
                   IF XCLM-MATURITY-DATE > 0 AND XCLM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CY' TO XCLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CY'
                   IF XCLM-MATURITY-DATE > 0 AND XCLM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KM' TO XCLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KM'
                   IF XCLM-MATURITY-DATE > 0 AND XCLM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MV' TO XCLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3080-DESCRIBE-REASON.
           EVALUATE XCLM-REASON-CD
               WHEN 'AL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'BT'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'CY'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'KM'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'MV'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'XK'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'ZW'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3090-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF XCLM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCLM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO XCLM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * BONUS AMT IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3100-LIMIT-BONUS.
           IF XCLT-BONUS-AMT < 10.00
               MOVE 10.00 TO XCLT-BONUS-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCLT-BONUS-AMT > 500.00
               MOVE 500.00 TO XCLT-BONUS-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3110-COUNT-SEQ.
           IF XCLT-SEQ < 9999999
               ADD 1 TO XCLT-SEQ
           END-IF
           IF XCLT-SEQ NOT < 10
               MOVE 'Y' TO XCLM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CONTRIB YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3120-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCLM-REGION-CD
                   ADD XCLM-CONTRIB-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCLM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCLM-CONTRIB-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PENALTY AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CONTRIB AMT.
      *----------------------------------------------------------------*
       3130-PRORATE-CONTRIB.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLT-CONTRIB-AMT ROUNDED =
                   XCLM-PENALTY-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO XCLT-CONTRIB-AMT
           END-IF
           ADD XCLT-CONTRIB-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * WEEKLY YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3140-FLAG-WEEKLY.
           IF XCLM-WEEKLY-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE XCLM-WEEKLY-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ACCRUED BAND TIER FOR THE PAYOUT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3150-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-ACCRUED-BAND-RATE(1) TO XCLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF XCLM-PAYOUT-AMT NOT < WS-ACCRUED-BAND-LOW(WS-TX)
                   MOVE WS-ACCRUED-BAND-RATE(WS-TX) TO XCLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE BONUS YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PENALTY YTD.
      *----------------------------------------------------------------*
       3160-PRORATE-PENALTY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLM-PENALTY-YTD ROUNDED =
                   XCLM-BONUS-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO XCLM-PENALTY-YTD
           END-IF
           ADD XCLM-PENALTY-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3170-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF XCLM-NEXT-REVIEW-DATE > 0 AND XCLM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCLM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD XCLM-CONTRIB-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD XCLM-CONTRIB-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD XCLM-CONTRIB-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD XCLM-CONTRIB-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCLM-CONTRIB-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E038 OPERATOR REQUIRED
      * E585 BONUS AMT NEGATIVE
      * E431 BONUS AMT OVER 5000
      * E439 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3180-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLT-OPERATOR = SPACES
                   MOVE 'E038' TO WS-REJECT-CODE
               WHEN XCLT-BONUS-AMT < 0
                   MOVE 'E585' TO WS-REJECT-CODE
               WHEN XCLT-BONUS-AMT > 5000.00
                   MOVE 'E431' TO WS-REJECT-CODE
               WHEN XCLT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E439' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 365 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3190-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF XCLT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCLT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO XCLM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3200-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF XCLM-LAST-ACT-DATE > 0 AND XCLM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCLM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD XCLM-CONTRIB-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD XCLM-CONTRIB-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD XCLM-CONTRIB-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD XCLM-CONTRIB-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCLM-CONTRIB-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WEEKLY YTD FEE ON THE BONUS YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3210-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN XCLM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO XCLM-WEEKLY-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLT-REVERSAL-FLAG = 'Y'
                   COMPUTE XCLM-WEEKLY-YTD ROUNDED = XCLM-BONUS-YTD *
                       XCLM-DISCOUNT / 200
               WHEN XCLM-PAYOUT-METHOD-CD = 'B'
                   MOVE 7.00 TO XCLM-WEEKLY-YTD
               WHEN XCLM-PAYOUT-METHOD-CD = 'H'
                   MOVE 12.00 TO XCLM-WEEKLY-YTD
               WHEN OTHER
                   COMPUTE XCLM-WEEKLY-YTD ROUNDED = XCLM-BONUS-YTD *
                       XCLM-DISCOUNT / 100
           END-EVALUATE
           IF XCLM-WEEKLY-YTD > 0 AND XCLM-WEEKLY-YTD < 1.00
               MOVE 1.00 TO XCLM-WEEKLY-YTD
           END-IF
           IF XCLM-WEEKLY-YTD > 25.00
               MOVE 25.00 TO XCLM-WEEKLY-YTD
           END-IF
           ADD XCLM-WEEKLY-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * MISSED YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3220-FLAG-MISSED.
           IF XCLM-MISSED-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE XCLM-MISSED-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * AL -> BT WHEN NOTICE DATE IS PAST
      * BT -> CY WHEN NOTICE DATE IS PAST
      * CY -> KM WHEN NOTICE DATE IS PAST
      * KM -> MV WHEN NOTICE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3230-SET-REASON.
           EVALUATE XCLM-REASON-CD
               WHEN 'AL'
                   IF XCLM-NOTICE-DATE > 0 AND XCLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BT' TO XCLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BT'
                   IF XCLM-NOTICE-DATE > 0 AND XCLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CY' TO XCLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CY'
                   IF XCLM-NOTICE-DATE > 0 AND XCLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KM' TO XCLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KM'
                   IF XCLM-NOTICE-DATE > 0 AND XCLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MV' TO XCLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * A -> B WHEN EFF DATE IS PAST
      * B -> D WHEN EFF DATE IS PAST
      * D -> E WHEN EFF DATE IS PAST
      * E -> T WHEN EFF DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3240-SET-TYPE.
           EVALUATE XCLM-TYPE-CD
               WHEN 'A'
                   IF XCLT-EFF-DATE > 0 AND XCLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'B' TO XCLM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'B'
                   IF XCLT-EFF-DATE > 0 AND XCLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO XCLM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF XCLT-EFF-DATE > 0 AND XCLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'E' TO XCLM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'E'
                   IF XCLT-EFF-DATE > 0 AND XCLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO XCLM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE CONTRIB TIER TIER FOR THE CONTRIB AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3250-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CONTRIB-TIER-RATE(1) TO XCLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF XCLM-CONTRIB-AMT NOT < WS-CONTRIB-TIER-LOW(WS-TX)
                   MOVE WS-CONTRIB-TIER-RATE(WS-TX) TO XCLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ACCRUED YTD.
      *----------------------------------------------------------------*
       3260-PRORATE-ACCRUED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLM-ACCRUED-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO XCLM-ACCRUED-YTD
           END-IF
           ADD XCLM-ACCRUED-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * CONTRIB AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3270-FLAG-CONTRIB.
           IF XCLT-CONTRIB-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE XCLT-CONTRIB-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3280-COUNT-CYCLE.
           IF XCLM-CYCLE-CNT < 9999999
               ADD 1 TO XCLM-CYCLE-CNT
           END-IF
           IF XCLM-CYCLE-CNT NOT < 12
               MOVE 'Y' TO XCLM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ACCRUED BAND TIER FOR THE GOAL YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3290-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-ACCRUED-BAND-RATE(1) TO XCLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF XCLM-GOAL-YTD NOT < WS-ACCRUED-BAND-LOW(WS-TX)
                   MOVE WS-ACCRUED-BAND-RATE(WS-TX) TO XCLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E676 OPERATOR REQUIRED
      * E979 BONUS AMT NEGATIVE
      * E281 BONUS AMT OVER 10000
      * E103 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3300-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLT-OPERATOR = SPACES
                   MOVE 'E676' TO WS-REJECT-CODE
               WHEN XCLM-BONUS-AMT < 0
                   MOVE 'E979' TO WS-REJECT-CODE
               WHEN XCLM-BONUS-AMT > 10000.00
                   MOVE 'E281' TO WS-REJECT-CODE
               WHEN XCLM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E103' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3310-COUNT-PAYMENT.
           IF XCLM-PAYMENT-CNT < 9999999
               ADD 1 TO XCLM-PAYMENT-CNT
           END-IF
           IF XCLM-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO XCLM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * EM -> FE WHEN EFF DATE IS PAST
      * FE -> PL WHEN EFF DATE IS PAST
      * PL -> RU WHEN EFF DATE IS PAST
      * RU -> SU WHEN EFF DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3320-SET-CYCLE.
           EVALUATE XCLM-CYCLE-CD
               WHEN 'EM'
                   IF XCLT-EFF-DATE > 0 AND XCLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FE' TO XCLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FE'
                   IF XCLT-EFF-DATE > 0 AND XCLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PL' TO XCLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PL'
                   IF XCLT-EFF-DATE > 0 AND XCLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RU' TO XCLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RU'
                   IF XCLT-EFF-DATE > 0 AND XCLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SU' TO XCLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3330-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF XCLM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCLM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO XCLT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E085 NAME REQUIRED
      * E288 PAYOUT AMT NEGATIVE
      * E849 PAYOUT AMT OVER 1000000
      * E991 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3340-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLM-NAME = SPACES
                   MOVE 'E085' TO WS-REJECT-CODE
               WHEN XCLM-PAYOUT-AMT < 0
                   MOVE 'E288' TO WS-REJECT-CODE
               WHEN XCLM-PAYOUT-AMT > 1000000.00
                   MOVE 'E849' TO WS-REJECT-CODE
               WHEN XCLM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E991' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE BONUS YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3350-PRORATE-BONUS.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   XCLM-BONUS-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PAYOUT YTD IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3360-LIMIT-PAYOUT.
           IF XCLM-PAYOUT-YTD < 0.00
               MOVE 0.00 TO XCLM-PAYOUT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCLM-PAYOUT-YTD > 5000.00
               MOVE 5000.00 TO XCLM-PAYOUT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * AL -> BT WHEN EXPIRY DATE IS PAST
      * BT -> CY WHEN EXPIRY DATE IS PAST
      * CY -> KM WHEN EXPIRY DATE IS PAST
      * KM -> MV WHEN EXPIRY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3370-SET-REASON.
           EVALUATE XCLM-REASON-CD
               WHEN 'AL'
                   IF XCLM-EXPIRY-DATE > 0 AND XCLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BT' TO XCLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BT'
                   IF XCLM-EXPIRY-DATE > 0 AND XCLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CY' TO XCLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CY'
                   IF XCLM-EXPIRY-DATE > 0 AND XCLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KM' TO XCLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KM'
                   IF XCLM-EXPIRY-DATE > 0 AND XCLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MV' TO XCLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E076 CUST ID REQUIRED
      * E471 CONTRIB AMT NEGATIVE
      * E371 CONTRIB AMT OVER 25000
      * E322 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3380-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLM-CUST-ID = SPACES
                   MOVE 'E076' TO WS-REJECT-CODE
               WHEN XCLT-CONTRIB-AMT < 0
                   MOVE 'E471' TO WS-REJECT-CODE
               WHEN XCLT-CONTRIB-AMT > 25000.00
                   MOVE 'E371' TO WS-REJECT-CODE
               WHEN XCLM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E322' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON GOAL AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3390-ACCRUE-GOAL.
           IF XCLM-GOAL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLM-GOAL-AMT * XCLM-RATE / 100 / 365
               ADD WS-WORK-AMT TO XCLT-BONUS-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ACCRUED AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3400-SPLIT-ACCRUED.
           COMPUTE WS-WORK-AMT = XCLM-ACCRUED-AMT / 2
           COMPUTE WS-WORK-AMT-2 = XCLM-ACCRUED-AMT - (WS-WORK-AMT * 2)
           COMPUTE XCLT-WEEKLY-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 365 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3410-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF XCLM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCLM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO XCLM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E423 NAME REQUIRED
      * E942 BONUS YTD NEGATIVE
      * E335 BONUS YTD OVER 25000
      * E965 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3420-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLM-NAME = SPACES
                   MOVE 'E423' TO WS-REJECT-CODE
               WHEN XCLM-BONUS-YTD < 0
                   MOVE 'E942' TO WS-REJECT-CODE
               WHEN XCLM-BONUS-YTD > 25000.00
                   MOVE 'E335' TO WS-REJECT-CODE
               WHEN XCLM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E965' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CONTRIB AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3430-ACCRUE-CONTRIB.
           IF XCLM-CONTRIB-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLM-CONTRIB-AMT * XCLM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO XCLM-PENALTY-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WEEKLY AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3440-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCLT-CHANNEL-CD
                   ADD XCLT-WEEKLY-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCLT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCLT-WEEKLY-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 15 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3450-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF XCLT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCLT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO XCLM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3460-COUNT-NOTICE.
           IF XCLM-NOTICE-CNT < 9999999
               ADD 1 TO XCLM-NOTICE-CNT
           END-IF
           IF XCLM-NOTICE-CNT NOT < 3
               MOVE 'Y' TO XCLM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E464 DESC REQUIRED
      * E302 GOAL YTD NEGATIVE
      * E889 GOAL YTD OVER 25000
      * E247 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3470-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLM-DESC = SPACES
                   MOVE 'E464' TO WS-REJECT-CODE
               WHEN XCLM-GOAL-YTD < 0
                   MOVE 'E302' TO WS-REJECT-CODE
               WHEN XCLM-GOAL-YTD > 25000.00
                   MOVE 'E889' TO WS-REJECT-CODE
               WHEN XCLM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E247' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * GOAL AMT IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3480-LIMIT-GOAL.
           IF XCLT-GOAL-AMT < 100.00
               MOVE 100.00 TO XCLT-GOAL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCLT-GOAL-AMT > 500.00
               MOVE 500.00 TO XCLT-GOAL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 60 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3490-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF XCLM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCLM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO XCLM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON GOAL AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3500-ACCRUE-GOAL.
           IF XCLM-GOAL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLM-GOAL-AMT * XCLM-RATE / 100 / 365
               ADD WS-WORK-AMT TO XCLM-PAYOUT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CONTRIB AMT IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3510-LIMIT-CONTRIB.
           IF XCLT-CONTRIB-AMT < 50.00
               MOVE 50.00 TO XCLT-CONTRIB-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCLT-CONTRIB-AMT > 1000.00
               MOVE 1000.00 TO XCLT-CONTRIB-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * BONUS AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3520-FLAG-BONUS.
           IF XCLT-BONUS-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE XCLT-BONUS-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CONTRIB YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO BONUS AMT.
      *----------------------------------------------------------------*
       3530-PRORATE-BONUS.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLM-BONUS-AMT ROUNDED =
                   XCLM-CONTRIB-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO XCLM-BONUS-AMT
           END-IF
           ADD XCLM-BONUS-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN LAST PAY DATE IS PAST
      * BAT -> MAL WHEN LAST PAY DATE IS PAST
      * MAL -> TEL WHEN LAST PAY DATE IS PAST
      * TEL -> ONL WHEN LAST PAY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3540-SET-CHANNEL.
           EVALUATE XCLT-CHANNEL-CD
               WHEN 'BRN'
                   IF XCLM-LAST-PAY-DATE > 0 AND XCLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO XCLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF XCLM-LAST-PAY-DATE > 0 AND XCLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO XCLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF XCLM-LAST-PAY-DATE > 0 AND XCLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO XCLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF XCLM-LAST-PAY-DATE > 0 AND XCLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO XCLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 90 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3550-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF XCLT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCLT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO XCLM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * GOAL AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3560-FLAG-GOAL.
           IF XCLT-GOAL-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE XCLT-GOAL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE BONUS YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3570-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN XCLM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO XCLM-BONUS-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-SUSPEND-FLAG = 'Y'
                   COMPUTE XCLM-BONUS-YTD ROUNDED = WS-WORK-AMT-2 *
                       XCLM-FEE-PCT / 200
               WHEN XCLM-REGION-CD = 'FLM'
                   MOVE 7.00 TO XCLM-BONUS-YTD
               WHEN XCLM-REGION-CD = 'HLS'
                   MOVE 5.00 TO XCLM-BONUS-YTD
               WHEN OTHER
                   COMPUTE XCLM-BONUS-YTD ROUNDED = WS-WORK-AMT-2 *
                       XCLM-FEE-PCT / 100
           END-EVALUATE
           IF XCLM-BONUS-YTD > 0 AND XCLM-BONUS-YTD < 1.00
               MOVE 1.00 TO XCLM-BONUS-YTD
           END-IF
           IF XCLM-BONUS-YTD > 250.00
               MOVE 250.00 TO XCLM-BONUS-YTD
           END-IF
           ADD XCLM-BONUS-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE CONTRIB TIER TIER FOR THE ACCRUED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3580-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CONTRIB-TIER-RATE(1) TO XCLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF XCLM-ACCRUED-AMT NOT < WS-CONTRIB-TIER-LOW(WS-TX)
                   MOVE WS-CONTRIB-TIER-RATE(WS-TX) TO XCLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PAYOUT METHOD CD:
      * B -> H WHEN LAST STMT DATE IS PAST
      * H -> U WHEN LAST STMT DATE IS PAST
      * U -> W WHEN LAST STMT DATE IS PAST
      * W -> B WHEN LAST STMT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3590-SET-PAYOUT-METHOD.
           EVALUATE XCLM-PAYOUT-METHOD-CD
               WHEN 'B'
                   IF XCLM-LAST-STMT-DATE > 0 AND XCLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO XCLM-PAYOUT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF XCLM-LAST-STMT-DATE > 0 AND XCLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO XCLM-PAYOUT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF XCLM-LAST-STMT-DATE > 0 AND XCLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO XCLM-PAYOUT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'W'
                   IF XCLM-LAST-STMT-DATE > 0 AND XCLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'B' TO XCLM-PAYOUT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYOUT YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3600-FLAG-PAYOUT.
           IF XCLM-PAYOUT-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE XCLM-PAYOUT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WEEKLY AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CONTRIB AMT.
      *----------------------------------------------------------------*
       3610-PRORATE-CONTRIB.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLT-CONTRIB-AMT ROUNDED =
                   XCLT-WEEKLY-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO XCLT-CONTRIB-AMT
           END-IF
           ADD XCLT-CONTRIB-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT GOAL YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3620-SPLIT-GOAL.
           COMPUTE WS-WORK-AMT = XCLM-GOAL-YTD / 4
           COMPUTE WS-WORK-AMT-2 = XCLM-GOAL-YTD - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * A -> B WHEN NOTICE DATE IS PAST
      * B -> D WHEN NOTICE DATE IS PAST
      * D -> E WHEN NOTICE DATE IS PAST
      * E -> T WHEN NOTICE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3630-SET-TYPE.
           EVALUATE XCLM-TYPE-CD
               WHEN 'A'
                   IF XCLM-NOTICE-DATE > 0 AND XCLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'B' TO XCLM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'B'
                   IF XCLM-NOTICE-DATE > 0 AND XCLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO XCLM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF XCLM-NOTICE-DATE > 0 AND XCLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'E' TO XCLM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'E'
                   IF XCLM-NOTICE-DATE > 0 AND XCLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO XCLM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE PAYOUT YTD FEE ON THE ACCRUED YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3640-ASSESS-STOP.
           EVALUATE TRUE
               WHEN XCLM-STOP-FLAG = 'Y'
                   MOVE ZERO TO XCLM-PAYOUT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLT-REVERSAL-FLAG = 'Y'
                   COMPUTE XCLM-PAYOUT-YTD ROUNDED = XCLM-ACCRUED-YTD *
                       XCLM-RATE / 200
               WHEN XCLM-TYPE-CD = 'A'
                   MOVE 20.00 TO XCLM-PAYOUT-YTD
               WHEN XCLM-TYPE-CD = 'B'
                   MOVE 15.00 TO XCLM-PAYOUT-YTD
               WHEN OTHER
                   COMPUTE XCLM-PAYOUT-YTD ROUNDED = XCLM-ACCRUED-YTD *
                       XCLM-RATE / 100
           END-EVALUATE
           IF XCLM-PAYOUT-YTD > 0 AND XCLM-PAYOUT-YTD < 5.00
               MOVE 5.00 TO XCLM-PAYOUT-YTD
           END-IF
           IF XCLM-PAYOUT-YTD > 35.00
               MOVE 35.00 TO XCLM-PAYOUT-YTD
           END-IF
           ADD XCLM-PAYOUT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON BONUS YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3650-ACCRUE-BONUS.
           IF XCLM-BONUS-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLM-BONUS-YTD * XCLM-RATE / 100 / 365
               ADD WS-WORK-AMT TO XCLM-ACCRUED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 90 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3660-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF XCLT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCLT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO XCLT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PAYOUT AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3670-SPLIT-PAYOUT.
           COMPUTE WS-WORK-AMT = XCLM-PAYOUT-AMT / 2
           COMPUTE WS-WORK-AMT-2 = XCLM-PAYOUT-AMT - (WS-WORK-AMT * 2)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 180 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3680-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF XCLM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCLM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO XCLM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE GOAL YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO GOAL AMT.
      *----------------------------------------------------------------*
       3690-PRORATE-GOAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLM-GOAL-AMT ROUNDED =
                   XCLM-GOAL-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO XCLM-GOAL-AMT
           END-IF
           ADD XCLM-GOAL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE PAYOUT YTD FEE ON THE BONUS AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3700-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN XCLM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO XCLM-PAYOUT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLT-REVERSAL-FLAG = 'Y'
                   COMPUTE XCLM-PAYOUT-YTD ROUNDED = XCLM-BONUS-AMT *
                       XCLM-RATE / 200
               WHEN XCLT-TRAN-CD = 'WD'
                   MOVE 5.00 TO XCLM-PAYOUT-YTD
               WHEN XCLT-TRAN-CD = 'DP'
                   MOVE 7.00 TO XCLM-PAYOUT-YTD
               WHEN OTHER
                   COMPUTE XCLM-PAYOUT-YTD ROUNDED = XCLM-BONUS-AMT *
                       XCLM-RATE / 100
           END-EVALUATE
           IF XCLM-PAYOUT-YTD > 0 AND XCLM-PAYOUT-YTD < 10.00
               MOVE 10.00 TO XCLM-PAYOUT-YTD
           END-IF
           IF XCLM-PAYOUT-YTD > 150.00
               MOVE 150.00 TO XCLM-PAYOUT-YTD
           END-IF
           ADD XCLM-PAYOUT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE MISSED GRID TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3710-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-MISSED-GRID-RATE(1) TO XCLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF WS-WORK-AMT NOT < WS-MISSED-GRID-LOW(WS-TX)
                   MOVE WS-MISSED-GRID-RATE(WS-TX) TO XCLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ACCRUED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAYOUT YTD.
      *----------------------------------------------------------------*
       3720-PRORATE-PAYOUT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLM-PAYOUT-YTD ROUNDED =
                   XCLM-ACCRUED-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO XCLM-PAYOUT-YTD
           END-IF
           ADD XCLM-PAYOUT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CONTRIB YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3730-ACCRUE-CONTRIB.
           IF XCLM-CONTRIB-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLM-CONTRIB-YTD * XCLM-RATE / 100 / 360
               ADD WS-WORK-AMT TO XCLM-MISSED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PENALTY AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3740-ACCRUE-PENALTY.
           IF XCLM-PENALTY-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLM-PENALTY-AMT * XCLM-RATE / 100 / 360
               ADD WS-WORK-AMT TO XCLM-PAYOUT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PENALTY AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3750-SPLIT-PENALTY.
           COMPUTE WS-WORK-AMT = XCLM-PENALTY-AMT / 12
           COMPUTE WS-WORK-AMT-2 = XCLM-PENALTY-AMT - (WS-WORK-AMT * 12)
           COMPUTE XCLM-MISSED-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PAYOUT METHOD CD:
      * B -> H WHEN OPEN DATE IS PAST
      * H -> U WHEN OPEN DATE IS PAST
      * U -> W WHEN OPEN DATE IS PAST
      * W -> B WHEN OPEN DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3760-SET-PAYOUT-METHOD.
           EVALUATE XCLM-PAYOUT-METHOD-CD
               WHEN 'B'
                   IF XCLM-OPEN-DATE > 0 AND XCLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO XCLM-PAYOUT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF XCLM-OPEN-DATE > 0 AND XCLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO XCLM-PAYOUT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF XCLM-OPEN-DATE > 0 AND XCLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO XCLM-PAYOUT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'W'
                   IF XCLM-OPEN-DATE > 0 AND XCLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'B' TO XCLM-PAYOUT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE ACCRUED AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3770-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCLM-REGION-CD
                   ADD XCLM-ACCRUED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCLM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCLM-ACCRUED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ACCRUED YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3780-ACCRUE-ACCRUED.
           IF XCLM-ACCRUED-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLM-ACCRUED-YTD * XCLM-RATE / 100 / 365
               ADD WS-WORK-AMT TO XCLM-WEEKLY-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3790-COUNT-RETRY.
           IF XCLM-RETRY-CNT < 9999999
               ADD 1 TO XCLM-RETRY-CNT
           END-IF
           IF XCLM-RETRY-CNT NOT < 3
               MOVE 'Y' TO XCLM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WEEKLY AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3800-SPLIT-WEEKLY.
           COMPUTE WS-WORK-AMT = XCLM-WEEKLY-AMT / 2
           COMPUTE WS-WORK-AMT-2 = XCLM-WEEKLY-AMT - (WS-WORK-AMT * 2)
           COMPUTE XCLM-PAYOUT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE MISSED GRID TIER FOR THE ACCRUED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3810-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-MISSED-GRID-RATE(1) TO XCLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF XCLM-ACCRUED-YTD NOT < WS-MISSED-GRID-LOW(WS-TX)
                   MOVE WS-MISSED-GRID-RATE(WS-TX) TO XCLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * GOAL AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3820-FLAG-GOAL.
           IF XCLM-GOAL-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE XCLM-GOAL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3830-COUNT-NOTICE.
           IF XCLM-NOTICE-CNT < 9999999
               ADD 1 TO XCLM-NOTICE-CNT
           END-IF
           IF XCLM-NOTICE-CNT NOT < 5
               MOVE 'Y' TO XCLM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ACCRUED YTD FEE ON THE MISSED AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3840-ASSESS-STOP.
           EVALUATE TRUE
               WHEN XCLM-STOP-FLAG = 'Y'
                   MOVE ZERO TO XCLM-ACCRUED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-SUSPEND-FLAG = 'Y'
                   COMPUTE XCLM-ACCRUED-YTD ROUNDED = XCLM-MISSED-AMT *
                       XCLM-RATE / 200
               WHEN XCLM-PLAN-CD = 'GN'
                   MOVE 10.00 TO XCLM-ACCRUED-YTD
               WHEN XCLM-PLAN-CD = 'JT'
                   MOVE 12.00 TO XCLM-ACCRUED-YTD
               WHEN OTHER
                   COMPUTE XCLM-ACCRUED-YTD ROUNDED = XCLM-MISSED-AMT *
                       XCLM-RATE / 100
           END-EVALUATE
           IF XCLM-ACCRUED-YTD > 0 AND XCLM-ACCRUED-YTD < 2.00
               MOVE 2.00 TO XCLM-ACCRUED-YTD
           END-IF
           IF XCLM-ACCRUED-YTD > 35.00
               MOVE 35.00 TO XCLM-ACCRUED-YTD
           END-IF
           ADD XCLM-ACCRUED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN NEXT REVIEW DATE IS PAST
      * BAT -> MAL WHEN NEXT REVIEW DATE IS PAST
      * MAL -> TEL WHEN NEXT REVIEW DATE IS PAST
      * TEL -> ONL WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3850-SET-CHANNEL.
           EVALUATE XCLT-CHANNEL-CD
               WHEN 'BRN'
                   IF XCLM-NEXT-REVIEW-DATE > 0 AND
                       XCLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'BAT' TO XCLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF XCLM-NEXT-REVIEW-DATE > 0 AND
                       XCLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'MAL' TO XCLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF XCLM-NEXT-REVIEW-DATE > 0 AND
                       XCLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'TEL' TO XCLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF XCLM-NEXT-REVIEW-DATE > 0 AND
                       XCLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'ONL' TO XCLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3860-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCLT-CHANNEL-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCLT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CONTRIB YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3870-ACCRUE-CONTRIB.
           IF XCLM-CONTRIB-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLM-CONTRIB-YTD * XCLM-RATE / 100 / 365
               ADD WS-WORK-AMT TO XCLM-MISSED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 365 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3880-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF XCLT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCLT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO XCLM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE MISSED YTD BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3890-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCLT-TRAN-CD
                   ADD XCLM-MISSED-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCLT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCLM-MISSED-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 45 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3900-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF XCLM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCLM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO XCLM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CONTRIB YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3910-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCLM-REASON-CD
                   ADD XCLM-CONTRIB-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCLM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCLM-CONTRIB-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E589 CONTACT REQUIRED
      * E595 ACCRUED AMT NEGATIVE
      * E980 ACCRUED AMT OVER 99999
      * E031 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3920-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLM-CONTACT = SPACES
                   MOVE 'E589' TO WS-REJECT-CODE
               WHEN XCLM-ACCRUED-AMT < 0
                   MOVE 'E595' TO WS-REJECT-CODE
               WHEN XCLM-ACCRUED-AMT > 99999.00
                   MOVE 'E980' TO WS-REJECT-CODE
               WHEN XCLM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E031' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PAYOUT YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3930-SPLIT-PAYOUT.
           COMPUTE WS-WORK-AMT = XCLM-PAYOUT-YTD / 12
           COMPUTE WS-WORK-AMT-2 = XCLM-PAYOUT-YTD - (WS-WORK-AMT * 12)
           COMPUTE XCLM-BONUS-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WEEKLY FEE ON THE WEEKLY AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3940-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN XCLM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO XCLM-WEEKLY-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-STOP-FLAG = 'Y'
                   COMPUTE XCLM-WEEKLY-AMT ROUNDED = XCLT-WEEKLY-AMT *
                       XCLM-DISCOUNT / 200
               WHEN XCLT-CHANNEL-CD = 'BRN'
                   MOVE 20.00 TO XCLM-WEEKLY-AMT
               WHEN XCLT-CHANNEL-CD = 'BAT'
                   MOVE 5.00 TO XCLM-WEEKLY-AMT
               WHEN OTHER
                   COMPUTE XCLM-WEEKLY-AMT ROUNDED = XCLT-WEEKLY-AMT *
                       XCLM-DISCOUNT / 100
           END-EVALUATE
           IF XCLM-WEEKLY-AMT > 0 AND XCLM-WEEKLY-AMT < 5.00
               MOVE 5.00 TO XCLM-WEEKLY-AMT
           END-IF
           IF XCLM-WEEKLY-AMT > 250.00
               MOVE 250.00 TO XCLM-WEEKLY-AMT
           END-IF
           ADD XCLM-WEEKLY-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE ACCRUED BAND TIER FOR THE BONUS AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3950-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ACCRUED-BAND-RATE(1) TO XCLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF XCLT-BONUS-AMT NOT < WS-ACCRUED-BAND-LOW(WS-TX)
                   MOVE WS-ACCRUED-BAND-RATE(WS-TX) TO XCLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT BONUS YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3960-SPLIT-BONUS.
           COMPUTE WS-WORK-AMT = XCLM-BONUS-YTD / 2
           COMPUTE WS-WORK-AMT-2 = XCLM-BONUS-YTD - (WS-WORK-AMT * 2)
           COMPUTE XCLM-MISSED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PAYOUT YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3970-SPLIT-PAYOUT.
           COMPUTE WS-WORK-AMT = XCLM-PAYOUT-YTD / 12
           COMPUTE WS-WORK-AMT-2 = XCLM-PAYOUT-YTD - (WS-WORK-AMT * 12)
           COMPUTE XCLT-WEEKLY-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE GOAL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO BONUS YTD.
      *----------------------------------------------------------------*
       3980-PRORATE-BONUS.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLM-BONUS-YTD ROUNDED =
                   XCLM-GOAL-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO XCLM-BONUS-YTD
           END-IF
           ADD XCLM-BONUS-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * GOAL AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3990-FLAG-GOAL.
           IF XCLT-GOAL-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE XCLT-GOAL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * CONTRIB AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4000-FLAG-CONTRIB.
           IF XCLM-CONTRIB-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE XCLM-CONTRIB-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WEEKLY AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4010-SPLIT-WEEKLY.
           COMPUTE WS-WORK-AMT = XCLM-WEEKLY-AMT / 4
           COMPUTE WS-WORK-AMT-2 = XCLM-WEEKLY-AMT - (WS-WORK-AMT * 4)
           COMPUTE XCLM-CONTRIB-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4020-DESCRIBE-REASON.
           EVALUATE XCLM-REASON-CD
               WHEN 'AL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'BT'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'CY'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'KM'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'MV'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'XK'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'ZW'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 120 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4030-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF XCLM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCLM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO XCLM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4040-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF XCLM-NEXT-DUE-DATE > 0 AND XCLM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCLM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD XCLM-WEEKLY-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD XCLM-WEEKLY-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD XCLM-WEEKLY-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD XCLM-WEEKLY-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCLM-WEEKLY-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4050-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF XCLM-NEXT-REVIEW-DATE > 0 AND XCLM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCLM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD XCLT-GOAL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD XCLT-GOAL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD XCLT-GOAL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD XCLT-GOAL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCLT-GOAL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE CONTRIB TIER TIER FOR THE WEEKLY AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4060-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CONTRIB-TIER-RATE(1) TO XCLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF XCLM-WEEKLY-AMT NOT < WS-CONTRIB-TIER-LOW(WS-TX)
                   MOVE WS-CONTRIB-TIER-RATE(WS-TX) TO XCLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4070-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE GOAL YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO MISSED AMT.
      *----------------------------------------------------------------*
       4080-PRORATE-MISSED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLM-MISSED-AMT ROUNDED =
                   XCLM-GOAL-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO XCLM-MISSED-AMT
           END-IF
           ADD XCLM-MISSED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE WEEKLY AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4090-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCLT-TRAN-CD
                   ADD XCLT-WEEKLY-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCLT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCLT-WEEKLY-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4100-DESCRIBE-TRAN.
           EVALUATE XCLT-TRAN-CD
               WHEN 'WD'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'DP'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'PO'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'BN'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'PN'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       4110-COUNT-NOTICE.
           IF XCLM-NOTICE-CNT < 9999999
               ADD 1 TO XCLM-NOTICE-CNT
           END-IF
           IF XCLM-NOTICE-CNT NOT < 12
               MOVE 'Y' TO XCLM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE MISSED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CONTRIB YTD.
      *----------------------------------------------------------------*
       4120-PRORATE-CONTRIB.
           IF WS-PERIOD-DAYS > 0
               COMPUTE XCLM-CONTRIB-YTD ROUNDED =
                   XCLM-MISSED-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO XCLM-CONTRIB-YTD
           END-IF
           ADD XCLM-CONTRIB-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4130-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF XCLM-NEXT-DUE-DATE > 0 AND XCLM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCLM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD XCLM-BONUS-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD XCLM-BONUS-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD XCLM-BONUS-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD XCLM-BONUS-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCLM-BONUS-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WEEKLY AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4140-ACCRUE-WEEKLY.
           IF XCLT-WEEKLY-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLT-WEEKLY-AMT * XCLM-RATE / 100 / 365
               ADD WS-WORK-AMT TO XCLM-GOAL-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ACCRUED AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4150-ACCRUE-ACCRUED.
           IF XCLM-ACCRUED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLM-ACCRUED-AMT * XCLM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO XCLM-MISSED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E702 ADDR REQUIRED
      * E649 CONTRIB AMT NEGATIVE
      * E702 CONTRIB AMT OVER 10000
      * E506 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       4160-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLM-ADDR = SPACES
                   MOVE 'E702' TO WS-REJECT-CODE
               WHEN XCLT-CONTRIB-AMT < 0
                   MOVE 'E649' TO WS-REJECT-CODE
               WHEN XCLT-CONTRIB-AMT > 10000.00
                   MOVE 'E702' TO WS-REJECT-CODE
               WHEN XCLM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E506' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PENALTY YTD IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4170-LIMIT-PENALTY.
           IF XCLM-PENALTY-YTD < 100.00
               MOVE 100.00 TO XCLM-PENALTY-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCLM-PENALTY-YTD > 10000.00
               MOVE 10000.00 TO XCLM-PENALTY-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * WD -> DP WHEN LAST ACT DATE IS PAST
      * DP -> PO WHEN LAST ACT DATE IS PAST
      * PO -> BN WHEN LAST ACT DATE IS PAST
      * BN -> PN WHEN LAST ACT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4180-SET-TRAN.
           EVALUATE XCLT-TRAN-CD
               WHEN 'WD'
                   IF XCLM-LAST-ACT-DATE > 0 AND XCLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DP' TO XCLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DP'
                   IF XCLM-LAST-ACT-DATE > 0 AND XCLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PO' TO XCLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PO'
                   IF XCLM-LAST-ACT-DATE > 0 AND XCLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BN' TO XCLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BN'
                   IF XCLM-LAST-ACT-DATE > 0 AND XCLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PN' TO XCLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 10 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4190-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF XCLM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCLM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO XCLT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E419 REF NO REQUIRED
      * E845 WORK AMT NEGATIVE
      * E033 WORK AMT OVER 50000
      * E512 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       4200-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN XCLM-REF-NO = SPACES
                   MOVE 'E419' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E845' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 50000.00
                   MOVE 'E033' TO WS-REJECT-CODE
               WHEN XCLT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E512' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-120, 121-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4210-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF XCLM-NEXT-REVIEW-DATE > 0 AND XCLM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCLM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD XCLM-PENALTY-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 120
                   ADD XCLM-PENALTY-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD XCLM-PENALTY-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD XCLM-PENALTY-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCLM-PENALTY-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON MISSED AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4220-ACCRUE-MISSED.
           IF XCLM-MISSED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLM-MISSED-AMT * XCLM-RATE / 100 / 360
               ADD WS-WORK-AMT TO XCLM-BONUS-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * BONUS AMT IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4230-LIMIT-BONUS.
           IF XCLT-BONUS-AMT < 25.00
               MOVE 25.00 TO XCLT-BONUS-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCLT-BONUS-AMT > 50000.00
               MOVE 50000.00 TO XCLT-BONUS-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PENALTY AMT IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4240-LIMIT-PENALTY.
           IF XCLM-PENALTY-AMT < 25.00
               MOVE 25.00 TO XCLM-PENALTY-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF XCLM-PENALTY-AMT > 10000.00
               MOVE 10000.00 TO XCLM-PENALTY-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4250-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF XCLM-LAST-PAY-DATE > 0 AND XCLM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCLM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD XCLM-GOAL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD XCLM-GOAL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD XCLM-GOAL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD XCLM-GOAL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCLM-GOAL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * FLM -> HLS WHEN NEXT DUE DATE IS PAST
      * HLS -> KMJ WHEN NEXT DUE DATE IS PAST
      * KMJ -> PDA WHEN NEXT DUE DATE IS PAST
      * PDA -> RVG WHEN NEXT DUE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4260-SET-REGION.
           EVALUATE XCLM-REGION-CD
               WHEN 'FLM'
                   IF XCLM-NEXT-DUE-DATE > 0 AND XCLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HLS' TO XCLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HLS'
                   IF XCLM-NEXT-DUE-DATE > 0 AND XCLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KMJ' TO XCLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KMJ'
                   IF XCLM-NEXT-DUE-DATE > 0 AND XCLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PDA' TO XCLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PDA'
                   IF XCLM-NEXT-DUE-DATE > 0 AND XCLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RVG' TO XCLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE BONUS FEE ON THE ACCRUED YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4270-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN XCLM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO XCLM-BONUS-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN XCLM-STOP-FLAG = 'Y'
                   COMPUTE XCLM-BONUS-AMT ROUNDED = XCLM-ACCRUED-YTD *
                       XCLM-RATE / 200
               WHEN XCLM-REGION-CD = 'FLM'
                   MOVE 20.00 TO XCLM-BONUS-AMT
               WHEN XCLM-REGION-CD = 'HLS'
                   MOVE 3.00 TO XCLM-BONUS-AMT
               WHEN OTHER
                   COMPUTE XCLM-BONUS-AMT ROUNDED = XCLM-ACCRUED-YTD *
                       XCLM-RATE / 100
           END-EVALUATE
           IF XCLM-BONUS-AMT > 0 AND XCLM-BONUS-AMT < 5.00
               MOVE 5.00 TO XCLM-BONUS-AMT
           END-IF
           IF XCLM-BONUS-AMT > 50.00
               MOVE 50.00 TO XCLM-BONUS-AMT
           END-IF
           ADD XCLM-BONUS-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT PAYOUT YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4280-SPLIT-PAYOUT.
           COMPUTE WS-WORK-AMT = XCLM-PAYOUT-YTD / 3
           COMPUTE WS-WORK-AMT-2 = XCLM-PAYOUT-YTD - (WS-WORK-AMT * 3)
           COMPUTE XCLM-WEEKLY-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE GOAL AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4290-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = XCLM-TYPE-CD
                   ADD XCLM-GOAL-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE XCLM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE XCLM-GOAL-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CONTRIB TIER TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4300-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CONTRIB-TIER-RATE(1) TO XCLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF WS-WORK-AMT NOT < WS-CONTRIB-TIER-LOW(WS-TX)
                   MOVE WS-CONTRIB-TIER-RATE(WS-TX) TO XCLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON BONUS AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4310-ACCRUE-BONUS.
           IF XCLT-BONUS-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLT-BONUS-AMT * XCLM-RATE / 100 / 360
               ADD WS-WORK-AMT TO XCLM-WEEKLY-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 60 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4320-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF XCLT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCLT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO XCLM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE MISSED GRID TIER FOR THE PAYOUT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4330-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-MISSED-GRID-RATE(1) TO XCLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF XCLM-PAYOUT-YTD NOT < WS-MISSED-GRID-LOW(WS-TX)
                   MOVE WS-MISSED-GRID-RATE(WS-TX) TO XCLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF XCLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO XCLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4340-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF XCLM-LAST-PAY-DATE > 0 AND XCLM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE XCLM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD XCLM-PENALTY-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD XCLM-PENALTY-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD XCLM-PENALTY-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD XCLM-PENALTY-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD XCLM-PENALTY-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CONTRIB YTD AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4350-ACCRUE-CONTRIB.
           IF XCLM-CONTRIB-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   XCLM-CONTRIB-YTD * XCLM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO XCLT-CONTRIB-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE XCL FIELDS TO XCFEE AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7020-CALL-XCFEE.
           INITIALIZE XCFEE-PARMS
           MOVE XCLM-BONUS-YTD
             TO XCFEE-BONUS-YTD
           MOVE XCLM-MISSED-YTD
             TO XCFEE-MISSED-YTD
           MOVE XCLM-GOAL-YTD
             TO XCFEE-GOAL-YTD
           MOVE XCLM-WEEKLY-AMT
             TO XCFEE-WEEKLY-AMT
           MOVE XCLM-GOAL-AMT
             TO XCFEE-GOAL-AMT
           MOVE XCLM-CONTRIB-AMT
             TO XCFEE-CONTRIB-AMT
           MOVE XCLM-MATURITY-DATE
             TO XCFEE-MATURITY-DATE
           MOVE XCLM-NOTICE-DATE
             TO XCFEE-NOTICE-DATE
           MOVE XCLM-LAST-PAY-DATE
             TO XCFEE-LAST-PAY-DATE
           MOVE XCLM-NEXT-DUE-DATE
             TO XCFEE-NEXT-DUE-DATE
           MOVE XCLM-REGION-CD
             TO XCFEE-REGION-CD
           MOVE XCLM-REASON-CD
             TO XCFEE-REASON-CD
           MOVE XCLM-CYCLE-CD
             TO XCFEE-CYCLE-CD
           MOVE XCLM-TYPE-CD
             TO XCFEE-TYPE-CD
           MOVE XCLM-AUTO-RENEW-FLAG
             TO XCFEE-AUTO-RENEW-FLAG
           MOVE XCLM-EXEMPT-FLAG
             TO XCFEE-EXEMPT-FLAG
           MOVE XCLM-STOP-FLAG
             TO XCFEE-STOP-FLAG
           MOVE XCLM-NOTICE-CNT
             TO XCFEE-NOTICE-CNT
           MOVE XCLM-CYCLE-CNT
             TO XCFEE-CYCLE-CNT
           MOVE XCLM-DISCOUNT
             TO XCFEE-DISCOUNT
           MOVE XCLM-RATE
             TO XCFEE-RATE
           MOVE XCLM-DESC
             TO XCFEE-DESC
           MOVE XCLM-REF-NO
             TO XCFEE-REF-NO
           CALL 'XCFEE' USING XCFEE-PARMS
           EVALUATE XCFEE-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE XCFEE-MISSED-YTD
                     TO XCLM-MISSED-YTD
                   MOVE XCFEE-GOAL-YTD
                     TO XCLM-GOAL-YTD
                   MOVE XCFEE-WEEKLY-AMT
                     TO XCLM-WEEKLY-AMT
                   MOVE XCFEE-CONTRIB-AMT
                     TO XCLM-CONTRIB-AMT
                   MOVE XCFEE-AUTO-RENEW-FLAG
                     TO XCLM-AUTO-RENEW-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RFEE' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-XCFEE' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE XCL FIELDS TO XCLIM AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7010-CALL-XCLIM.
           INITIALIZE XCLIM-PARMS
           MOVE XCLM-PAYOUT-YTD
             TO XCLIM-PAYOUT-YTD
           MOVE XCLM-ACCRUED-AMT
             TO XCLIM-ACCRUED-AMT
           MOVE XCLM-GOAL-AMT
             TO XCLIM-GOAL-AMT
           MOVE XCLM-PAYOUT-AMT
             TO XCLIM-PAYOUT-AMT
           MOVE XCLM-WEEKLY-AMT
             TO XCLIM-WEEKLY-AMT
           MOVE XCLM-PENALTY-YTD
             TO XCLIM-PENALTY-YTD
           MOVE XCLM-NOTICE-DATE
             TO XCLIM-NOTICE-DATE
           MOVE XCLM-OPEN-DATE
             TO XCLIM-OPEN-DATE
           MOVE XCLM-LAST-PAY-DATE
             TO XCLIM-LAST-PAY-DATE
           MOVE XCLM-LAST-STMT-DATE
             TO XCLIM-LAST-STMT-DATE
           MOVE XCLM-REGION-CD
             TO XCLIM-REGION-CD
           MOVE XCLM-PLAN-CD
             TO XCLIM-PLAN-CD
           MOVE XCLM-CYCLE-CD
             TO XCLIM-CYCLE-CD
           MOVE XCLM-REASON-CD
             TO XCLIM-REASON-CD
           MOVE XCLM-SUSPEND-FLAG
             TO XCLIM-SUSPEND-FLAG
           MOVE XCLM-STOP-FLAG
             TO XCLIM-STOP-FLAG
           MOVE XCLM-PREFERRED-FLAG
             TO XCLIM-PREFERRED-FLAG
           MOVE XCLM-PAYMENT-CNT
             TO XCLIM-PAYMENT-CNT
           MOVE XCLM-CYCLE-CNT
             TO XCLIM-CYCLE-CNT
           MOVE XCLM-DISCOUNT
             TO XCLIM-DISCOUNT
           MOVE XCLM-FEE-PCT
             TO XCLIM-FEE-PCT
           MOVE XCLM-REF-NO
             TO XCLIM-REF-NO
           MOVE XCLM-ADDR
             TO XCLIM-ADDR
           CALL 'XCLIM' USING XCLIM-PARMS
           EVALUATE XCLIM-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE XCLIM-PAYOUT-YTD
                     TO XCLM-PAYOUT-YTD
                   MOVE XCLIM-WEEKLY-AMT
                     TO XCLM-WEEKLY-AMT
                   MOVE XCLIM-PENALTY-YTD
                     TO XCLM-PENALTY-YTD
                   CONTINUE
               WHEN 08
                   MOVE 'RLIM' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-XCLIM' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE XCL FIELDS TO XCTIER AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7000-CALL-XCTIER.
           INITIALIZE XCTIER-PARMS
           MOVE XCLM-BONUS-YTD
             TO XCTIER-BONUS-YTD
           MOVE XCLM-ACCRUED-AMT
             TO XCTIER-ACCRUED-AMT
           MOVE XCLM-PAYOUT-AMT
             TO XCTIER-PAYOUT-AMT
           MOVE XCLM-ACCRUED-YTD
             TO XCTIER-ACCRUED-YTD
           MOVE XCLM-MISSED-AMT
             TO XCTIER-MISSED-AMT
           MOVE XCLM-GOAL-AMT
             TO XCTIER-GOAL-AMT
           MOVE XCLM-NOTICE-DATE
             TO XCTIER-NOTICE-DATE
           MOVE XCLM-LAST-PAY-DATE
             TO XCTIER-LAST-PAY-DATE
           MOVE XCLM-LAST-STMT-DATE
             TO XCTIER-LAST-STMT-DATE
           MOVE XCLM-EXPIRY-DATE
             TO XCTIER-EXPIRY-DATE
           MOVE XCLM-PAYOUT-METHOD-CD
             TO XCTIER-PAYOUT-METHOD-CD
           MOVE XCLM-REGION-CD
             TO XCTIER-REGION-CD
           MOVE XCLM-PLAN-CD
             TO XCTIER-PLAN-CD
           MOVE XCLM-REASON-CD
             TO XCTIER-REASON-CD
           MOVE XCLM-OVERRIDE-FLAG
             TO XCTIER-OVERRIDE-FLAG
           MOVE XCLM-SUSPEND-FLAG
             TO XCTIER-SUSPEND-FLAG
           MOVE XCLM-PREFERRED-FLAG
             TO XCTIER-PREFERRED-FLAG
           MOVE XCLM-RETRY-CNT
             TO XCTIER-RETRY-CNT
           MOVE XCLM-CYCLE-CNT
             TO XCTIER-CYCLE-CNT
           MOVE XCLM-RATE
             TO XCTIER-RATE
           MOVE XCLM-DISCOUNT
             TO XCTIER-DISCOUNT
           MOVE XCLM-NAME
             TO XCTIER-NAME
           MOVE XCLM-CONTACT
             TO XCTIER-CONTACT
           CALL 'XCTIER' USING XCTIER-PARMS
           EVALUATE XCTIER-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE XCTIER-BONUS-YTD
                     TO XCLM-BONUS-YTD
                   MOVE XCTIER-ACCRUED-AMT
                     TO XCLM-ACCRUED-AMT
                   MOVE XCTIER-PAYOUT-AMT
                     TO XCLM-PAYOUT-AMT
                   MOVE XCTIER-ACCRUED-YTD
                     TO XCLM-ACCRUED-YTD
                   MOVE XCTIER-GOAL-AMT
                     TO XCLM-GOAL-AMT
                   MOVE XCTIER-OVERRIDE-FLAG
                     TO XCLM-OVERRIDE-FLAG
                   MOVE XCTIER-PREFERRED-FLAG
                     TO XCLM-PREFERRED-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RIER' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-XCTIER' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE XCLT-KEY TO RD-KEY
           MOVE XCLT-TRAN-CD TO RD-CODE
           MOVE XCLT-BONUS-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE XCLT-KEY TO RD-KEY
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
           DISPLAY 'XCLYREN READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'XCLYREN' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
