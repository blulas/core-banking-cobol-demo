      *================================================================*
      * PROGRAM  : SDBAUDT                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : SAFE DEPOSIT BOXES - AUDIT TRAIL. READS THE SDB     *
      *            TRANSACTIONS, UPDATES THE SDB MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. SAFE DEPOSIT BOX RENTAL BILLING,*
      *            ACCESS LOG AND DRILLING                             *
      * INPUT    : SDBMST (KSDS), SDBTRN, BANKCTL                      *
      * OUTPUT   : SDBOUT, SDBAURPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, SDWAIV, SDLIM           *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1993-12-16 PQR  ORIGINAL                                       *
      * 1994-08-06 LMB  PREPAID LIMIT REVISED INC9980                  *
      * 1997-12-15 RJT  RENT CALCULATION CORRECTED PRB4499             *
      * 2000-04-25 BHS  AUDIT FINDING - DRILL CONTROL TOTALS REQ9756   *
      * 2002-05-17 TWH  KEY-FEE STATUS CODES ADDED REQ8394             *
      * 2008-02-09 PQR  RENT CALCULATION CORRECTED CHG3047             *
      * 2009-12-06 EWS  DEPOSIT LIMIT REVISED INC2762                  *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SDBAUDT.
       AUTHOR. C R PATEL.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "SDBMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS SDBM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "SDBTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "SDBOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "SDBAURPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY SDBMST.
       FD  TRAN-FILE.
       COPY SDBTRN.
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
                                           VALUE 19.000000.
           05  WS-DAYS                     PIC S9(07) VALUE 0.
           05  WS-PERIOD-DAYS              PIC S9(05) VALUE 31.
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
      * WS-RENT-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-RENT-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000056500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000178000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002550000000181000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003050000000849500'.
       01  WS-RENT-SCHED-TBL REDEFINES WS-RENT-SCHED-VALUES.
           05  WS-RENT-SCHED-ROW OCCURS 4 TIMES.
               10  WS-RENT-SCHED-LOW       PIC 9(09)V99.
               10  WS-RENT-SCHED-RATE      PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-DRILL-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-DRILL-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000169500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000086500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012500000000668500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012600000000058000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012850000000153500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012900000000034000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017900000000698000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018400000000368500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018900000000382500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018950000000732000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00019200000000298000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00029200000000103000'.
       01  WS-DRILL-BAND-TBL REDEFINES WS-DRILL-BAND-VALUES.
           05  WS-DRILL-BAND-ROW OCCURS 12 TIMES.
               10  WS-DRILL-BAND-LOW       PIC 9(09)V99.
               10  WS-DRILL-BAND-RATE      PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-PREPAID-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PREPAID-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000725500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000337500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005500000000862000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008000000000497500'.
       01  WS-PREPAID-TIER-TBL REDEFINES WS-PREPAID-TIER-VALUES.
           05  WS-PREPAID-TIER-ROW OCCURS 4 TIMES.
               10  WS-PREPAID-TIER-LOW     PIC 9(09)V99.
               10  WS-PREPAID-TIER-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-DRILL-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-DRILL-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000282000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000721500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007500000000113000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008000000000358000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010500000000666000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010550000000786000'.
       01  WS-DRILL-GRID-TBL REDEFINES WS-DRILL-GRID-VALUES.
           05  WS-DRILL-GRID-ROW OCCURS 6 TIMES.
               10  WS-DRILL-GRID-LOW       PIC 9(09)V99.
               10  WS-DRILL-GRID-RATE      PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  SDBAUDT  SAFE DEPOSIT BOXES'.
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
       COPY SDWAIVP.
       COPY SDLIMP.

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
               MOVE 'SDBMST' TO ABND-FILE-NAME
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
           MOVE SDBT-KEY TO SDBM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-FLAG-PREPAID
               PERFORM 3010-DESCRIBE-BOX-SIZE
               PERFORM 3020-ASSESS-REVIEW
               PERFORM 3030-PRORATE-KEY-FEE
               PERFORM 3040-AGE-LAST-MAINT
               PERFORM 3050-SPLIT-RENT
               PERFORM 3060-ASSESS-PREFERRED
               PERFORM 3070-ACCRUE-RENT
               PERFORM 3080-LIMIT-RENT
               PERFORM 3090-PRORATE-LATE-FEE
               PERFORM 3100-ASSESS-REVIEW
               PERFORM 3110-ACCUM-SOURCE
               PERFORM 3120-ASSESS-PREFERRED
               PERFORM 3130-ACCUM-VAULT
               PERFORM 3140-SPLIT-DEPOSIT
               PERFORM 3150-CHECK-OPEN
               PERFORM 3160-EDIT-REF-NO
               PERFORM 3170-SET-BOX-SIZE
               PERFORM 3180-CHECK-NEXT-DUE
               PERFORM 3190-EDIT-ADDR
               PERFORM 3200-FLAG-KEY-FEE
               PERFORM 3210-ACCUM-TRAN
               PERFORM 3220-PRORATE-KEY-FEE
               PERFORM 3230-LIMIT-DRILL
               PERFORM 3240-EDIT-REJECT
               PERFORM 3250-DESCRIBE-BOX-SIZE
               PERFORM 3260-AGE-NEXT-DUE
               PERFORM 3270-ACCUM-SOURCE
               PERFORM 3280-COUNT-RETRY
               PERFORM 3290-SELECT-DISCOUNT
               PERFORM 3300-ACCRUE-RENT
               PERFORM 3310-AGE-EFF
               PERFORM 3320-ACCRUE-PREPAID
               PERFORM 3330-AGE-MATURITY
               PERFORM 3340-SPLIT-RENT
               PERFORM 3350-ASSESS-AUTO-RENEW
               PERFORM 3360-AGE-NOTICE
               PERFORM 3370-CHECK-CLOSE
               PERFORM 3380-COUNT-NOTICE
               PERFORM 3390-SET-CHANNEL
               PERFORM 3400-SET-SOURCE
               PERFORM 3410-SPLIT-DRILL
               PERFORM 3420-LIMIT-DEPOSIT
               PERFORM 3430-SPLIT-DEPOSIT
               PERFORM 3440-CHECK-NOTICE
               PERFORM 3450-ACCUM-VAULT
               PERFORM 3460-COUNT-SEQ
               PERFORM 3470-COUNT-ITEM
               PERFORM 3480-SET-BOX-SIZE
               PERFORM 3490-PRORATE-DRILL
               PERFORM 3500-ACCUM-REASON
               PERFORM 3510-DESCRIBE-SOURCE
               PERFORM 3520-DESCRIBE-BOX-SIZE
               PERFORM 3530-LIMIT-ITEM
               PERFORM 3540-DESCRIBE-CHANNEL
               PERFORM 3550-EDIT-MEMO
               PERFORM 3560-ASSESS-REVIEW
               PERFORM 3570-ACCUM-TRAN
               PERFORM 3580-SELECT-PENALTY-PCT
               PERFORM 3590-EDIT-REF-NO
               PERFORM 3600-CHECK-LAST-ACT
               PERFORM 3610-CHECK-NEXT-DUE
               PERFORM 3620-DESCRIBE-BRANCH
               PERFORM 3630-DESCRIBE-CYCLE
               PERFORM 3640-LIMIT-KEY-FEE
               PERFORM 3650-LIMIT-DRILL
               PERFORM 3660-ASSESS-SUSPEND
               PERFORM 3670-COUNT-ITEM
               PERFORM 3680-SELECT-DISCOUNT
               PERFORM 3690-ASSESS-PREFERRED
               PERFORM 3700-DESCRIBE-VAULT
               PERFORM 3710-EDIT-NAME
               PERFORM 3720-DESCRIBE-REASON
               PERFORM 3730-SPLIT-DRILL
               PERFORM 3740-ASSESS-SUSPEND
               PERFORM 3750-LIMIT-PREPAID
               PERFORM 3760-SET-REASON
               PERFORM 3770-DESCRIBE-CYCLE
               PERFORM 3780-SET-CYCLE
               PERFORM 3790-ACCRUE-KEY-FEE
               PERFORM 3800-PRORATE-DEPOSIT
               PERFORM 3810-SELECT-DISCOUNT
               PERFORM 3820-SET-BOX-SIZE
               PERFORM 3830-PRORATE-KEY-FEE
               PERFORM 3840-SELECT-PENALTY-PCT
               PERFORM 3850-SET-SOURCE
               PERFORM 3860-PRORATE-DEPOSIT
               PERFORM 3870-PRORATE-DRILL
               PERFORM 3880-ACCRUE-KEY-FEE
               PERFORM 3890-COUNT-NOTICE
               PERFORM 3900-SPLIT-RENT
               PERFORM 3910-AGE-CLOSE
               PERFORM 3920-AGE-LAST-MAINT
               PERFORM 3930-DESCRIBE-CYCLE
               PERFORM 3940-COUNT-NOTICE
               PERFORM 3950-CHECK-OPEN
               PERFORM 3960-ACCUM-CYCLE
               PERFORM 3970-LIMIT-PREPAID
               PERFORM 3980-COUNT-NOTICE
               PERFORM 3990-SET-BOX-SIZE
               PERFORM 4000-SPLIT-DRILL
               PERFORM 4010-COUNT-ITEM
               PERFORM 4020-PRORATE-PREPAID
               PERFORM 4030-AGE-LAST-STMT
               PERFORM 4040-CHECK-POST
               PERFORM 4050-COUNT-NOTICE
               PERFORM 4060-CHECK-CLOSE
               PERFORM 4070-LIMIT-DRILL
               PERFORM 4080-CHECK-POST
               PERFORM 4090-FLAG-ITEM
               PERFORM 4100-ACCUM-VAULT
               PERFORM 4110-ACCRUE-LATE-FEE
               PERFORM 4120-ACCRUE-DRILL
               PERFORM 4130-FLAG-LATE-FEE
               PERFORM 4140-COUNT-ITEM
               PERFORM 4150-ACCUM-VAULT
               PERFORM 4160-ACCRUE-PREPAID
               PERFORM 4170-AGE-LAST-STMT
               PERFORM 4180-EDIT-REJECT
               PERFORM 4190-DESCRIBE-BOX-SIZE
               PERFORM 4200-COUNT-ITEM
               PERFORM 4210-FLAG-RENT
               PERFORM 4220-DESCRIBE-BOX-SIZE
               PERFORM 4230-DESCRIBE-BOX-SIZE
               PERFORM 4240-EDIT-DESC
               PERFORM 4250-SELECT-DISCOUNT
               PERFORM 4260-CHECK-EFF
               PERFORM 4270-LIMIT-KEY-FEE
               PERFORM 4280-AGE-LAST-MAINT
               PERFORM 4290-SPLIT-DEPOSIT
               PERFORM 4300-EDIT-REF-NO
               PERFORM 4310-FLAG-DRILL
               PERFORM 4320-FLAG-KEY-FEE
               PERFORM 4330-SELECT-PENALTY-PCT
               PERFORM 4340-LIMIT-RENT
               PERFORM 4350-CHECK-CLOSE
               PERFORM 4360-PRORATE-RENT
               PERFORM 4370-EDIT-OPERATOR
               PERFORM 4380-FLAG-RENT
               PERFORM 4390-EDIT-REF-NO
               PERFORM 4400-CHECK-LAST-MAINT
               PERFORM 4410-ACCUM-BOX-SIZE
               PERFORM 7040-CALL-SDWAIV
               PERFORM 7020-CALL-SDLIM
               REWRITE SDB-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'SDBMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM SDB-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * PREPAID YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3000-FLAG-PREPAID.
           IF SDBM-PREPAID-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE SDBM-PREPAID-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BOX SIZE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3010-DESCRIBE-BOX-SIZE.
           EVALUATE SDBM-BOX-SIZE-CD
               WHEN 'CU'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'MG'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'VG'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'VN'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'XJ'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE RENT FEE ON THE WORK AMT 2.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3020-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN SDBM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO SDBT-RENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDBT-REVERSAL-FLAG = 'Y'
                   COMPUTE SDBT-RENT-AMT ROUNDED = WS-WORK-AMT-2 *
                       SDBM-FEE-PCT / 200
               WHEN SDBM-BOX-SIZE-CD = 'CU'
                   MOVE 5.00 TO SDBT-RENT-AMT
               WHEN SDBM-BOX-SIZE-CD = 'MG'
                   MOVE 10.00 TO SDBT-RENT-AMT
               WHEN OTHER
                   COMPUTE SDBT-RENT-AMT ROUNDED = WS-WORK-AMT-2 *
                       SDBM-FEE-PCT / 100
           END-EVALUATE
           IF SDBT-RENT-AMT > 0 AND SDBT-RENT-AMT < 5.00
               MOVE 5.00 TO SDBT-RENT-AMT
           END-IF
           IF SDBT-RENT-AMT > 150.00
               MOVE 150.00 TO SDBT-RENT-AMT
           END-IF
           ADD SDBT-RENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE RENT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO KEY FEE AMT.
      *----------------------------------------------------------------*
       3030-PRORATE-KEY-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDBM-KEY-FEE-AMT ROUNDED =
                   SDBT-RENT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SDBM-KEY-FEE-AMT
           END-IF
           ADD SDBM-KEY-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-90, 91-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3040-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF SDBM-LAST-MAINT-DATE > 0 AND SDBM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDBM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SDBT-DEPOSIT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD SDBT-DEPOSIT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD SDBT-DEPOSIT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SDBT-DEPOSIT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDBT-DEPOSIT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT RENT AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3050-SPLIT-RENT.
           COMPUTE WS-WORK-AMT = SDBM-RENT-AMT / 12
           COMPUTE WS-WORK-AMT-2 = SDBM-RENT-AMT - (WS-WORK-AMT * 12)
           COMPUTE SDBM-KEY-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RENT YTD FEE ON THE LATE FEE AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3060-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN SDBM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO SDBM-RENT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDBT-REVERSAL-FLAG = 'Y'
                   COMPUTE SDBM-RENT-YTD ROUNDED = SDBM-LATE-FEE-AMT *
                       SDBM-FEE-PCT / 200
               WHEN SDBM-SOURCE-CD = 'DD'
                   MOVE 5.00 TO SDBM-RENT-YTD
               WHEN SDBM-SOURCE-CD = 'JZ'
                   MOVE 20.00 TO SDBM-RENT-YTD
               WHEN OTHER
                   COMPUTE SDBM-RENT-YTD ROUNDED = SDBM-LATE-FEE-AMT *
                       SDBM-FEE-PCT / 100
           END-EVALUATE
           IF SDBM-RENT-YTD > 0 AND SDBM-RENT-YTD < 10.00
               MOVE 10.00 TO SDBM-RENT-YTD
           END-IF
           IF SDBM-RENT-YTD > 150.00
               MOVE 150.00 TO SDBM-RENT-YTD
           END-IF
           ADD SDBM-RENT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RENT YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3070-ACCRUE-RENT.
           IF SDBM-RENT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBM-RENT-YTD * SDBM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO SDBM-KEY-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RENT YTD IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3080-LIMIT-RENT.
           IF SDBM-RENT-YTD < 50.00
               MOVE 50.00 TO SDBM-RENT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SDBM-RENT-YTD > 10000.00
               MOVE 10000.00 TO SDBM-RENT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE KEY FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO LATE FEE YTD.
      *----------------------------------------------------------------*
       3090-PRORATE-LATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDBM-LATE-FEE-YTD ROUNDED =
                   SDBT-KEY-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SDBM-LATE-FEE-YTD
           END-IF
           ADD SDBM-LATE-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE RENT YTD FEE ON THE DEPOSIT YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3100-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN SDBM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO SDBM-RENT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDBM-SUSPEND-FLAG = 'Y'
                   COMPUTE SDBM-RENT-YTD ROUNDED = SDBM-DEPOSIT-YTD *
                       SDBM-PENALTY-PCT / 200
               WHEN SDBM-CYCLE-CD = 'AP'
                   MOVE 7.00 TO SDBM-RENT-YTD
               WHEN SDBM-CYCLE-CD = 'DA'
                   MOVE 20.00 TO SDBM-RENT-YTD
               WHEN OTHER
                   COMPUTE SDBM-RENT-YTD ROUNDED = SDBM-DEPOSIT-YTD *
                       SDBM-PENALTY-PCT / 100
           END-EVALUATE
           IF SDBM-RENT-YTD > 0 AND SDBM-RENT-YTD < 5.00
               MOVE 5.00 TO SDBM-RENT-YTD
           END-IF
           IF SDBM-RENT-YTD > 35.00
               MOVE 35.00 TO SDBM-RENT-YTD
           END-IF
           ADD SDBM-RENT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE KEY FEE AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3110-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDBM-SOURCE-CD
                   ADD SDBM-KEY-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDBM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDBM-KEY-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PREPAID YTD FEE ON THE WORK AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3120-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN SDBM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO SDBM-PREPAID-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDBM-REVIEW-FLAG = 'Y'
                   COMPUTE SDBM-PREPAID-YTD ROUNDED = WS-WORK-AMT *
                       SDBM-PENALTY-PCT / 200
               WHEN SDBM-REASON-CD = 'J'
                   MOVE 12.00 TO SDBM-PREPAID-YTD
               WHEN SDBM-REASON-CD = 'K'
                   MOVE 15.00 TO SDBM-PREPAID-YTD
               WHEN OTHER
                   COMPUTE SDBM-PREPAID-YTD ROUNDED = WS-WORK-AMT *
                       SDBM-PENALTY-PCT / 100
           END-EVALUATE
           IF SDBM-PREPAID-YTD > 0 AND SDBM-PREPAID-YTD < 1.00
               MOVE 1.00 TO SDBM-PREPAID-YTD
           END-IF
           IF SDBM-PREPAID-YTD > 50.00
               MOVE 50.00 TO SDBM-PREPAID-YTD
           END-IF
           ADD SDBM-PREPAID-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE DRILL AMT BY VAULT CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3130-ACCUM-VAULT.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDBM-VAULT-CD
                   ADD SDBM-DRILL-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDBM-VAULT-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDBM-DRILL-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DEPOSIT AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3140-SPLIT-DEPOSIT.
           COMPUTE WS-WORK-AMT = SDBT-DEPOSIT-AMT / 2
           COMPUTE WS-WORK-AMT-2 = SDBT-DEPOSIT-AMT - (WS-WORK-AMT * 2)
           COMPUTE SDBM-KEY-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 90 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3150-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF SDBM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDBM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO SDBM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E826 REF NO REQUIRED
      * E294 DEPOSIT AMT NEGATIVE
      * E641 DEPOSIT AMT OVER 25000
      * E173 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3160-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDBM-REF-NO = SPACES
                   MOVE 'E826' TO WS-REJECT-CODE
               WHEN SDBT-DEPOSIT-AMT < 0
                   MOVE 'E294' TO WS-REJECT-CODE
               WHEN SDBT-DEPOSIT-AMT > 25000.00
                   MOVE 'E641' TO WS-REJECT-CODE
               WHEN SDBT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E173' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BOX SIZE CD:
      * CU -> MG WHEN LAST STMT DATE IS PAST
      * MG -> VG WHEN LAST STMT DATE IS PAST
      * VG -> VN WHEN LAST STMT DATE IS PAST
      * VN -> XJ WHEN LAST STMT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3170-SET-BOX-SIZE.
           EVALUATE SDBM-BOX-SIZE-CD
               WHEN 'CU'
                   IF SDBM-LAST-STMT-DATE > 0 AND SDBM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MG' TO SDBM-BOX-SIZE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MG'
                   IF SDBM-LAST-STMT-DATE > 0 AND SDBM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VG' TO SDBM-BOX-SIZE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VG'
                   IF SDBM-LAST-STMT-DATE > 0 AND SDBM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VN' TO SDBM-BOX-SIZE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VN'
                   IF SDBM-LAST-STMT-DATE > 0 AND SDBM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XJ' TO SDBM-BOX-SIZE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3180-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF SDBM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDBM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO SDBM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E533 ADDR REQUIRED
      * E281 LATE FEE YTD NEGATIVE
      * E386 LATE FEE YTD OVER 5000
      * E766 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3190-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDBM-ADDR = SPACES
                   MOVE 'E533' TO WS-REJECT-CODE
               WHEN SDBM-LATE-FEE-YTD < 0
                   MOVE 'E281' TO WS-REJECT-CODE
               WHEN SDBM-LATE-FEE-YTD > 5000.00
                   MOVE 'E386' TO WS-REJECT-CODE
               WHEN SDBM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E766' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * KEY FEE AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3200-FLAG-KEY-FEE.
           IF SDBT-KEY-FEE-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE SDBT-KEY-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE DEPOSIT YTD BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3210-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDBT-TRAN-CD
                   ADD SDBM-DEPOSIT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDBT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDBM-DEPOSIT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DRILL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO KEY FEE AMT.
      *----------------------------------------------------------------*
       3220-PRORATE-KEY-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDBM-KEY-FEE-AMT ROUNDED =
                   SDBM-DRILL-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SDBM-KEY-FEE-AMT
           END-IF
           ADD SDBM-KEY-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DRILL AMT IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3230-LIMIT-DRILL.
           IF SDBT-DRILL-AMT < 100.00
               MOVE 100.00 TO SDBT-DRILL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SDBT-DRILL-AMT > 500.00
               MOVE 500.00 TO SDBT-DRILL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E680 REJECT CD REQUIRED
      * E873 DRILL AMT NEGATIVE
      * E906 DRILL AMT OVER 250000
      * E586 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3240-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDBT-REJECT-CD = SPACES
                   MOVE 'E680' TO WS-REJECT-CODE
               WHEN SDBM-DRILL-AMT < 0
                   MOVE 'E873' TO WS-REJECT-CODE
               WHEN SDBM-DRILL-AMT > 250000.00
                   MOVE 'E906' TO WS-REJECT-CODE
               WHEN SDBM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E586' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BOX SIZE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3250-DESCRIBE-BOX-SIZE.
           EVALUATE SDBM-BOX-SIZE-CD
               WHEN 'CU'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'MG'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'VG'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'VN'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'XJ'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3260-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF SDBM-NEXT-DUE-DATE > 0 AND SDBM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDBM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SDBM-DRILL-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD SDBM-DRILL-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD SDBM-DRILL-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SDBM-DRILL-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDBM-DRILL-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE DRILL AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3270-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDBM-SOURCE-CD
                   ADD SDBT-DRILL-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDBM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDBT-DRILL-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3280-COUNT-RETRY.
           IF SDBM-RETRY-CNT < 9999999
               ADD 1 TO SDBM-RETRY-CNT
           END-IF
           IF SDBM-RETRY-CNT NOT < 5
               MOVE 'Y' TO SDBM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DRILL BAND TIER FOR THE PREPAID YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3290-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-DRILL-BAND-RATE(1) TO SDBM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF SDBM-PREPAID-YTD NOT < WS-DRILL-BAND-LOW(WS-TX)
                   MOVE WS-DRILL-BAND-RATE(WS-TX) TO SDBM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RENT YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3300-ACCRUE-RENT.
           IF SDBM-RENT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBM-RENT-YTD * SDBM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO SDBM-DEPOSIT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3310-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF SDBT-EFF-DATE > 0 AND SDBT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDBT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SDBT-DEPOSIT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD SDBT-DEPOSIT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD SDBT-DEPOSIT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD SDBT-DEPOSIT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDBT-DEPOSIT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PREPAID AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3320-ACCRUE-PREPAID.
           IF SDBM-PREPAID-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBM-PREPAID-AMT * SDBM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO SDBM-LATE-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3330-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF SDBM-MATURITY-DATE > 0 AND SDBM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDBM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SDBM-PREPAID-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD SDBM-PREPAID-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD SDBM-PREPAID-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SDBM-PREPAID-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDBM-PREPAID-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT RENT YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3340-SPLIT-RENT.
           COMPUTE WS-WORK-AMT = SDBM-RENT-YTD / 3
           COMPUTE WS-WORK-AMT-2 = SDBM-RENT-YTD - (WS-WORK-AMT * 3)
           COMPUTE SDBM-DRILL-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE KEY FEE FEE ON THE DRILL AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3350-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN SDBM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO SDBT-KEY-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDBT-REVERSAL-FLAG = 'Y'
                   COMPUTE SDBT-KEY-FEE-AMT ROUNDED = SDBT-DRILL-AMT *
                       SDBM-PENALTY-PCT / 200
               WHEN SDBT-CHANNEL-CD = 'BRN'
                   MOVE 7.00 TO SDBT-KEY-FEE-AMT
               WHEN SDBT-CHANNEL-CD = 'BAT'
                   MOVE 5.00 TO SDBT-KEY-FEE-AMT
               WHEN OTHER
                   COMPUTE SDBT-KEY-FEE-AMT ROUNDED = SDBT-DRILL-AMT *
                       SDBM-PENALTY-PCT / 100
           END-EVALUATE
           IF SDBT-KEY-FEE-AMT > 0 AND SDBT-KEY-FEE-AMT < 5.00
               MOVE 5.00 TO SDBT-KEY-FEE-AMT
           END-IF
           IF SDBT-KEY-FEE-AMT > 75.00
               MOVE 75.00 TO SDBT-KEY-FEE-AMT
           END-IF
           ADD SDBT-KEY-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3360-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF SDBM-NOTICE-DATE > 0 AND SDBM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDBM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SDBM-LATE-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD SDBM-LATE-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD SDBM-LATE-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD SDBM-LATE-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDBM-LATE-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3370-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF SDBM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDBM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO SDBM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3380-COUNT-NOTICE.
           IF SDBM-NOTICE-CNT < 9999999
               ADD 1 TO SDBM-NOTICE-CNT
           END-IF
           IF SDBM-NOTICE-CNT NOT < 12
               MOVE 'Y' TO SDBM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN EFF DATE IS PAST
      * BAT -> MAL WHEN EFF DATE IS PAST
      * MAL -> TEL WHEN EFF DATE IS PAST
      * TEL -> ONL WHEN EFF DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3390-SET-CHANNEL.
           EVALUATE SDBT-CHANNEL-CD
               WHEN 'BRN'
                   IF SDBT-EFF-DATE > 0 AND SDBT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO SDBT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF SDBT-EFF-DATE > 0 AND SDBT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO SDBT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF SDBT-EFF-DATE > 0 AND SDBT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO SDBT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF SDBT-EFF-DATE > 0 AND SDBT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO SDBT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * DD -> JZ WHEN LAST STMT DATE IS PAST
      * JZ -> PS WHEN LAST STMT DATE IS PAST
      * PS -> UM WHEN LAST STMT DATE IS PAST
      * UM -> DD WHEN LAST STMT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3400-SET-SOURCE.
           EVALUATE SDBM-SOURCE-CD
               WHEN 'DD'
                   IF SDBM-LAST-STMT-DATE > 0 AND SDBM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JZ' TO SDBM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JZ'
                   IF SDBM-LAST-STMT-DATE > 0 AND SDBM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PS' TO SDBM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PS'
                   IF SDBM-LAST-STMT-DATE > 0 AND SDBM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UM' TO SDBM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UM'
                   IF SDBM-LAST-STMT-DATE > 0 AND SDBM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DD' TO SDBM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT DRILL YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3410-SPLIT-DRILL.
           COMPUTE WS-WORK-AMT = SDBM-DRILL-YTD / 2
           COMPUTE WS-WORK-AMT-2 = SDBM-DRILL-YTD - (WS-WORK-AMT * 2)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DEPOSIT YTD IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3420-LIMIT-DEPOSIT.
           IF SDBM-DEPOSIT-YTD < 100.00
               MOVE 100.00 TO SDBM-DEPOSIT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SDBM-DEPOSIT-YTD > 50000.00
               MOVE 50000.00 TO SDBM-DEPOSIT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DEPOSIT AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3430-SPLIT-DEPOSIT.
           COMPUTE WS-WORK-AMT = SDBT-DEPOSIT-AMT / 4
           COMPUTE WS-WORK-AMT-2 = SDBT-DEPOSIT-AMT - (WS-WORK-AMT * 4)
           COMPUTE SDBM-DRILL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3440-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF SDBM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDBM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO SDBM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE DEPOSIT YTD BY VAULT CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3450-ACCUM-VAULT.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDBM-VAULT-CD
                   ADD SDBM-DEPOSIT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDBM-VAULT-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDBM-DEPOSIT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3460-COUNT-SEQ.
           IF SDBT-SEQ < 9999999
               ADD 1 TO SDBT-SEQ
           END-IF
           IF SDBT-SEQ NOT < 12
               MOVE 'Y' TO SDBM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3470-COUNT-ITEM.
           IF SDBM-ITEM-CNT < 9999999
               ADD 1 TO SDBM-ITEM-CNT
           END-IF
           IF SDBM-ITEM-CNT NOT < 5
               MOVE 'Y' TO SDBM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BOX SIZE CD:
      * CU -> MG WHEN LAST MAINT DATE IS PAST
      * MG -> VG WHEN LAST MAINT DATE IS PAST
      * VG -> VN WHEN LAST MAINT DATE IS PAST
      * VN -> XJ WHEN LAST MAINT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3480-SET-BOX-SIZE.
           EVALUATE SDBM-BOX-SIZE-CD
               WHEN 'CU'
                   IF SDBM-LAST-MAINT-DATE > 0 AND SDBM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'MG' TO SDBM-BOX-SIZE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MG'
                   IF SDBM-LAST-MAINT-DATE > 0 AND SDBM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'VG' TO SDBM-BOX-SIZE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VG'
                   IF SDBM-LAST-MAINT-DATE > 0 AND SDBM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'VN' TO SDBM-BOX-SIZE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VN'
                   IF SDBM-LAST-MAINT-DATE > 0 AND SDBM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'XJ' TO SDBM-BOX-SIZE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE LATE FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DRILL YTD.
      *----------------------------------------------------------------*
       3490-PRORATE-DRILL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDBM-DRILL-YTD ROUNDED =
                   SDBM-LATE-FEE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SDBM-DRILL-YTD
           END-IF
           ADD SDBM-DRILL-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE LATE FEE YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3500-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDBM-REASON-CD
                   ADD SDBM-LATE-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDBM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDBM-LATE-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3510-DESCRIBE-SOURCE.
           EVALUATE SDBM-SOURCE-CD
               WHEN 'DD'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'JZ'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'PS'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'UM'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE BOX SIZE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3520-DESCRIBE-BOX-SIZE.
           EVALUATE SDBM-BOX-SIZE-CD
               WHEN 'CU'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'MG'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'VG'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'VN'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'XJ'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3530-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 50000.00
               MOVE 50000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3540-DESCRIBE-CHANNEL.
           EVALUATE SDBT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E047 MEMO REQUIRED
      * E706 RENT AMT NEGATIVE
      * E225 RENT AMT OVER 250000
      * E585 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3550-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDBT-MEMO = SPACES
                   MOVE 'E047' TO WS-REJECT-CODE
               WHEN SDBT-RENT-AMT < 0
                   MOVE 'E706' TO WS-REJECT-CODE
               WHEN SDBT-RENT-AMT > 250000.00
                   MOVE 'E225' TO WS-REJECT-CODE
               WHEN SDBM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E585' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RENT YTD FEE ON THE LATE FEE YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3560-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN SDBM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO SDBM-RENT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDBM-SUSPEND-FLAG = 'Y'
                   COMPUTE SDBM-RENT-YTD ROUNDED = SDBM-LATE-FEE-YTD *
                       SDBM-FEE-PCT / 200
               WHEN SDBM-REASON-CD = 'J'
                   MOVE 20.00 TO SDBM-RENT-YTD
               WHEN SDBM-REASON-CD = 'K'
                   MOVE 3.00 TO SDBM-RENT-YTD
               WHEN OTHER
                   COMPUTE SDBM-RENT-YTD ROUNDED = SDBM-LATE-FEE-YTD *
                       SDBM-FEE-PCT / 100
           END-EVALUATE
           IF SDBM-RENT-YTD > 0 AND SDBM-RENT-YTD < 2.00
               MOVE 2.00 TO SDBM-RENT-YTD
           END-IF
           IF SDBM-RENT-YTD > 25.00
               MOVE 25.00 TO SDBM-RENT-YTD
           END-IF
           ADD SDBM-RENT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE LATE FEE AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3570-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDBT-TRAN-CD
                   ADD SDBM-LATE-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDBT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDBM-LATE-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PREPAID TIER TIER FOR THE DRILL YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3580-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PREPAID-TIER-RATE(1) TO SDBM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF SDBM-DRILL-YTD NOT < WS-PREPAID-TIER-LOW(WS-TX)
                   MOVE WS-PREPAID-TIER-RATE(WS-TX) TO SDBM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E184 REF NO REQUIRED
      * E795 WORK AMT NEGATIVE
      * E778 WORK AMT OVER 5000
      * E707 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3590-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDBM-REF-NO = SPACES
                   MOVE 'E184' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E795' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 5000.00
                   MOVE 'E778' TO WS-REJECT-CODE
               WHEN SDBM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E707' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 120 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3600-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF SDBM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDBM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO SDBM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3610-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF SDBM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDBM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO SDBM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3620-DESCRIBE-BRANCH.
           EVALUATE SDBM-BRANCH-CD
               WHEN 'EXS'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'FJW'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'YFB'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3630-DESCRIBE-CYCLE.
           EVALUATE SDBM-CYCLE-CD
               WHEN 'AP'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'DA'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'HV'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'SE'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * KEY FEE YTD IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3640-LIMIT-KEY-FEE.
           IF SDBM-KEY-FEE-YTD < 0.00
               MOVE 0.00 TO SDBM-KEY-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SDBM-KEY-FEE-YTD > 50000.00
               MOVE 50000.00 TO SDBM-KEY-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DRILL AMT IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3650-LIMIT-DRILL.
           IF SDBM-DRILL-AMT < 100.00
               MOVE 100.00 TO SDBM-DRILL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SDBM-DRILL-AMT > 10000.00
               MOVE 10000.00 TO SDBM-DRILL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RENT FEE ON THE KEY FEE YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3660-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN SDBM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO SDBM-RENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDBM-REVIEW-FLAG = 'Y'
                   COMPUTE SDBM-RENT-AMT ROUNDED = SDBM-KEY-FEE-YTD *
                       SDBM-PENALTY-PCT / 200
               WHEN SDBM-REASON-CD = 'J'
                   MOVE 5.00 TO SDBM-RENT-AMT
               WHEN SDBM-REASON-CD = 'K'
                   MOVE 10.00 TO SDBM-RENT-AMT
               WHEN OTHER
                   COMPUTE SDBM-RENT-AMT ROUNDED = SDBM-KEY-FEE-YTD *
                       SDBM-PENALTY-PCT / 100
           END-EVALUATE
           IF SDBM-RENT-AMT > 0 AND SDBM-RENT-AMT < 10.00
               MOVE 10.00 TO SDBM-RENT-AMT
           END-IF
           IF SDBM-RENT-AMT > 100.00
               MOVE 100.00 TO SDBM-RENT-AMT
           END-IF
           ADD SDBM-RENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3670-COUNT-ITEM.
           IF SDBM-ITEM-CNT < 9999999
               ADD 1 TO SDBM-ITEM-CNT
           END-IF
           IF SDBM-ITEM-CNT NOT < 3
               MOVE 'Y' TO SDBM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DRILL GRID TIER FOR THE LATE FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3680-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-DRILL-GRID-RATE(1) TO SDBM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF SDBM-LATE-FEE-YTD NOT < WS-DRILL-GRID-LOW(WS-TX)
                   MOVE WS-DRILL-GRID-RATE(WS-TX) TO SDBM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DEPOSIT FEE ON THE DRILL YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3690-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN SDBM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO SDBT-DEPOSIT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDBM-EXEMPT-FLAG = 'Y'
                   COMPUTE SDBT-DEPOSIT-AMT ROUNDED = SDBM-DRILL-YTD *
                       SDBM-DISCOUNT / 200
               WHEN SDBT-TRAN-CD = 'RN'
                   MOVE 5.00 TO SDBT-DEPOSIT-AMT
               WHEN SDBT-TRAN-CD = 'AC'
                   MOVE 10.00 TO SDBT-DEPOSIT-AMT
               WHEN OTHER
                   COMPUTE SDBT-DEPOSIT-AMT ROUNDED = SDBM-DRILL-YTD *
                       SDBM-DISCOUNT / 100
           END-EVALUATE
           IF SDBT-DEPOSIT-AMT > 0 AND SDBT-DEPOSIT-AMT < 1.00
               MOVE 1.00 TO SDBT-DEPOSIT-AMT
           END-IF
           IF SDBT-DEPOSIT-AMT > 150.00
               MOVE 150.00 TO SDBT-DEPOSIT-AMT
           END-IF
           ADD SDBT-DEPOSIT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE VAULT CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3700-DESCRIBE-VAULT.
           EVALUATE SDBM-VAULT-CD
               WHEN 'BU'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'ET'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'FF'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'NG'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'NH'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E741 NAME REQUIRED
      * E251 DEPOSIT YTD NEGATIVE
      * E606 DEPOSIT YTD OVER 250000
      * E246 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3710-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDBM-NAME = SPACES
                   MOVE 'E741' TO WS-REJECT-CODE
               WHEN SDBM-DEPOSIT-YTD < 0
                   MOVE 'E251' TO WS-REJECT-CODE
               WHEN SDBM-DEPOSIT-YTD > 250000.00
                   MOVE 'E606' TO WS-REJECT-CODE
               WHEN SDBM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E246' TO WS-REJECT-CODE
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
       3720-DESCRIBE-REASON.
           EVALUATE SDBM-REASON-CD
               WHEN 'J'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'K'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'L'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'M'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'S'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'W'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'Y'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT DRILL AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3730-SPLIT-DRILL.
           COMPUTE WS-WORK-AMT = SDBM-DRILL-AMT / 12
           COMPUTE WS-WORK-AMT-2 = SDBM-DRILL-AMT - (WS-WORK-AMT * 12)
           COMPUTE SDBT-DEPOSIT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PREPAID YTD FEE ON THE DEPOSIT AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3740-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN SDBM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO SDBM-PREPAID-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDBM-PREFERRED-FLAG = 'Y'
                   COMPUTE SDBM-PREPAID-YTD ROUNDED = SDBT-DEPOSIT-AMT *
                       SDBM-FEE-PCT / 200
               WHEN SDBM-REASON-CD = 'J'
                   MOVE 3.00 TO SDBM-PREPAID-YTD
               WHEN SDBM-REASON-CD = 'K'
                   MOVE 15.00 TO SDBM-PREPAID-YTD
               WHEN OTHER
                   COMPUTE SDBM-PREPAID-YTD ROUNDED = SDBT-DEPOSIT-AMT *
                       SDBM-FEE-PCT / 100
           END-EVALUATE
           IF SDBM-PREPAID-YTD > 0 AND SDBM-PREPAID-YTD < 10.00
               MOVE 10.00 TO SDBM-PREPAID-YTD
           END-IF
           IF SDBM-PREPAID-YTD > 50.00
               MOVE 50.00 TO SDBM-PREPAID-YTD
           END-IF
           ADD SDBM-PREPAID-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PREPAID YTD IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3750-LIMIT-PREPAID.
           IF SDBM-PREPAID-YTD < 10.00
               MOVE 10.00 TO SDBM-PREPAID-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SDBM-PREPAID-YTD > 10000.00
               MOVE 10000.00 TO SDBM-PREPAID-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * J -> K WHEN LAST STMT DATE IS PAST
      * K -> L WHEN LAST STMT DATE IS PAST
      * L -> M WHEN LAST STMT DATE IS PAST
      * M -> S WHEN LAST STMT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3760-SET-REASON.
           EVALUATE SDBM-REASON-CD
               WHEN 'J'
                   IF SDBM-LAST-STMT-DATE > 0 AND SDBM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'K' TO SDBM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF SDBM-LAST-STMT-DATE > 0 AND SDBM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO SDBM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF SDBM-LAST-STMT-DATE > 0 AND SDBM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO SDBM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF SDBM-LAST-STMT-DATE > 0 AND SDBM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO SDBM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3770-DESCRIBE-CYCLE.
           EVALUATE SDBM-CYCLE-CD
               WHEN 'AP'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'DA'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'HV'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'SE'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * AP -> DA WHEN CLOSE DATE IS PAST
      * DA -> HV WHEN CLOSE DATE IS PAST
      * HV -> SE WHEN CLOSE DATE IS PAST
      * SE -> AP WHEN CLOSE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3780-SET-CYCLE.
           EVALUATE SDBM-CYCLE-CD
               WHEN 'AP'
                   IF SDBM-CLOSE-DATE > 0 AND SDBM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DA' TO SDBM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DA'
                   IF SDBM-CLOSE-DATE > 0 AND SDBM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HV' TO SDBM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HV'
                   IF SDBM-CLOSE-DATE > 0 AND SDBM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SE' TO SDBM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SE'
                   IF SDBM-CLOSE-DATE > 0 AND SDBM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AP' TO SDBM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON KEY FEE YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3790-ACCRUE-KEY-FEE.
           IF SDBM-KEY-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBM-KEY-FEE-YTD * SDBM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO SDBM-PREPAID-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DEPOSIT YTD.
      *----------------------------------------------------------------*
       3800-PRORATE-DEPOSIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDBM-DEPOSIT-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SDBM-DEPOSIT-YTD
           END-IF
           ADD SDBM-DEPOSIT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE DRILL BAND TIER FOR THE PREPAID AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3810-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-DRILL-BAND-RATE(1) TO SDBM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF SDBM-PREPAID-AMT NOT < WS-DRILL-BAND-LOW(WS-TX)
                   MOVE WS-DRILL-BAND-RATE(WS-TX) TO SDBM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BOX SIZE CD:
      * CU -> MG WHEN CLOSE DATE IS PAST
      * MG -> VG WHEN CLOSE DATE IS PAST
      * VG -> VN WHEN CLOSE DATE IS PAST
      * VN -> XJ WHEN CLOSE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3820-SET-BOX-SIZE.
           EVALUATE SDBM-BOX-SIZE-CD
               WHEN 'CU'
                   IF SDBM-CLOSE-DATE > 0 AND SDBM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MG' TO SDBM-BOX-SIZE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MG'
                   IF SDBM-CLOSE-DATE > 0 AND SDBM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VG' TO SDBM-BOX-SIZE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VG'
                   IF SDBM-CLOSE-DATE > 0 AND SDBM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VN' TO SDBM-BOX-SIZE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VN'
                   IF SDBM-CLOSE-DATE > 0 AND SDBM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XJ' TO SDBM-BOX-SIZE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE PREPAID AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO KEY FEE YTD.
      *----------------------------------------------------------------*
       3830-PRORATE-KEY-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDBM-KEY-FEE-YTD ROUNDED =
                   SDBM-PREPAID-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SDBM-KEY-FEE-YTD
           END-IF
           ADD SDBM-KEY-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE DRILL GRID TIER FOR THE LATE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3840-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DRILL-GRID-RATE(1) TO SDBM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF SDBM-LATE-FEE-AMT NOT < WS-DRILL-GRID-LOW(WS-TX)
                   MOVE WS-DRILL-GRID-RATE(WS-TX) TO SDBM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * DD -> JZ WHEN NEXT DUE DATE IS PAST
      * JZ -> PS WHEN NEXT DUE DATE IS PAST
      * PS -> UM WHEN NEXT DUE DATE IS PAST
      * UM -> DD WHEN NEXT DUE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3850-SET-SOURCE.
           EVALUATE SDBM-SOURCE-CD
               WHEN 'DD'
                   IF SDBM-NEXT-DUE-DATE > 0 AND SDBM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JZ' TO SDBM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JZ'
                   IF SDBM-NEXT-DUE-DATE > 0 AND SDBM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PS' TO SDBM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PS'
                   IF SDBM-NEXT-DUE-DATE > 0 AND SDBM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UM' TO SDBM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UM'
                   IF SDBM-NEXT-DUE-DATE > 0 AND SDBM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DD' TO SDBM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE KEY FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO DEPOSIT AMT.
      *----------------------------------------------------------------*
       3860-PRORATE-DEPOSIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDBT-DEPOSIT-AMT ROUNDED =
                   SDBM-KEY-FEE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SDBT-DEPOSIT-AMT
           END-IF
           ADD SDBT-DEPOSIT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE DEPOSIT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DRILL AMT.
      *----------------------------------------------------------------*
       3870-PRORATE-DRILL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDBM-DRILL-AMT ROUNDED =
                   SDBM-DEPOSIT-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SDBM-DRILL-AMT
           END-IF
           ADD SDBM-DRILL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON KEY FEE YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3880-ACCRUE-KEY-FEE.
           IF SDBM-KEY-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBM-KEY-FEE-YTD * SDBM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO SDBM-PREPAID-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3890-COUNT-NOTICE.
           IF SDBM-NOTICE-CNT < 9999999
               ADD 1 TO SDBM-NOTICE-CNT
           END-IF
           IF SDBM-NOTICE-CNT NOT < 6
               MOVE 'Y' TO SDBM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT RENT AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3900-SPLIT-RENT.
           COMPUTE WS-WORK-AMT = SDBT-RENT-AMT / 2
           COMPUTE WS-WORK-AMT-2 = SDBT-RENT-AMT - (WS-WORK-AMT * 2)
           COMPUTE SDBT-DRILL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3910-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF SDBM-CLOSE-DATE > 0 AND SDBM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDBM-CLOSE-DATE TO DTU-DATE-1
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
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3920-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF SDBM-LAST-MAINT-DATE > 0 AND SDBM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDBM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3930-DESCRIBE-CYCLE.
           EVALUATE SDBM-CYCLE-CD
               WHEN 'AP'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'DA'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'HV'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'SE'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3940-COUNT-NOTICE.
           IF SDBM-NOTICE-CNT < 9999999
               ADD 1 TO SDBM-NOTICE-CNT
           END-IF
           IF SDBM-NOTICE-CNT NOT < 3
               MOVE 'Y' TO SDBM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 60 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3950-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF SDBM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDBM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO SDBT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE LATE FEE AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3960-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDBM-CYCLE-CD
                   ADD SDBM-LATE-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDBM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDBM-LATE-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PREPAID YTD IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3970-LIMIT-PREPAID.
           IF SDBM-PREPAID-YTD < 50.00
               MOVE 50.00 TO SDBM-PREPAID-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SDBM-PREPAID-YTD > 2500.00
               MOVE 2500.00 TO SDBM-PREPAID-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3980-COUNT-NOTICE.
           IF SDBM-NOTICE-CNT < 9999999
               ADD 1 TO SDBM-NOTICE-CNT
           END-IF
           IF SDBM-NOTICE-CNT NOT < 10
               MOVE 'Y' TO SDBM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BOX SIZE CD:
      * CU -> MG WHEN LAST STMT DATE IS PAST
      * MG -> VG WHEN LAST STMT DATE IS PAST
      * VG -> VN WHEN LAST STMT DATE IS PAST
      * VN -> XJ WHEN LAST STMT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3990-SET-BOX-SIZE.
           EVALUATE SDBM-BOX-SIZE-CD
               WHEN 'CU'
                   IF SDBM-LAST-STMT-DATE > 0 AND SDBM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MG' TO SDBM-BOX-SIZE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MG'
                   IF SDBM-LAST-STMT-DATE > 0 AND SDBM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VG' TO SDBM-BOX-SIZE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VG'
                   IF SDBM-LAST-STMT-DATE > 0 AND SDBM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VN' TO SDBM-BOX-SIZE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VN'
                   IF SDBM-LAST-STMT-DATE > 0 AND SDBM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XJ' TO SDBM-BOX-SIZE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT DRILL YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4000-SPLIT-DRILL.
           COMPUTE WS-WORK-AMT = SDBM-DRILL-YTD / 12
           COMPUTE WS-WORK-AMT-2 = SDBM-DRILL-YTD - (WS-WORK-AMT * 12)
           COMPUTE SDBM-DRILL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4010-COUNT-ITEM.
           IF SDBM-ITEM-CNT < 9999999
               ADD 1 TO SDBM-ITEM-CNT
           END-IF
           IF SDBM-ITEM-CNT NOT < 3
               MOVE 'Y' TO SDBM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PREPAID AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PREPAID YTD.
      *----------------------------------------------------------------*
       4020-PRORATE-PREPAID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDBM-PREPAID-YTD ROUNDED =
                   SDBM-PREPAID-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SDBM-PREPAID-YTD
           END-IF
           ADD SDBM-PREPAID-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4030-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF SDBM-LAST-STMT-DATE > 0 AND SDBM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDBM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SDBM-PREPAID-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD SDBM-PREPAID-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD SDBM-PREPAID-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SDBM-PREPAID-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDBM-PREPAID-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 15 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4040-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF SDBT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDBT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO SDBM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       4050-COUNT-NOTICE.
           IF SDBM-NOTICE-CNT < 9999999
               ADD 1 TO SDBM-NOTICE-CNT
           END-IF
           IF SDBM-NOTICE-CNT NOT < 6
               MOVE 'Y' TO SDBM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4060-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF SDBM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDBM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO SDBT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DRILL AMT IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4070-LIMIT-DRILL.
           IF SDBM-DRILL-AMT < 50.00
               MOVE 50.00 TO SDBM-DRILL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SDBM-DRILL-AMT > 10000.00
               MOVE 10000.00 TO SDBM-DRILL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 90 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4080-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF SDBT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDBT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO SDBM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4090-FLAG-ITEM.
           IF WS-WORK-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE DEPOSIT YTD BY VAULT CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4100-ACCUM-VAULT.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDBM-VAULT-CD
                   ADD SDBM-DEPOSIT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDBM-VAULT-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDBM-DEPOSIT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON LATE FEE AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4110-ACCRUE-LATE-FEE.
           IF SDBM-LATE-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBM-LATE-FEE-AMT * SDBM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO SDBM-PREPAID-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DRILL AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4120-ACCRUE-DRILL.
           IF SDBM-DRILL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBM-DRILL-AMT * SDBM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO SDBM-LATE-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * LATE FEE YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4130-FLAG-LATE-FEE.
           IF SDBM-LATE-FEE-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE SDBM-LATE-FEE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       4140-COUNT-ITEM.
           IF SDBM-ITEM-CNT < 9999999
               ADD 1 TO SDBM-ITEM-CNT
           END-IF
           IF SDBM-ITEM-CNT NOT < 3
               MOVE 'Y' TO SDBM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PREPAID AMT BY VAULT CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4150-ACCUM-VAULT.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDBM-VAULT-CD
                   ADD SDBM-PREPAID-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDBM-VAULT-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDBM-PREPAID-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PREPAID AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4160-ACCRUE-PREPAID.
           IF SDBM-PREPAID-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBM-PREPAID-AMT * SDBM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4170-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF SDBM-LAST-STMT-DATE > 0 AND SDBM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDBM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SDBM-DRILL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD SDBM-DRILL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD SDBM-DRILL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD SDBM-DRILL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDBM-DRILL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E374 REJECT CD REQUIRED
      * E441 DRILL AMT NEGATIVE
      * E251 DRILL AMT OVER 50000
      * E995 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       4180-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDBT-REJECT-CD = SPACES
                   MOVE 'E374' TO WS-REJECT-CODE
               WHEN SDBT-DRILL-AMT < 0
                   MOVE 'E441' TO WS-REJECT-CODE
               WHEN SDBT-DRILL-AMT > 50000.00
                   MOVE 'E251' TO WS-REJECT-CODE
               WHEN SDBM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E995' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BOX SIZE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4190-DESCRIBE-BOX-SIZE.
           EVALUATE SDBM-BOX-SIZE-CD
               WHEN 'CU'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'MG'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'VG'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'VN'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'XJ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4200-COUNT-ITEM.
           IF SDBM-ITEM-CNT < 9999999
               ADD 1 TO SDBM-ITEM-CNT
           END-IF
           IF SDBM-ITEM-CNT NOT < 6
               MOVE 'Y' TO SDBM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RENT YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4210-FLAG-RENT.
           IF SDBM-RENT-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE SDBM-RENT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BOX SIZE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4220-DESCRIBE-BOX-SIZE.
           EVALUATE SDBM-BOX-SIZE-CD
               WHEN 'CU'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'MG'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'VG'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'VN'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'XJ'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE BOX SIZE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4230-DESCRIBE-BOX-SIZE.
           EVALUATE SDBM-BOX-SIZE-CD
               WHEN 'CU'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'MG'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'VG'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'VN'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'XJ'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E261 DESC REQUIRED
      * E607 KEY FEE YTD NEGATIVE
      * E411 KEY FEE YTD OVER 50000
      * E969 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       4240-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDBM-DESC = SPACES
                   MOVE 'E261' TO WS-REJECT-CODE
               WHEN SDBM-KEY-FEE-YTD < 0
                   MOVE 'E607' TO WS-REJECT-CODE
               WHEN SDBM-KEY-FEE-YTD > 50000.00
                   MOVE 'E411' TO WS-REJECT-CODE
               WHEN SDBM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E969' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PREPAID TIER TIER FOR THE DEPOSIT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4250-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-PREPAID-TIER-RATE(1) TO SDBM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF SDBM-DEPOSIT-YTD NOT < WS-PREPAID-TIER-LOW(WS-TX)
                   MOVE WS-PREPAID-TIER-RATE(WS-TX) TO SDBM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 365 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4260-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF SDBT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDBT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO SDBM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * KEY FEE AMT IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4270-LIMIT-KEY-FEE.
           IF SDBT-KEY-FEE-AMT < 25.00
               MOVE 25.00 TO SDBT-KEY-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SDBT-KEY-FEE-AMT > 1000.00
               MOVE 1000.00 TO SDBT-KEY-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4280-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF SDBM-LAST-MAINT-DATE > 0 AND SDBM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDBM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SDBM-KEY-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD SDBM-KEY-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD SDBM-KEY-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD SDBM-KEY-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDBM-KEY-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4290-SPLIT-DEPOSIT.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 2)
           COMPUTE SDBM-DEPOSIT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E675 REF NO REQUIRED
      * E618 KEY FEE AMT NEGATIVE
      * E106 KEY FEE AMT OVER 1000000
      * E506 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       4300-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDBM-REF-NO = SPACES
                   MOVE 'E675' TO WS-REJECT-CODE
               WHEN SDBM-KEY-FEE-AMT < 0
                   MOVE 'E618' TO WS-REJECT-CODE
               WHEN SDBM-KEY-FEE-AMT > 1000000.00
                   MOVE 'E106' TO WS-REJECT-CODE
               WHEN SDBM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E506' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DRILL YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4310-FLAG-DRILL.
           IF SDBM-DRILL-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE SDBM-DRILL-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * KEY FEE YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4320-FLAG-KEY-FEE.
           IF SDBM-KEY-FEE-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE SDBM-KEY-FEE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DRILL BAND TIER FOR THE LATE FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4330-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DRILL-BAND-RATE(1) TO SDBM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF SDBM-LATE-FEE-YTD NOT < WS-DRILL-BAND-LOW(WS-TX)
                   MOVE WS-DRILL-BAND-RATE(WS-TX) TO SDBM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RENT AMT IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4340-LIMIT-RENT.
           IF SDBT-RENT-AMT < 50.00
               MOVE 50.00 TO SDBT-RENT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SDBT-RENT-AMT > 1000.00
               MOVE 1000.00 TO SDBT-RENT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4350-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF SDBM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDBM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO SDBM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE RENT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       4360-PRORATE-RENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   SDBT-RENT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E258 OPERATOR REQUIRED
      * E755 DRILL AMT NEGATIVE
      * E777 DRILL AMT OVER 1000000
      * E597 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       4370-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDBT-OPERATOR = SPACES
                   MOVE 'E258' TO WS-REJECT-CODE
               WHEN SDBT-DRILL-AMT < 0
                   MOVE 'E755' TO WS-REJECT-CODE
               WHEN SDBT-DRILL-AMT > 1000000.00
                   MOVE 'E777' TO WS-REJECT-CODE
               WHEN SDBM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E597' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RENT AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4380-FLAG-RENT.
           IF SDBM-RENT-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE SDBM-RENT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E649 REF NO REQUIRED
      * E315 DRILL YTD NEGATIVE
      * E782 DRILL YTD OVER 99999
      * E235 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       4390-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDBM-REF-NO = SPACES
                   MOVE 'E649' TO WS-REJECT-CODE
               WHEN SDBM-DRILL-YTD < 0
                   MOVE 'E315' TO WS-REJECT-CODE
               WHEN SDBM-DRILL-YTD > 99999.00
                   MOVE 'E782' TO WS-REJECT-CODE
               WHEN SDBT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E235' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 15 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4400-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF SDBM-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDBM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO SDBM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE RENT YTD BY BOX SIZE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4410-ACCUM-BOX-SIZE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDBM-BOX-SIZE-CD
                   ADD SDBM-RENT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDBM-BOX-SIZE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDBM-RENT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE SDB FIELDS TO SDWAIV AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7040-CALL-SDWAIV.
           INITIALIZE SDWAIV-PARMS
           MOVE SDBM-DEPOSIT-YTD
             TO SDWAIV-DEPOSIT-YTD
           MOVE SDBM-DRILL-AMT
             TO SDWAIV-DRILL-AMT
           MOVE SDBM-RENT-YTD
             TO SDWAIV-RENT-YTD
           MOVE SDBM-LATE-FEE-AMT
             TO SDWAIV-LATE-FEE-AMT
           MOVE SDBM-DRILL-YTD
             TO SDWAIV-DRILL-YTD
           MOVE SDBM-KEY-FEE-YTD
             TO SDWAIV-KEY-FEE-YTD
           MOVE SDBM-NEXT-DUE-DATE
             TO SDWAIV-NEXT-DUE-DATE
           MOVE SDBM-OPEN-DATE
             TO SDWAIV-OPEN-DATE
           MOVE SDBM-MATURITY-DATE
             TO SDWAIV-MATURITY-DATE
           MOVE SDBM-CLOSE-DATE
             TO SDWAIV-CLOSE-DATE
           MOVE SDBM-VAULT-CD
             TO SDWAIV-VAULT-CD
           MOVE SDBM-SOURCE-CD
             TO SDWAIV-SOURCE-CD
           MOVE SDBM-BOX-SIZE-CD
             TO SDWAIV-BOX-SIZE-CD
           MOVE SDBM-BRANCH-CD
             TO SDWAIV-BRANCH-CD
           MOVE SDBM-SUSPEND-FLAG
             TO SDWAIV-SUSPEND-FLAG
           MOVE SDBM-REVIEW-FLAG
             TO SDWAIV-REVIEW-FLAG
           MOVE SDBM-OVERRIDE-FLAG
             TO SDWAIV-OVERRIDE-FLAG
           MOVE SDBM-PAYMENT-CNT
             TO SDWAIV-PAYMENT-CNT
           MOVE SDBM-NOTICE-CNT
             TO SDWAIV-NOTICE-CNT
           MOVE SDBM-PENALTY-PCT
             TO SDWAIV-PENALTY-PCT
           MOVE SDBM-FEE-PCT
             TO SDWAIV-FEE-PCT
           MOVE SDBM-CUST-ID
             TO SDWAIV-CUST-ID
           MOVE SDBM-NAME
             TO SDWAIV-NAME
           CALL 'SDWAIV' USING SDWAIV-PARMS
           EVALUATE SDWAIV-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE SDWAIV-DRILL-AMT
                     TO SDBM-DRILL-AMT
                   MOVE SDWAIV-REVIEW-FLAG
                     TO SDBM-REVIEW-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RAIV' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7040-CALL-SDWAIV' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE SDB FIELDS TO SDLIM AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7020-CALL-SDLIM.
           INITIALIZE SDLIM-PARMS
           MOVE SDBM-RENT-YTD
             TO SDLIM-RENT-YTD
           MOVE SDBM-DEPOSIT-YTD
             TO SDLIM-DEPOSIT-YTD
           MOVE SDBM-LATE-FEE-YTD
             TO SDLIM-LATE-FEE-YTD
           MOVE SDBM-DEPOSIT-AMT
             TO SDLIM-DEPOSIT-AMT
           MOVE SDBM-LATE-FEE-AMT
             TO SDLIM-LATE-FEE-AMT
           MOVE SDBM-DRILL-YTD
             TO SDLIM-DRILL-YTD
           MOVE SDBM-LAST-ACT-DATE
             TO SDLIM-LAST-ACT-DATE
           MOVE SDBM-OPEN-DATE
             TO SDLIM-OPEN-DATE
           MOVE SDBM-NEXT-DUE-DATE
             TO SDLIM-NEXT-DUE-DATE
           MOVE SDBM-NOTICE-DATE
             TO SDLIM-NOTICE-DATE
           MOVE SDBM-CYCLE-CD
             TO SDLIM-CYCLE-CD
           MOVE SDBM-BRANCH-CD
             TO SDLIM-BRANCH-CD
           MOVE SDBM-REASON-CD
             TO SDLIM-REASON-CD
           MOVE SDBM-BOX-SIZE-CD
             TO SDLIM-BOX-SIZE-CD
           MOVE SDBM-OVERRIDE-FLAG
             TO SDLIM-OVERRIDE-FLAG
           MOVE SDBM-EXEMPT-FLAG
             TO SDLIM-EXEMPT-FLAG
           MOVE SDBM-REVIEW-FLAG
             TO SDLIM-REVIEW-FLAG
           MOVE SDBM-RETRY-CNT
             TO SDLIM-RETRY-CNT
           MOVE SDBM-NOTICE-CNT
             TO SDLIM-NOTICE-CNT
           MOVE SDBM-DISCOUNT
             TO SDLIM-DISCOUNT
           MOVE SDBM-FEE-PCT
             TO SDLIM-FEE-PCT
           MOVE SDBM-CONTACT
             TO SDLIM-CONTACT
           MOVE SDBM-ADDR
             TO SDLIM-ADDR
           CALL 'SDLIM' USING SDLIM-PARMS
           EVALUATE SDLIM-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE SDLIM-DEPOSIT-YTD
                     TO SDBM-DEPOSIT-YTD
                   MOVE SDLIM-LATE-FEE-AMT
                     TO SDBM-LATE-FEE-AMT
                   MOVE SDLIM-DRILL-YTD
                     TO SDBM-DRILL-YTD
                   CONTINUE
               WHEN 08
                   MOVE 'RLIM' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-SDLIM' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE SDBT-KEY TO RD-KEY
           MOVE SDBT-TRAN-CD TO RD-CODE
           MOVE SDBT-KEY-FEE-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE SDBT-KEY TO RD-KEY
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
           DISPLAY 'SDBAUDT READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'SDBAUDT' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
