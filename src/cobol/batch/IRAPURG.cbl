      *================================================================*
      * PROGRAM  : IRAPURG                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : IRA CUSTODY - PURGE AND ARCHIVE. READS THE IRA      *
      *            TRANSACTIONS, UPDATES THE IRA MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. INDIVIDUAL RETIREMENT ACCOUNT   *
      *            CONTRIBUTIONS, DISTRIBUTIONS AND 5498               *
      * INPUT    : IRAMST (KSDS), IRATRN, BANKCTL                      *
      * OUTPUT   : IRAOUT, IRAPURPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, IRELG, IRPRC            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-04-15 JLM  ORIGINAL                                       *
      * 1996-03-21 SKN  CONTRIB STATUS CODES ADDED REQ2797             *
      * 1997-01-10 LMB  ROLLOVER RERUN PROTECTION CHG7481              *
      * 1997-07-28 CRP  PERFORMANCE - WITHHOLD READ AHEAD PRB9389      *
      * 2003-12-04 SKN  WITHHOLD LIMIT REVISED INC2538                 *
      * 2003-12-22 TWH  DISTRIB CUTOFF MOVED REQ7908                   *
      * 2004-04-05 BHS  DISTRIB TABLE EXPANDED PRB6718                 *
      * 2004-09-19 TWH  ROLLOVER EXCEPTIONS TO OPS QUEUE REQ9635       *
      * 2009-08-11 PQR  EXCESS STATUS CODES ADDED REQ5788              *
      * 2009-10-17 MKL  NEW DISTRIB REPORT COLUMNS REQ7125             *
      * 2011-12-09 PQR  EXCESS CUTOFF MOVED PRB7957                    *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. IRAPURG.
       AUTHOR. B H SANDERS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "IRAMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS IRAM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "IRATRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "IRAOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "IRAPURPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY IRAMST.
       FD  TRAN-FILE.
       COPY IRATRN.
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
      * WS-CONTRIB-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CONTRIB-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000509000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000584000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002600000000368000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003100000000451500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003200000000745000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003700000000854000'.
       01  WS-CONTRIB-SCHED-TBL REDEFINES WS-CONTRIB-SCHED-VALUES.
           05  WS-CONTRIB-SCHED-ROW OCCURS 6 TIMES.
               10  WS-CONTRIB-SCHED-LOW    PIC 9(09)V99.
               10  WS-CONTRIB-SCHED-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-DISTRIB-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-DISTRIB-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000252000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000232500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001500000000666500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011500000000732000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016500000000734500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026500000000465500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00036500000000306000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00036550000000140500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00036800000000203500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00036850000000420000'.
       01  WS-DISTRIB-BAND-TBL REDEFINES WS-DISTRIB-BAND-VALUES.
           05  WS-DISTRIB-BAND-ROW OCCURS 10 TIMES.
               10  WS-DISTRIB-BAND-LOW     PIC 9(09)V99.
               10  WS-DISTRIB-BAND-RATE    PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  IRAPURG  IRA CUSTODY'.
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
       COPY IRELGP.
       COPY IRPRCP.

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
               MOVE 'IRAMST' TO ABND-FILE-NAME
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
           MOVE IRAT-KEY TO IRAM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-PRORATE-CONTRIB
               PERFORM 3010-LIMIT-RMD
               PERFORM 3020-AGE-POST
               PERFORM 3030-ACCRUE-WITHHOLD
               PERFORM 3040-PRORATE-FMV
               PERFORM 3050-CHECK-EXPIRY
               PERFORM 3060-PRORATE-EXCESS
               PERFORM 3070-LIMIT-ROLLOVER
               PERFORM 3080-FLAG-ITEM
               PERFORM 3090-SPLIT-EXCESS
               PERFORM 3100-CHECK-LAST-STMT
               PERFORM 3110-COUNT-RETRY
               PERFORM 3120-ASSESS-REVERSAL
               PERFORM 3130-COUNT-ITEM
               PERFORM 3140-DESCRIBE-TYPE
               PERFORM 3150-CHECK-EFF
               PERFORM 3160-SET-TYPE
               PERFORM 3170-ACCUM-CHANNEL
               PERFORM 3180-EDIT-ADDR
               PERFORM 3190-FLAG-ITEM
               PERFORM 3200-SET-CHANNEL
               PERFORM 3210-EDIT-DESC
               PERFORM 3220-ACCUM-TRAN
               PERFORM 3230-LIMIT-DISTRIB
               PERFORM 3240-COUNT-CYCLE
               PERFORM 3250-DESCRIBE-TYPE
               PERFORM 3260-ACCRUE-EXCESS
               PERFORM 3270-AGE-EFF
               PERFORM 3280-EDIT-MEMO
               PERFORM 3290-SELECT-RATE
               PERFORM 3300-SPLIT-EXCESS
               PERFORM 3310-SPLIT-FMV
               PERFORM 3320-COUNT-RETRY
               PERFORM 3330-SELECT-FEE-PCT
               PERFORM 3340-SELECT-RATE
               PERFORM 3350-ACCRUE-DISTRIB
               PERFORM 3360-LIMIT-FMV
               PERFORM 3370-LIMIT-ROLLOVER
               PERFORM 3380-EDIT-REJECT
               PERFORM 3390-EDIT-REF-NO
               PERFORM 3400-ASSESS-AUTO-RENEW
               PERFORM 3410-DESCRIBE-REASON
               PERFORM 3420-COUNT-RETRY
               PERFORM 3430-COUNT-RETRY
               PERFORM 3440-SET-TYPE
               PERFORM 3450-AGE-MATURITY
               PERFORM 3460-SET-TRAN
               PERFORM 3470-SELECT-MARGIN
               PERFORM 3480-SELECT-MARGIN
               PERFORM 3490-DESCRIBE-TRAN
               PERFORM 3500-ACCUM-CHANNEL
               PERFORM 3510-PRORATE-CONTRIB
               PERFORM 3520-ASSESS-HOLD
               PERFORM 3530-ACCUM-REASON
               PERFORM 3540-SPLIT-RMD
               PERFORM 3550-SELECT-FEE-PCT
               PERFORM 3560-SPLIT-EXCESS
               PERFORM 3570-COUNT-PAYMENT
               PERFORM 3580-FLAG-FMV
               PERFORM 3590-ACCUM-CHANNEL
               PERFORM 3600-SET-TYPE
               PERFORM 3610-PRORATE-DISTRIB
               PERFORM 3620-SELECT-MARGIN
               PERFORM 3630-CHECK-LAST-STMT
               PERFORM 3640-SET-TRAN
               PERFORM 3650-COUNT-PAYMENT
               PERFORM 3660-ACCRUE-ROLLOVER
               PERFORM 3670-ACCRUE-RMD
               PERFORM 3680-SET-REGION
               PERFORM 3690-AGE-LAST-ACT
               PERFORM 3700-DESCRIBE-IRA-TYPE
               PERFORM 3710-ACCRUE-DISTRIB
               PERFORM 3720-EDIT-DESC
               PERFORM 3730-COUNT-CYCLE
               PERFORM 3740-COUNT-ITEM
               PERFORM 3750-AGE-POST
               PERFORM 3760-LIMIT-FMV
               PERFORM 3770-ASSESS-REVIEW
               PERFORM 3780-LIMIT-WITHHOLD
               PERFORM 3790-ACCRUE-DISTRIB
               PERFORM 3800-CHECK-NEXT-REVIEW
               PERFORM 3810-SET-DIST-CODE
               PERFORM 3820-ACCUM-TRAN
               PERFORM 3830-AGE-EFF
               PERFORM 3840-ACCUM-DIST-CODE
               PERFORM 3850-SPLIT-DISTRIB
               PERFORM 3860-SET-IRA-TYPE
               PERFORM 3870-CHECK-NOTICE
               PERFORM 3880-SELECT-FEE-PCT
               PERFORM 3890-SET-IRA-TYPE
               PERFORM 3900-FLAG-EXCESS
               PERFORM 3910-SELECT-RATE
               PERFORM 3920-EDIT-ADDR
               PERFORM 3930-ACCUM-CHANNEL
               PERFORM 3940-SET-IRA-TYPE
               PERFORM 3950-LIMIT-ROLLOVER
               PERFORM 3960-FLAG-ROLLOVER
               PERFORM 3970-LIMIT-DISTRIB
               PERFORM 3980-CHECK-EXPIRY
               PERFORM 3990-EDIT-REF-NO
               PERFORM 4000-ACCUM-CHANNEL
               PERFORM 4010-PRORATE-CONTRIB
               PERFORM 4020-FLAG-ROLLOVER
               PERFORM 4030-DESCRIBE-REASON
               PERFORM 4040-FLAG-WITHHOLD
               PERFORM 4050-SET-TYPE
               PERFORM 4060-ACCRUE-FMV
               PERFORM 4070-PRORATE-RMD
               PERFORM 4080-FLAG-EXCESS
               PERFORM 4090-ACCUM-REASON
               PERFORM 4100-AGE-EFFECTIVE
               PERFORM 4110-SPLIT-EXCESS
               PERFORM 4120-SET-TRAN
               PERFORM 4130-DESCRIBE-IRA-TYPE
               PERFORM 4140-SET-REGION
               PERFORM 4150-ACCUM-REGION
               PERFORM 4160-DESCRIBE-DIST-CODE
               PERFORM 4170-AGE-NOTICE
               PERFORM 4180-SET-REGION
               PERFORM 4190-COUNT-ITEM
               PERFORM 4200-ACCUM-TYPE
               PERFORM 4210-LIMIT-ITEM
               PERFORM 4220-ACCUM-DIST-CODE
               PERFORM 4230-AGE-OPEN
               PERFORM 4240-FLAG-FMV
               PERFORM 4250-DESCRIBE-REASON
               PERFORM 4260-PRORATE-EXCESS
               PERFORM 4270-EDIT-REF-NO
               PERFORM 4280-DESCRIBE-DIST-CODE
               PERFORM 4290-ASSESS-REVIEW
               PERFORM 4300-DESCRIBE-DIST-CODE
               PERFORM 4310-FLAG-ITEM
               PERFORM 4320-COUNT-PAYMENT
               PERFORM 4330-PRORATE-RMD
               PERFORM 4340-SELECT-FEE-PCT
               PERFORM 4350-SPLIT-EXCESS
               PERFORM 4360-ACCUM-TRAN
               PERFORM 4370-AGE-OPEN
               PERFORM 4380-ACCRUE-ROLLOVER
               PERFORM 4390-FLAG-FMV
               PERFORM 4400-FLAG-WITHHOLD
               PERFORM 4410-PRORATE-WITHHOLD
               PERFORM 4420-SET-TRAN
               PERFORM 4430-ACCUM-CHANNEL
               PERFORM 4440-EDIT-REJECT
               PERFORM 4450-DESCRIBE-DIST-CODE
               PERFORM 4460-ACCRUE-RMD
               PERFORM 4470-FLAG-WITHHOLD
               PERFORM 4480-COUNT-PAYMENT
               PERFORM 4490-SELECT-RATE
               PERFORM 4500-AGE-POST
               PERFORM 4510-SET-IRA-TYPE
               PERFORM 4520-AGE-NEXT-REVIEW
               PERFORM 4530-EDIT-OPERATOR
               PERFORM 4540-PRORATE-EXCESS
               PERFORM 7020-CALL-IRELG
               PERFORM 7000-CALL-IRPRC
               REWRITE IRA-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'IRAMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM IRA-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * PRORATE RMD YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CONTRIB YTD.
      *----------------------------------------------------------------*
       3000-PRORATE-CONTRIB.
           IF WS-PERIOD-DAYS > 0
               COMPUTE IRAM-CONTRIB-YTD ROUNDED =
                   IRAM-RMD-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO IRAM-CONTRIB-YTD
           END-IF
           ADD IRAM-CONTRIB-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * RMD AMT IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3010-LIMIT-RMD.
           IF IRAM-RMD-AMT < 50.00
               MOVE 50.00 TO IRAM-RMD-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF IRAM-RMD-AMT > 10000.00
               MOVE 10000.00 TO IRAM-RMD-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3020-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF IRAT-POST-DATE > 0 AND IRAT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD IRAM-WITHHOLD-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD IRAM-WITHHOLD-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD IRAM-WITHHOLD-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD IRAM-WITHHOLD-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD IRAM-WITHHOLD-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WITHHOLD YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3030-ACCRUE-WITHHOLD.
           IF IRAM-WITHHOLD-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   IRAM-WITHHOLD-YTD * IRAM-RATE / 100 / 365
               ADD WS-WORK-AMT TO IRAT-RMD-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FMV YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3040-PRORATE-FMV.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   IRAM-FMV-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3050-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF IRAM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO IRAM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE EXCESS AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3060-PRORATE-EXCESS.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   IRAM-EXCESS-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ROLLOVER AMT IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3070-LIMIT-ROLLOVER.
           IF IRAM-ROLLOVER-AMT < 10.00
               MOVE 10.00 TO IRAM-ROLLOVER-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF IRAM-ROLLOVER-AMT > 50000.00
               MOVE 50000.00 TO IRAM-ROLLOVER-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3080-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT EXCESS YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3090-SPLIT-EXCESS.
           COMPUTE WS-WORK-AMT = IRAM-EXCESS-YTD / 4
           COMPUTE WS-WORK-AMT-2 = IRAM-EXCESS-YTD - (WS-WORK-AMT * 4)
           COMPUTE IRAM-RMD-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 365 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3100-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF IRAM-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO IRAM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3110-COUNT-RETRY.
           IF IRAM-RETRY-CNT < 9999999
               ADD 1 TO IRAM-RETRY-CNT
           END-IF
           IF IRAM-RETRY-CNT NOT < 3
               MOVE 'Y' TO IRAT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ROLLOVER FEE ON THE DISTRIB AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3120-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN IRAT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO IRAM-ROLLOVER-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN IRAM-HOLD-FLAG = 'Y'
                   COMPUTE IRAM-ROLLOVER-AMT ROUNDED = IRAT-DISTRIB-AMT
                       * IRAM-MARGIN / 200
               WHEN IRAT-CHANNEL-CD = 'BRN'
                   MOVE 10.00 TO IRAM-ROLLOVER-AMT
               WHEN IRAT-CHANNEL-CD = 'BAT'
                   MOVE 20.00 TO IRAM-ROLLOVER-AMT
               WHEN OTHER
                   COMPUTE IRAM-ROLLOVER-AMT ROUNDED = IRAT-DISTRIB-AMT
                       * IRAM-MARGIN / 100
           END-EVALUATE
           IF IRAM-ROLLOVER-AMT > 0 AND IRAM-ROLLOVER-AMT < 10.00
               MOVE 10.00 TO IRAM-ROLLOVER-AMT
           END-IF
           IF IRAM-ROLLOVER-AMT > 50.00
               MOVE 50.00 TO IRAM-ROLLOVER-AMT
           END-IF
           ADD IRAM-ROLLOVER-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3130-COUNT-ITEM.
           IF IRAM-ITEM-CNT < 9999999
               ADD 1 TO IRAM-ITEM-CNT
           END-IF
           IF IRAM-ITEM-CNT NOT < 10
               MOVE 'Y' TO IRAM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3140-DESCRIBE-TYPE.
           EVALUATE IRAM-TYPE-CD
               WHEN 'EA'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'HR'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'XZ'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 365 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3150-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF IRAT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO IRAM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * EA -> HR WHEN EXPIRY DATE IS PAST
      * HR -> XZ WHEN EXPIRY DATE IS PAST
      * XZ -> EA WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3160-SET-TYPE.
           EVALUATE IRAM-TYPE-CD
               WHEN 'EA'
                   IF IRAM-EXPIRY-DATE > 0 AND IRAM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HR' TO IRAM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HR'
                   IF IRAM-EXPIRY-DATE > 0 AND IRAM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XZ' TO IRAM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XZ'
                   IF IRAM-EXPIRY-DATE > 0 AND IRAM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EA' TO IRAM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE ROLLOVER AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3170-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRAT-CHANNEL-CD
                   ADD IRAT-ROLLOVER-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRAT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE IRAT-ROLLOVER-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E504 ADDR REQUIRED
      * E039 EXCESS AMT NEGATIVE
      * E188 EXCESS AMT OVER 10000
      * E607 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3180-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN IRAM-ADDR = SPACES
                   MOVE 'E504' TO WS-REJECT-CODE
               WHEN IRAM-EXCESS-AMT < 0
                   MOVE 'E039' TO WS-REJECT-CODE
               WHEN IRAM-EXCESS-AMT > 10000.00
                   MOVE 'E188' TO WS-REJECT-CODE
               WHEN IRAM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E607' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3190-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN NEXT DUE DATE IS PAST
      * BAT -> MAL WHEN NEXT DUE DATE IS PAST
      * MAL -> TEL WHEN NEXT DUE DATE IS PAST
      * TEL -> ONL WHEN NEXT DUE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3200-SET-CHANNEL.
           EVALUATE IRAT-CHANNEL-CD
               WHEN 'BRN'
                   IF IRAM-NEXT-DUE-DATE > 0 AND IRAM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO IRAT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF IRAM-NEXT-DUE-DATE > 0 AND IRAM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO IRAT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF IRAM-NEXT-DUE-DATE > 0 AND IRAM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO IRAT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF IRAM-NEXT-DUE-DATE > 0 AND IRAM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO IRAT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E130 DESC REQUIRED
      * E998 ROLLOVER YTD NEGATIVE
      * E605 ROLLOVER YTD OVER 250000
      * E729 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3210-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN IRAM-DESC = SPACES
                   MOVE 'E130' TO WS-REJECT-CODE
               WHEN IRAM-ROLLOVER-YTD < 0
                   MOVE 'E998' TO WS-REJECT-CODE
               WHEN IRAM-ROLLOVER-YTD > 250000.00
                   MOVE 'E605' TO WS-REJECT-CODE
               WHEN IRAT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E729' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ROLLOVER YTD BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3220-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRAT-TRAN-CD
                   ADD IRAM-ROLLOVER-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRAT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE IRAM-ROLLOVER-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DISTRIB YTD IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3230-LIMIT-DISTRIB.
           IF IRAM-DISTRIB-YTD < 10.00
               MOVE 10.00 TO IRAM-DISTRIB-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF IRAM-DISTRIB-YTD > 1000.00
               MOVE 1000.00 TO IRAM-DISTRIB-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3240-COUNT-CYCLE.
           IF IRAM-CYCLE-CNT < 9999999
               ADD 1 TO IRAM-CYCLE-CNT
           END-IF
           IF IRAM-CYCLE-CNT NOT < 5
               MOVE 'Y' TO IRAT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3250-DESCRIBE-TYPE.
           EVALUATE IRAM-TYPE-CD
               WHEN 'EA'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'HR'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'XZ'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON EXCESS AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3260-ACCRUE-EXCESS.
           IF IRAT-EXCESS-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   IRAT-EXCESS-AMT * IRAM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO IRAM-RMD-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3270-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF IRAT-EFF-DATE > 0 AND IRAT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD IRAM-CONTRIB-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD IRAM-CONTRIB-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD IRAM-CONTRIB-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD IRAM-CONTRIB-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD IRAM-CONTRIB-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E156 MEMO REQUIRED
      * E679 FMV YTD NEGATIVE
      * E187 FMV YTD OVER 1000000
      * E950 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3280-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN IRAT-MEMO = SPACES
                   MOVE 'E156' TO WS-REJECT-CODE
               WHEN IRAM-FMV-YTD < 0
                   MOVE 'E679' TO WS-REJECT-CODE
               WHEN IRAM-FMV-YTD > 1000000.00
                   MOVE 'E187' TO WS-REJECT-CODE
               WHEN IRAM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E950' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DISTRIB BAND TIER FOR THE WITHHOLD YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3290-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-DISTRIB-BAND-RATE(1) TO IRAM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF IRAM-WITHHOLD-YTD NOT < WS-DISTRIB-BAND-LOW(WS-TX)
                   MOVE WS-DISTRIB-BAND-RATE(WS-TX) TO IRAM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF IRAM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO IRAM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT EXCESS AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3300-SPLIT-EXCESS.
           COMPUTE WS-WORK-AMT = IRAM-EXCESS-AMT / 4
           COMPUTE WS-WORK-AMT-2 = IRAM-EXCESS-AMT - (WS-WORK-AMT * 4)
           COMPUTE IRAM-ROLLOVER-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FMV AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3310-SPLIT-FMV.
           COMPUTE WS-WORK-AMT = IRAM-FMV-AMT / 4
           COMPUTE WS-WORK-AMT-2 = IRAM-FMV-AMT - (WS-WORK-AMT * 4)
           COMPUTE IRAM-WITHHOLD-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3320-COUNT-RETRY.
           IF IRAM-RETRY-CNT < 9999999
               ADD 1 TO IRAM-RETRY-CNT
           END-IF
           IF IRAM-RETRY-CNT NOT < 5
               MOVE 'Y' TO IRAM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CONTRIB SCHED TIER FOR THE CONTRIB AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3330-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CONTRIB-SCHED-RATE(1) TO IRAM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF IRAM-CONTRIB-AMT NOT < WS-CONTRIB-SCHED-LOW(WS-TX)
                   MOVE WS-CONTRIB-SCHED-RATE(WS-TX) TO IRAM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF IRAM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO IRAM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CONTRIB SCHED TIER FOR THE WITHHOLD AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3340-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CONTRIB-SCHED-RATE(1) TO IRAM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF IRAM-WITHHOLD-AMT NOT < WS-CONTRIB-SCHED-LOW(WS-TX)
                   MOVE WS-CONTRIB-SCHED-RATE(WS-TX) TO IRAM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF IRAM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO IRAM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DISTRIB AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3350-ACCRUE-DISTRIB.
           IF IRAM-DISTRIB-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   IRAM-DISTRIB-AMT * IRAM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FMV YTD IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3360-LIMIT-FMV.
           IF IRAM-FMV-YTD < 0.00
               MOVE 0.00 TO IRAM-FMV-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF IRAM-FMV-YTD > 1000.00
               MOVE 1000.00 TO IRAM-FMV-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ROLLOVER AMT IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3370-LIMIT-ROLLOVER.
           IF IRAM-ROLLOVER-AMT < 50.00
               MOVE 50.00 TO IRAM-ROLLOVER-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF IRAM-ROLLOVER-AMT > 1000.00
               MOVE 1000.00 TO IRAM-ROLLOVER-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E752 REJECT CD REQUIRED
      * E344 DISTRIB AMT NEGATIVE
      * E322 DISTRIB AMT OVER 99999
      * E056 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3380-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN IRAT-REJECT-CD = SPACES
                   MOVE 'E752' TO WS-REJECT-CODE
               WHEN IRAT-DISTRIB-AMT < 0
                   MOVE 'E344' TO WS-REJECT-CODE
               WHEN IRAT-DISTRIB-AMT > 99999.00
                   MOVE 'E322' TO WS-REJECT-CODE
               WHEN IRAM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E056' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E426 REF NO REQUIRED
      * E481 WITHHOLD YTD NEGATIVE
      * E682 WITHHOLD YTD OVER 1000000
      * E121 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3390-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN IRAM-REF-NO = SPACES
                   MOVE 'E426' TO WS-REJECT-CODE
               WHEN IRAM-WITHHOLD-YTD < 0
                   MOVE 'E481' TO WS-REJECT-CODE
               WHEN IRAM-WITHHOLD-YTD > 1000000.00
                   MOVE 'E682' TO WS-REJECT-CODE
               WHEN IRAM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E121' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DISTRIB FEE ON THE WITHHOLD AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3400-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN IRAM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO IRAT-DISTRIB-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN IRAM-STOP-FLAG = 'Y'
                   COMPUTE IRAT-DISTRIB-AMT ROUNDED = IRAM-WITHHOLD-AMT
                       * IRAM-RATE / 200
               WHEN IRAM-CHANNEL-CD = 'AS'
                   MOVE 12.00 TO IRAT-DISTRIB-AMT
               WHEN IRAM-CHANNEL-CD = 'AY'
                   MOVE 12.00 TO IRAT-DISTRIB-AMT
               WHEN OTHER
                   COMPUTE IRAT-DISTRIB-AMT ROUNDED = IRAM-WITHHOLD-AMT
                       * IRAM-RATE / 100
           END-EVALUATE
           IF IRAT-DISTRIB-AMT > 0 AND IRAT-DISTRIB-AMT < 2.00
               MOVE 2.00 TO IRAT-DISTRIB-AMT
           END-IF
           IF IRAT-DISTRIB-AMT > 25.00
               MOVE 25.00 TO IRAT-DISTRIB-AMT
           END-IF
           ADD IRAT-DISTRIB-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3410-DESCRIBE-REASON.
           EVALUATE IRAM-REASON-CD
               WHEN 'CMR'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'LJS'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'NRT'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'UGA'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'WBS'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ZYU'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3420-COUNT-RETRY.
           IF IRAM-RETRY-CNT < 9999999
               ADD 1 TO IRAM-RETRY-CNT
           END-IF
           IF IRAM-RETRY-CNT NOT < 6
               MOVE 'Y' TO IRAM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3430-COUNT-RETRY.
           IF IRAM-RETRY-CNT < 9999999
               ADD 1 TO IRAM-RETRY-CNT
           END-IF
           IF IRAM-RETRY-CNT NOT < 3
               MOVE 'Y' TO IRAM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * EA -> HR WHEN EFF DATE IS PAST
      * HR -> XZ WHEN EFF DATE IS PAST
      * XZ -> EA WHEN EFF DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3440-SET-TYPE.
           EVALUATE IRAM-TYPE-CD
               WHEN 'EA'
                   IF IRAT-EFF-DATE > 0 AND IRAT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HR' TO IRAM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HR'
                   IF IRAT-EFF-DATE > 0 AND IRAT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XZ' TO IRAM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XZ'
                   IF IRAT-EFF-DATE > 0 AND IRAT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EA' TO IRAM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3450-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF IRAM-MATURITY-DATE > 0 AND IRAM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD IRAM-CONTRIB-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD IRAM-CONTRIB-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD IRAM-CONTRIB-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD IRAM-CONTRIB-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD IRAM-CONTRIB-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * CN -> DS WHEN NOTICE DATE IS PAST
      * DS -> RO WHEN NOTICE DATE IS PAST
      * RO -> RC WHEN NOTICE DATE IS PAST
      * RC -> WH WHEN NOTICE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3460-SET-TRAN.
           EVALUATE IRAT-TRAN-CD
               WHEN 'CN'
                   IF IRAM-NOTICE-DATE > 0 AND IRAM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DS' TO IRAT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DS'
                   IF IRAM-NOTICE-DATE > 0 AND IRAM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RO' TO IRAT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RO'
                   IF IRAM-NOTICE-DATE > 0 AND IRAM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RC' TO IRAT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RC'
                   IF IRAM-NOTICE-DATE > 0 AND IRAM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WH' TO IRAT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE DISTRIB BAND TIER FOR THE EXCESS YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3470-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-DISTRIB-BAND-RATE(1) TO IRAM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF IRAM-EXCESS-YTD NOT < WS-DISTRIB-BAND-LOW(WS-TX)
                   MOVE WS-DISTRIB-BAND-RATE(WS-TX) TO IRAM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF IRAM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO IRAM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CONTRIB SCHED TIER FOR THE WITHHOLD AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3480-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-CONTRIB-SCHED-RATE(1) TO IRAM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF IRAM-WITHHOLD-AMT NOT < WS-CONTRIB-SCHED-LOW(WS-TX)
                   MOVE WS-CONTRIB-SCHED-RATE(WS-TX) TO IRAM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF IRAM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO IRAM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3490-DESCRIBE-TRAN.
           EVALUATE IRAT-TRAN-CD
               WHEN 'CN'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'DS'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'RO'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'RC'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'WH'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE EXCESS YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3500-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRAM-CHANNEL-CD
                   ADD IRAM-EXCESS-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRAM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE IRAM-EXCESS-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE EXCESS YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CONTRIB AMT.
      *----------------------------------------------------------------*
       3510-PRORATE-CONTRIB.
           IF WS-PERIOD-DAYS > 0
               COMPUTE IRAM-CONTRIB-AMT ROUNDED =
                   IRAM-EXCESS-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO IRAM-CONTRIB-AMT
           END-IF
           ADD IRAM-CONTRIB-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE FMV YTD FEE ON THE EXCESS AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3520-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN IRAM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO IRAM-FMV-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN IRAM-OVERRIDE-FLAG = 'Y'
                   COMPUTE IRAM-FMV-YTD ROUNDED = IRAT-EXCESS-AMT *
                       IRAM-RATE / 200
               WHEN IRAM-REASON-CD = 'CMR'
                   MOVE 7.00 TO IRAM-FMV-YTD
               WHEN IRAM-REASON-CD = 'LJS'
                   MOVE 3.00 TO IRAM-FMV-YTD
               WHEN OTHER
                   COMPUTE IRAM-FMV-YTD ROUNDED = IRAT-EXCESS-AMT *
                       IRAM-RATE / 100
           END-EVALUATE
           IF IRAM-FMV-YTD > 0 AND IRAM-FMV-YTD < 1.00
               MOVE 1.00 TO IRAM-FMV-YTD
           END-IF
           IF IRAM-FMV-YTD > 35.00
               MOVE 35.00 TO IRAM-FMV-YTD
           END-IF
           ADD IRAM-FMV-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE EXCESS YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3530-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRAM-REASON-CD
                   ADD IRAM-EXCESS-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRAM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE IRAM-EXCESS-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT RMD AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3540-SPLIT-RMD.
           COMPUTE WS-WORK-AMT = IRAM-RMD-AMT / 4
           COMPUTE WS-WORK-AMT-2 = IRAM-RMD-AMT - (WS-WORK-AMT * 4)
           COMPUTE IRAM-FMV-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DISTRIB BAND TIER FOR THE RMD AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3550-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DISTRIB-BAND-RATE(1) TO IRAM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF IRAT-RMD-AMT NOT < WS-DISTRIB-BAND-LOW(WS-TX)
                   MOVE WS-DISTRIB-BAND-RATE(WS-TX) TO IRAM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF IRAM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO IRAM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3560-SPLIT-EXCESS.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 12)
           COMPUTE IRAT-EXCESS-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3570-COUNT-PAYMENT.
           IF IRAM-PAYMENT-CNT < 9999999
               ADD 1 TO IRAM-PAYMENT-CNT
           END-IF
           IF IRAM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO IRAM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FMV YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3580-FLAG-FMV.
           IF IRAM-FMV-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE IRAM-FMV-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE DISTRIB AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3590-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRAT-CHANNEL-CD
                   ADD IRAM-DISTRIB-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRAT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE IRAM-DISTRIB-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * EA -> HR WHEN POST DATE IS PAST
      * HR -> XZ WHEN POST DATE IS PAST
      * XZ -> EA WHEN POST DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3600-SET-TYPE.
           EVALUATE IRAM-TYPE-CD
               WHEN 'EA'
                   IF IRAT-POST-DATE > 0 AND IRAT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HR' TO IRAM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HR'
                   IF IRAT-POST-DATE > 0 AND IRAT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XZ' TO IRAM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XZ'
                   IF IRAT-POST-DATE > 0 AND IRAT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EA' TO IRAM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE RMD YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DISTRIB AMT.
      *----------------------------------------------------------------*
       3610-PRORATE-DISTRIB.
           IF WS-PERIOD-DAYS > 0
               COMPUTE IRAT-DISTRIB-AMT ROUNDED =
                   IRAM-RMD-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO IRAT-DISTRIB-AMT
           END-IF
           ADD IRAT-DISTRIB-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE DISTRIB BAND TIER FOR THE EXCESS AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3620-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-DISTRIB-BAND-RATE(1) TO IRAM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF IRAM-EXCESS-AMT NOT < WS-DISTRIB-BAND-LOW(WS-TX)
                   MOVE WS-DISTRIB-BAND-RATE(WS-TX) TO IRAM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF IRAM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO IRAM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 15 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3630-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF IRAM-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO IRAM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * CN -> DS WHEN LAST ACT DATE IS PAST
      * DS -> RO WHEN LAST ACT DATE IS PAST
      * RO -> RC WHEN LAST ACT DATE IS PAST
      * RC -> WH WHEN LAST ACT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3640-SET-TRAN.
           EVALUATE IRAT-TRAN-CD
               WHEN 'CN'
                   IF IRAM-LAST-ACT-DATE > 0 AND IRAM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DS' TO IRAT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DS'
                   IF IRAM-LAST-ACT-DATE > 0 AND IRAM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RO' TO IRAT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RO'
                   IF IRAM-LAST-ACT-DATE > 0 AND IRAM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RC' TO IRAT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RC'
                   IF IRAM-LAST-ACT-DATE > 0 AND IRAM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WH' TO IRAT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3650-COUNT-PAYMENT.
           IF IRAM-PAYMENT-CNT < 9999999
               ADD 1 TO IRAM-PAYMENT-CNT
           END-IF
           IF IRAM-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO IRAM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ROLLOVER YTD AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3660-ACCRUE-ROLLOVER.
           IF IRAM-ROLLOVER-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   IRAM-ROLLOVER-YTD * IRAM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO IRAM-RMD-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RMD AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3670-ACCRUE-RMD.
           IF IRAT-RMD-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   IRAT-RMD-AMT * IRAM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO IRAM-FMV-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * CE -> FL WHEN NEXT DUE DATE IS PAST
      * FL -> GY WHEN NEXT DUE DATE IS PAST
      * GY -> HU WHEN NEXT DUE DATE IS PAST
      * HU -> KA WHEN NEXT DUE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3680-SET-REGION.
           EVALUATE IRAM-REGION-CD
               WHEN 'CE'
                   IF IRAM-NEXT-DUE-DATE > 0 AND IRAM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FL' TO IRAM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FL'
                   IF IRAM-NEXT-DUE-DATE > 0 AND IRAM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GY' TO IRAM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GY'
                   IF IRAM-NEXT-DUE-DATE > 0 AND IRAM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HU' TO IRAM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HU'
                   IF IRAM-NEXT-DUE-DATE > 0 AND IRAM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KA' TO IRAM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3690-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF IRAM-LAST-ACT-DATE > 0 AND IRAM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD IRAM-RMD-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD IRAM-RMD-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD IRAM-RMD-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD IRAM-RMD-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD IRAM-RMD-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE IRA TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3700-DESCRIBE-IRA-TYPE.
           EVALUATE IRAM-IRA-TYPE-CD
               WHEN 'F'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'V'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'W'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'Y'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DISTRIB YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3710-ACCRUE-DISTRIB.
           IF IRAM-DISTRIB-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   IRAM-DISTRIB-YTD * IRAM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO IRAM-ROLLOVER-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E086 DESC REQUIRED
      * E338 WITHHOLD AMT NEGATIVE
      * E663 WITHHOLD AMT OVER 1000000
      * E853 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3720-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN IRAM-DESC = SPACES
                   MOVE 'E086' TO WS-REJECT-CODE
               WHEN IRAM-WITHHOLD-AMT < 0
                   MOVE 'E338' TO WS-REJECT-CODE
               WHEN IRAM-WITHHOLD-AMT > 1000000.00
                   MOVE 'E663' TO WS-REJECT-CODE
               WHEN IRAM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E853' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3730-COUNT-CYCLE.
           IF IRAM-CYCLE-CNT < 9999999
               ADD 1 TO IRAM-CYCLE-CNT
           END-IF
           IF IRAM-CYCLE-CNT NOT < 3
               MOVE 'Y' TO IRAM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3740-COUNT-ITEM.
           IF IRAM-ITEM-CNT < 9999999
               ADD 1 TO IRAM-ITEM-CNT
           END-IF
           IF IRAM-ITEM-CNT NOT < 6
               MOVE 'Y' TO IRAM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3750-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF IRAT-POST-DATE > 0 AND IRAT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD IRAM-FMV-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD IRAM-FMV-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD IRAM-FMV-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD IRAM-FMV-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD IRAM-FMV-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FMV YTD IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3760-LIMIT-FMV.
           IF IRAM-FMV-YTD < 50.00
               MOVE 50.00 TO IRAM-FMV-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF IRAM-FMV-YTD > 1000.00
               MOVE 1000.00 TO IRAM-FMV-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RMD FEE ON THE EXCESS AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3770-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN IRAM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO IRAM-RMD-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN IRAM-STOP-FLAG = 'Y'
                   COMPUTE IRAM-RMD-AMT ROUNDED = IRAT-EXCESS-AMT *
                       IRAM-FEE-PCT / 200
               WHEN IRAM-REGION-CD = 'CE'
                   MOVE 7.00 TO IRAM-RMD-AMT
               WHEN IRAM-REGION-CD = 'FL'
                   MOVE 5.00 TO IRAM-RMD-AMT
               WHEN OTHER
                   COMPUTE IRAM-RMD-AMT ROUNDED = IRAT-EXCESS-AMT *
                       IRAM-FEE-PCT / 100
           END-EVALUATE
           IF IRAM-RMD-AMT > 0 AND IRAM-RMD-AMT < 10.00
               MOVE 10.00 TO IRAM-RMD-AMT
           END-IF
           IF IRAM-RMD-AMT > 250.00
               MOVE 250.00 TO IRAM-RMD-AMT
           END-IF
           ADD IRAM-RMD-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * WITHHOLD AMT IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3780-LIMIT-WITHHOLD.
           IF IRAM-WITHHOLD-AMT < 25.00
               MOVE 25.00 TO IRAM-WITHHOLD-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF IRAM-WITHHOLD-AMT > 50000.00
               MOVE 50000.00 TO IRAM-WITHHOLD-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DISTRIB YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3790-ACCRUE-DISTRIB.
           IF IRAM-DISTRIB-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   IRAM-DISTRIB-YTD * IRAM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO IRAT-ROLLOVER-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 180 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3800-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF IRAM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO IRAM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DIST CODE CD:
      * C -> J WHEN LAST STMT DATE IS PAST
      * J -> U WHEN LAST STMT DATE IS PAST
      * U -> X WHEN LAST STMT DATE IS PAST
      * X -> C WHEN LAST STMT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3810-SET-DIST-CODE.
           EVALUATE IRAM-DIST-CODE-CD
               WHEN 'C'
                   IF IRAM-LAST-STMT-DATE > 0 AND IRAM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'J' TO IRAM-DIST-CODE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'J'
                   IF IRAM-LAST-STMT-DATE > 0 AND IRAM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO IRAM-DIST-CODE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF IRAM-LAST-STMT-DATE > 0 AND IRAM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'X' TO IRAM-DIST-CODE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'X'
                   IF IRAM-LAST-STMT-DATE > 0 AND IRAM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'C' TO IRAM-DIST-CODE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE ROLLOVER AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3820-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRAT-TRAN-CD
                   ADD IRAT-ROLLOVER-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRAT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE IRAT-ROLLOVER-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3830-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF IRAT-EFF-DATE > 0 AND IRAT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD IRAT-DISTRIB-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD IRAT-DISTRIB-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD IRAT-DISTRIB-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD IRAT-DISTRIB-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD IRAT-DISTRIB-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE EXCESS AMT BY DIST CODE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3840-ACCUM-DIST-CODE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRAM-DIST-CODE-CD
                   ADD IRAT-EXCESS-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRAM-DIST-CODE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE IRAT-EXCESS-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DISTRIB AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3850-SPLIT-DISTRIB.
           COMPUTE WS-WORK-AMT = IRAM-DISTRIB-AMT / 12
           COMPUTE WS-WORK-AMT-2 = IRAM-DISTRIB-AMT - (WS-WORK-AMT * 12)
           COMPUTE IRAM-ROLLOVER-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR IRA TYPE CD:
      * F -> V WHEN OPEN DATE IS PAST
      * V -> W WHEN OPEN DATE IS PAST
      * W -> Y WHEN OPEN DATE IS PAST
      * Y -> F WHEN OPEN DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3860-SET-IRA-TYPE.
           EVALUATE IRAM-IRA-TYPE-CD
               WHEN 'F'
                   IF IRAM-OPEN-DATE > 0 AND IRAM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'V' TO IRAM-IRA-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'V'
                   IF IRAM-OPEN-DATE > 0 AND IRAM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO IRAM-IRA-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'W'
                   IF IRAM-OPEN-DATE > 0 AND IRAM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'Y' TO IRAM-IRA-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'Y'
                   IF IRAM-OPEN-DATE > 0 AND IRAM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'F' TO IRAM-IRA-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3870-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF IRAM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO IRAM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DISTRIB BAND TIER FOR THE EXCESS YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3880-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DISTRIB-BAND-RATE(1) TO IRAM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF IRAM-EXCESS-YTD NOT < WS-DISTRIB-BAND-LOW(WS-TX)
                   MOVE WS-DISTRIB-BAND-RATE(WS-TX) TO IRAM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF IRAM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO IRAM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR IRA TYPE CD:
      * F -> V WHEN NOTICE DATE IS PAST
      * V -> W WHEN NOTICE DATE IS PAST
      * W -> Y WHEN NOTICE DATE IS PAST
      * Y -> F WHEN NOTICE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3890-SET-IRA-TYPE.
           EVALUATE IRAM-IRA-TYPE-CD
               WHEN 'F'
                   IF IRAM-NOTICE-DATE > 0 AND IRAM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'V' TO IRAM-IRA-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'V'
                   IF IRAM-NOTICE-DATE > 0 AND IRAM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO IRAM-IRA-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'W'
                   IF IRAM-NOTICE-DATE > 0 AND IRAM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'Y' TO IRAM-IRA-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'Y'
                   IF IRAM-NOTICE-DATE > 0 AND IRAM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'F' TO IRAM-IRA-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCESS AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3900-FLAG-EXCESS.
           IF IRAT-EXCESS-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE IRAT-EXCESS-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CONTRIB SCHED TIER FOR THE EXCESS AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3910-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CONTRIB-SCHED-RATE(1) TO IRAM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF IRAT-EXCESS-AMT NOT < WS-CONTRIB-SCHED-LOW(WS-TX)
                   MOVE WS-CONTRIB-SCHED-RATE(WS-TX) TO IRAM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF IRAM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO IRAM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E495 ADDR REQUIRED
      * E780 FMV YTD NEGATIVE
      * E110 FMV YTD OVER 10000
      * E865 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3920-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN IRAM-ADDR = SPACES
                   MOVE 'E495' TO WS-REJECT-CODE
               WHEN IRAM-FMV-YTD < 0
                   MOVE 'E780' TO WS-REJECT-CODE
               WHEN IRAM-FMV-YTD > 10000.00
                   MOVE 'E110' TO WS-REJECT-CODE
               WHEN IRAM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E865' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WITHHOLD AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3930-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRAT-CHANNEL-CD
                   ADD IRAM-WITHHOLD-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRAT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE IRAM-WITHHOLD-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR IRA TYPE CD:
      * F -> V WHEN POST DATE IS PAST
      * V -> W WHEN POST DATE IS PAST
      * W -> Y WHEN POST DATE IS PAST
      * Y -> F WHEN POST DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3940-SET-IRA-TYPE.
           EVALUATE IRAM-IRA-TYPE-CD
               WHEN 'F'
                   IF IRAT-POST-DATE > 0 AND IRAT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'V' TO IRAM-IRA-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'V'
                   IF IRAT-POST-DATE > 0 AND IRAT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO IRAM-IRA-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'W'
                   IF IRAT-POST-DATE > 0 AND IRAT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'Y' TO IRAM-IRA-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'Y'
                   IF IRAT-POST-DATE > 0 AND IRAT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'F' TO IRAM-IRA-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ROLLOVER AMT IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3950-LIMIT-ROLLOVER.
           IF IRAM-ROLLOVER-AMT < 50.00
               MOVE 50.00 TO IRAM-ROLLOVER-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF IRAM-ROLLOVER-AMT > 50000.00
               MOVE 50000.00 TO IRAM-ROLLOVER-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ROLLOVER AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3960-FLAG-ROLLOVER.
           IF IRAM-ROLLOVER-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE IRAM-ROLLOVER-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DISTRIB AMT IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3970-LIMIT-DISTRIB.
           IF IRAT-DISTRIB-AMT < 10.00
               MOVE 10.00 TO IRAT-DISTRIB-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF IRAT-DISTRIB-AMT > 10000.00
               MOVE 10000.00 TO IRAT-DISTRIB-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3980-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF IRAM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO IRAM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E686 REF NO REQUIRED
      * E038 EXCESS YTD NEGATIVE
      * E166 EXCESS YTD OVER 99999
      * E715 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3990-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN IRAM-REF-NO = SPACES
                   MOVE 'E686' TO WS-REJECT-CODE
               WHEN IRAM-EXCESS-YTD < 0
                   MOVE 'E038' TO WS-REJECT-CODE
               WHEN IRAM-EXCESS-YTD > 99999.00
                   MOVE 'E166' TO WS-REJECT-CODE
               WHEN IRAM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E715' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE DISTRIB AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4000-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRAT-CHANNEL-CD
                   ADD IRAT-DISTRIB-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRAT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE IRAT-DISTRIB-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE EXCESS AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CONTRIB YTD.
      *----------------------------------------------------------------*
       4010-PRORATE-CONTRIB.
           IF WS-PERIOD-DAYS > 0
               COMPUTE IRAM-CONTRIB-YTD ROUNDED =
                   IRAM-EXCESS-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO IRAM-CONTRIB-YTD
           END-IF
           ADD IRAM-CONTRIB-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ROLLOVER AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4020-FLAG-ROLLOVER.
           IF IRAM-ROLLOVER-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE IRAM-ROLLOVER-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4030-DESCRIBE-REASON.
           EVALUATE IRAM-REASON-CD
               WHEN 'CMR'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'LJS'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'NRT'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'UGA'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'WBS'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'ZYU'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WITHHOLD AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4040-FLAG-WITHHOLD.
           IF IRAM-WITHHOLD-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE IRAM-WITHHOLD-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * EA -> HR WHEN EFFECTIVE DATE IS PAST
      * HR -> XZ WHEN EFFECTIVE DATE IS PAST
      * XZ -> EA WHEN EFFECTIVE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4050-SET-TYPE.
           EVALUATE IRAM-TYPE-CD
               WHEN 'EA'
                   IF IRAM-EFFECTIVE-DATE > 0 AND IRAM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HR' TO IRAM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HR'
                   IF IRAM-EFFECTIVE-DATE > 0 AND IRAM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XZ' TO IRAM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XZ'
                   IF IRAM-EFFECTIVE-DATE > 0 AND IRAM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EA' TO IRAM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FMV AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4060-ACCRUE-FMV.
           IF IRAM-FMV-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   IRAM-FMV-AMT * IRAM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO IRAM-DISTRIB-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO RMD YTD.
      *----------------------------------------------------------------*
       4070-PRORATE-RMD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE IRAM-RMD-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO IRAM-RMD-YTD
           END-IF
           ADD IRAM-RMD-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * EXCESS YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4080-FLAG-EXCESS.
           IF IRAM-EXCESS-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE IRAM-EXCESS-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ROLLOVER AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4090-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRAM-REASON-CD
                   ADD IRAT-ROLLOVER-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRAM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE IRAT-ROLLOVER-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4100-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF IRAM-EFFECTIVE-DATE > 0 AND IRAM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD IRAM-ROLLOVER-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD IRAM-ROLLOVER-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD IRAM-ROLLOVER-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD IRAM-ROLLOVER-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD IRAM-ROLLOVER-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT EXCESS YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4110-SPLIT-EXCESS.
           COMPUTE WS-WORK-AMT = IRAM-EXCESS-YTD / 3
           COMPUTE WS-WORK-AMT-2 = IRAM-EXCESS-YTD - (WS-WORK-AMT * 3)
           COMPUTE IRAM-WITHHOLD-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * CN -> DS WHEN EXPIRY DATE IS PAST
      * DS -> RO WHEN EXPIRY DATE IS PAST
      * RO -> RC WHEN EXPIRY DATE IS PAST
      * RC -> WH WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4120-SET-TRAN.
           EVALUATE IRAT-TRAN-CD
               WHEN 'CN'
                   IF IRAM-EXPIRY-DATE > 0 AND IRAM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DS' TO IRAT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DS'
                   IF IRAM-EXPIRY-DATE > 0 AND IRAM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RO' TO IRAT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RO'
                   IF IRAM-EXPIRY-DATE > 0 AND IRAM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RC' TO IRAT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RC'
                   IF IRAM-EXPIRY-DATE > 0 AND IRAM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WH' TO IRAT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE IRA TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4130-DESCRIBE-IRA-TYPE.
           EVALUATE IRAM-IRA-TYPE-CD
               WHEN 'F'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'V'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'W'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'Y'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * CE -> FL WHEN EFFECTIVE DATE IS PAST
      * FL -> GY WHEN EFFECTIVE DATE IS PAST
      * GY -> HU WHEN EFFECTIVE DATE IS PAST
      * HU -> KA WHEN EFFECTIVE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4140-SET-REGION.
           EVALUATE IRAM-REGION-CD
               WHEN 'CE'
                   IF IRAM-EFFECTIVE-DATE > 0 AND IRAM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FL' TO IRAM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FL'
                   IF IRAM-EFFECTIVE-DATE > 0 AND IRAM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GY' TO IRAM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GY'
                   IF IRAM-EFFECTIVE-DATE > 0 AND IRAM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HU' TO IRAM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HU'
                   IF IRAM-EFFECTIVE-DATE > 0 AND IRAM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KA' TO IRAM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE DISTRIB AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4150-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRAM-REGION-CD
                   ADD IRAT-DISTRIB-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRAM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE IRAT-DISTRIB-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE DIST CODE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4160-DESCRIBE-DIST-CODE.
           EVALUATE IRAM-DIST-CODE-CD
               WHEN 'C'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'J'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'U'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'X'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4170-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF IRAM-NOTICE-DATE > 0 AND IRAM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD IRAM-FMV-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD IRAM-FMV-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD IRAM-FMV-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD IRAM-FMV-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD IRAM-FMV-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * CE -> FL WHEN EXPIRY DATE IS PAST
      * FL -> GY WHEN EXPIRY DATE IS PAST
      * GY -> HU WHEN EXPIRY DATE IS PAST
      * HU -> KA WHEN EXPIRY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4180-SET-REGION.
           EVALUATE IRAM-REGION-CD
               WHEN 'CE'
                   IF IRAM-EXPIRY-DATE > 0 AND IRAM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FL' TO IRAM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FL'
                   IF IRAM-EXPIRY-DATE > 0 AND IRAM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GY' TO IRAM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GY'
                   IF IRAM-EXPIRY-DATE > 0 AND IRAM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HU' TO IRAM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HU'
                   IF IRAM-EXPIRY-DATE > 0 AND IRAM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KA' TO IRAM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       4190-COUNT-ITEM.
           IF IRAM-ITEM-CNT < 9999999
               ADD 1 TO IRAM-ITEM-CNT
           END-IF
           IF IRAM-ITEM-CNT NOT < 12
               MOVE 'Y' TO IRAM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE EXCESS YTD BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4200-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRAM-TYPE-CD
                   ADD IRAM-EXCESS-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRAM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE IRAM-EXCESS-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4210-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 50.00
               MOVE 50.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 2500.00
               MOVE 2500.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE RMD YTD BY DIST CODE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4220-ACCUM-DIST-CODE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRAM-DIST-CODE-CD
                   ADD IRAM-RMD-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRAM-DIST-CODE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE IRAM-RMD-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       4230-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF IRAM-OPEN-DATE > 0 AND IRAM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD IRAM-ROLLOVER-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD IRAM-ROLLOVER-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD IRAM-ROLLOVER-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD IRAM-ROLLOVER-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD IRAM-ROLLOVER-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FMV YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4240-FLAG-FMV.
           IF IRAM-FMV-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE IRAM-FMV-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4250-DESCRIBE-REASON.
           EVALUATE IRAM-REASON-CD
               WHEN 'CMR'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'LJS'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'NRT'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'UGA'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'WBS'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'ZYU'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO EXCESS AMT.
      *----------------------------------------------------------------*
       4260-PRORATE-EXCESS.
           IF WS-PERIOD-DAYS > 0
               COMPUTE IRAT-EXCESS-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO IRAT-EXCESS-AMT
           END-IF
           ADD IRAT-EXCESS-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E246 REF NO REQUIRED
      * E836 ROLLOVER AMT NEGATIVE
      * E406 ROLLOVER AMT OVER 10000
      * E358 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       4270-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN IRAM-REF-NO = SPACES
                   MOVE 'E246' TO WS-REJECT-CODE
               WHEN IRAM-ROLLOVER-AMT < 0
                   MOVE 'E836' TO WS-REJECT-CODE
               WHEN IRAM-ROLLOVER-AMT > 10000.00
                   MOVE 'E406' TO WS-REJECT-CODE
               WHEN IRAM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E358' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE DIST CODE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4280-DESCRIBE-DIST-CODE.
           EVALUATE IRAM-DIST-CODE-CD
               WHEN 'C'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'J'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'U'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'X'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE FMV YTD FEE ON THE ROLLOVER AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       4290-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN IRAM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO IRAM-FMV-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN IRAM-SUSPEND-FLAG = 'Y'
                   COMPUTE IRAM-FMV-YTD ROUNDED = IRAT-ROLLOVER-AMT *
                       IRAM-MARGIN / 200
               WHEN IRAM-DIST-CODE-CD = 'C'
                   MOVE 20.00 TO IRAM-FMV-YTD
               WHEN IRAM-DIST-CODE-CD = 'J'
                   MOVE 3.00 TO IRAM-FMV-YTD
               WHEN OTHER
                   COMPUTE IRAM-FMV-YTD ROUNDED = IRAT-ROLLOVER-AMT *
                       IRAM-MARGIN / 100
           END-EVALUATE
           IF IRAM-FMV-YTD > 0 AND IRAM-FMV-YTD < 10.00
               MOVE 10.00 TO IRAM-FMV-YTD
           END-IF
           IF IRAM-FMV-YTD > 250.00
               MOVE 250.00 TO IRAM-FMV-YTD
           END-IF
           ADD IRAM-FMV-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE DIST CODE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4300-DESCRIBE-DIST-CODE.
           EVALUATE IRAM-DIST-CODE-CD
               WHEN 'C'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'J'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'U'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'X'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4310-FLAG-ITEM.
           IF WS-WORK-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       4320-COUNT-PAYMENT.
           IF IRAM-PAYMENT-CNT < 9999999
               ADD 1 TO IRAM-PAYMENT-CNT
           END-IF
           IF IRAM-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO IRAM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DISTRIB AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO RMD AMT.
      *----------------------------------------------------------------*
       4330-PRORATE-RMD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE IRAT-RMD-AMT ROUNDED =
                   IRAT-DISTRIB-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO IRAT-RMD-AMT
           END-IF
           ADD IRAT-RMD-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE CONTRIB SCHED TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4340-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CONTRIB-SCHED-RATE(1) TO IRAM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF WS-WORK-AMT-2 NOT < WS-CONTRIB-SCHED-LOW(WS-TX)
                   MOVE WS-CONTRIB-SCHED-RATE(WS-TX) TO IRAM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF IRAM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO IRAM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT EXCESS AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4350-SPLIT-EXCESS.
           COMPUTE WS-WORK-AMT = IRAM-EXCESS-AMT / 4
           COMPUTE WS-WORK-AMT-2 = IRAM-EXCESS-AMT - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE RMD AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4360-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRAT-TRAN-CD
                   ADD IRAT-RMD-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRAT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE IRAT-RMD-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       4370-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF IRAM-OPEN-DATE > 0 AND IRAM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD IRAM-FMV-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD IRAM-FMV-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD IRAM-FMV-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD IRAM-FMV-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD IRAM-FMV-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ROLLOVER AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4380-ACCRUE-ROLLOVER.
           IF IRAT-ROLLOVER-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   IRAT-ROLLOVER-AMT * IRAM-RATE / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FMV YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4390-FLAG-FMV.
           IF IRAM-FMV-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE IRAM-FMV-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * WITHHOLD YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4400-FLAG-WITHHOLD.
           IF IRAM-WITHHOLD-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE IRAM-WITHHOLD-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WITHHOLD AMT.
      *----------------------------------------------------------------*
       4410-PRORATE-WITHHOLD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE IRAM-WITHHOLD-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO IRAM-WITHHOLD-AMT
           END-IF
           ADD IRAM-WITHHOLD-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * CN -> DS WHEN NEXT DUE DATE IS PAST
      * DS -> RO WHEN NEXT DUE DATE IS PAST
      * RO -> RC WHEN NEXT DUE DATE IS PAST
      * RC -> WH WHEN NEXT DUE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4420-SET-TRAN.
           EVALUATE IRAT-TRAN-CD
               WHEN 'CN'
                   IF IRAM-NEXT-DUE-DATE > 0 AND IRAM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DS' TO IRAT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DS'
                   IF IRAM-NEXT-DUE-DATE > 0 AND IRAM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RO' TO IRAT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RO'
                   IF IRAM-NEXT-DUE-DATE > 0 AND IRAM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RC' TO IRAT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RC'
                   IF IRAM-NEXT-DUE-DATE > 0 AND IRAM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WH' TO IRAT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE ROLLOVER AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4430-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRAM-CHANNEL-CD
                   ADD IRAT-ROLLOVER-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRAM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE IRAT-ROLLOVER-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E009 REJECT CD REQUIRED
      * E177 DISTRIB AMT NEGATIVE
      * E098 DISTRIB AMT OVER 250000
      * E354 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       4440-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN IRAT-REJECT-CD = SPACES
                   MOVE 'E009' TO WS-REJECT-CODE
               WHEN IRAT-DISTRIB-AMT < 0
                   MOVE 'E177' TO WS-REJECT-CODE
               WHEN IRAT-DISTRIB-AMT > 250000.00
                   MOVE 'E098' TO WS-REJECT-CODE
               WHEN IRAM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E354' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE DIST CODE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4450-DESCRIBE-DIST-CODE.
           EVALUATE IRAM-DIST-CODE-CD
               WHEN 'C'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'J'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'U'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'X'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RMD YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4460-ACCRUE-RMD.
           IF IRAM-RMD-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   IRAM-RMD-YTD * IRAM-RATE / 100 / 360
               ADD WS-WORK-AMT TO IRAM-ROLLOVER-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WITHHOLD AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4470-FLAG-WITHHOLD.
           IF IRAM-WITHHOLD-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE IRAM-WITHHOLD-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4480-COUNT-PAYMENT.
           IF IRAM-PAYMENT-CNT < 9999999
               ADD 1 TO IRAM-PAYMENT-CNT
           END-IF
           IF IRAM-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO IRAT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DISTRIB BAND TIER FOR THE EXCESS AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4490-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-DISTRIB-BAND-RATE(1) TO IRAM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF IRAT-EXCESS-AMT NOT < WS-DISTRIB-BAND-LOW(WS-TX)
                   MOVE WS-DISTRIB-BAND-RATE(WS-TX) TO IRAM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF IRAM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO IRAM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-90, 91-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4500-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF IRAT-POST-DATE > 0 AND IRAT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD IRAM-ROLLOVER-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD IRAM-ROLLOVER-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD IRAM-ROLLOVER-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD IRAM-ROLLOVER-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD IRAM-ROLLOVER-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR IRA TYPE CD:
      * F -> V WHEN POST DATE IS PAST
      * V -> W WHEN POST DATE IS PAST
      * W -> Y WHEN POST DATE IS PAST
      * Y -> F WHEN POST DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4510-SET-IRA-TYPE.
           EVALUATE IRAM-IRA-TYPE-CD
               WHEN 'F'
                   IF IRAT-POST-DATE > 0 AND IRAT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'V' TO IRAM-IRA-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'V'
                   IF IRAT-POST-DATE > 0 AND IRAT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO IRAM-IRA-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'W'
                   IF IRAT-POST-DATE > 0 AND IRAT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'Y' TO IRAM-IRA-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'Y'
                   IF IRAT-POST-DATE > 0 AND IRAT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'F' TO IRAM-IRA-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4520-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF IRAM-NEXT-REVIEW-DATE > 0 AND IRAM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD IRAM-FMV-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD IRAM-FMV-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD IRAM-FMV-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD IRAM-FMV-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD IRAM-FMV-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E631 OPERATOR REQUIRED
      * E471 WITHHOLD YTD NEGATIVE
      * E694 WITHHOLD YTD OVER 99999
      * E784 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       4530-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN IRAT-OPERATOR = SPACES
                   MOVE 'E631' TO WS-REJECT-CODE
               WHEN IRAM-WITHHOLD-YTD < 0
                   MOVE 'E471' TO WS-REJECT-CODE
               WHEN IRAM-WITHHOLD-YTD > 99999.00
                   MOVE 'E694' TO WS-REJECT-CODE
               WHEN IRAM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E784' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WITHHOLD AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO EXCESS AMT.
      *----------------------------------------------------------------*
       4540-PRORATE-EXCESS.
           IF WS-PERIOD-DAYS > 0
               COMPUTE IRAM-EXCESS-AMT ROUNDED =
                   IRAM-WITHHOLD-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO IRAM-EXCESS-AMT
           END-IF
           ADD IRAM-EXCESS-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PASS THE IRA FIELDS TO IRELG AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7020-CALL-IRELG.
           INITIALIZE IRELG-PARMS
           MOVE IRAM-FMV-AMT
             TO IRELG-FMV-AMT
           MOVE IRAM-EXCESS-AMT
             TO IRELG-EXCESS-AMT
           MOVE IRAM-ROLLOVER-YTD
             TO IRELG-ROLLOVER-YTD
           MOVE IRAM-RMD-AMT
             TO IRELG-RMD-AMT
           MOVE IRAM-ROLLOVER-AMT
             TO IRELG-ROLLOVER-AMT
           MOVE IRAM-FMV-YTD
             TO IRELG-FMV-YTD
           MOVE IRAM-LAST-ACT-DATE
             TO IRELG-LAST-ACT-DATE
           MOVE IRAM-LAST-STMT-DATE
             TO IRELG-LAST-STMT-DATE
           MOVE IRAM-EXPIRY-DATE
             TO IRELG-EXPIRY-DATE
           MOVE IRAM-NEXT-REVIEW-DATE
             TO IRELG-NEXT-REVIEW-DATE
           MOVE IRAM-CHANNEL-CD
             TO IRELG-CHANNEL-CD
           MOVE IRAM-REGION-CD
             TO IRELG-REGION-CD
           MOVE IRAM-REASON-CD
             TO IRELG-REASON-CD
           MOVE IRAM-TYPE-CD
             TO IRELG-TYPE-CD
           MOVE IRAM-STOP-FLAG
             TO IRELG-STOP-FLAG
           MOVE IRAM-REVIEW-FLAG
             TO IRELG-REVIEW-FLAG
           MOVE IRAM-AUTO-RENEW-FLAG
             TO IRELG-AUTO-RENEW-FLAG
           MOVE IRAM-CYCLE-CNT
             TO IRELG-CYCLE-CNT
           MOVE IRAM-RETRY-CNT
             TO IRELG-RETRY-CNT
           MOVE IRAM-MARGIN
             TO IRELG-MARGIN
           MOVE IRAM-FEE-PCT
             TO IRELG-FEE-PCT
           MOVE IRAM-ADDR
             TO IRELG-ADDR
           MOVE IRAM-NAME
             TO IRELG-NAME
           CALL 'IRELG' USING IRELG-PARMS
           EVALUATE IRELG-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE IRELG-FMV-AMT
                     TO IRAM-FMV-AMT
                   MOVE IRELG-EXCESS-AMT
                     TO IRAM-EXCESS-AMT
                   MOVE IRELG-ROLLOVER-AMT
                     TO IRAM-ROLLOVER-AMT
                   MOVE IRELG-STOP-FLAG
                     TO IRAM-STOP-FLAG
                   MOVE IRELG-REVIEW-FLAG
                     TO IRAM-REVIEW-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RELG' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-IRELG' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE IRA FIELDS TO IRPRC AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7000-CALL-IRPRC.
           INITIALIZE IRPRC-PARMS
           MOVE IRAM-RMD-YTD
             TO IRPRC-RMD-YTD
           MOVE IRAM-CONTRIB-YTD
             TO IRPRC-CONTRIB-YTD
           MOVE IRAM-EXCESS-YTD
             TO IRPRC-EXCESS-YTD
           MOVE IRAM-DISTRIB-YTD
             TO IRPRC-DISTRIB-YTD
           MOVE IRAM-FMV-AMT
             TO IRPRC-FMV-AMT
           MOVE IRAM-ROLLOVER-AMT
             TO IRPRC-ROLLOVER-AMT
           MOVE IRAM-NEXT-REVIEW-DATE
             TO IRPRC-NEXT-REVIEW-DATE
           MOVE IRAM-LAST-STMT-DATE
             TO IRPRC-LAST-STMT-DATE
           MOVE IRAM-LAST-ACT-DATE
             TO IRPRC-LAST-ACT-DATE
           MOVE IRAM-EXPIRY-DATE
             TO IRPRC-EXPIRY-DATE
           MOVE IRAM-IRA-TYPE-CD
             TO IRPRC-IRA-TYPE-CD
           MOVE IRAM-TYPE-CD
             TO IRPRC-TYPE-CD
           MOVE IRAM-CHANNEL-CD
             TO IRPRC-CHANNEL-CD
           MOVE IRAM-REASON-CD
             TO IRPRC-REASON-CD
           MOVE IRAM-OVERRIDE-FLAG
             TO IRPRC-OVERRIDE-FLAG
           MOVE IRAM-SUSPEND-FLAG
             TO IRPRC-SUSPEND-FLAG
           MOVE IRAM-AUTO-RENEW-FLAG
             TO IRPRC-AUTO-RENEW-FLAG
           MOVE IRAM-PAYMENT-CNT
             TO IRPRC-PAYMENT-CNT
           MOVE IRAM-RETRY-CNT
             TO IRPRC-RETRY-CNT
           MOVE IRAM-MARGIN
             TO IRPRC-MARGIN
           MOVE IRAM-FEE-PCT
             TO IRPRC-FEE-PCT
           MOVE IRAM-NAME
             TO IRPRC-NAME
           MOVE IRAM-ADDR
             TO IRPRC-ADDR
           CALL 'IRPRC' USING IRPRC-PARMS
           EVALUATE IRPRC-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE IRPRC-RMD-YTD
                     TO IRAM-RMD-YTD
                   MOVE IRPRC-DISTRIB-YTD
                     TO IRAM-DISTRIB-YTD
                   MOVE IRPRC-FMV-AMT
                     TO IRAM-FMV-AMT
                   MOVE IRPRC-ROLLOVER-AMT
                     TO IRAM-ROLLOVER-AMT
                   MOVE IRPRC-OVERRIDE-FLAG
                     TO IRAM-OVERRIDE-FLAG
                   MOVE IRPRC-SUSPEND-FLAG
                     TO IRAM-SUSPEND-FLAG
                   MOVE IRPRC-AUTO-RENEW-FLAG
                     TO IRAM-AUTO-RENEW-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RPRC' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-IRPRC' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE IRAT-KEY TO RD-KEY
           MOVE IRAT-TRAN-CD TO RD-CODE
           MOVE IRAT-DISTRIB-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE IRAT-KEY TO RD-KEY
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
           DISPLAY 'IRAPURG READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'IRAPURG' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
