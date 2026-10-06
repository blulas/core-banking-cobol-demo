      *================================================================*
      * PROGRAM  : YKBRCON                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : DATE WINDOW BRIDGE - RECONCILIATION. READS THE YDB  *
      *            TRANSACTIONS, UPDATES THE YDB MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. TWO-DIGIT YEAR WINDOWING BRIDGE *
      *            FOR CENTURY DATE CONVERSION                         *
      * INPUT    : YDBMST (KSDS), YDBTRN, BANKCTL                      *
      * OUTPUT   : YDBOUT, YKBRCRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, YKLIM, YKSTAT           *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-10-26 SKN  ORIGINAL                                       *
      * 1993-04-13 JLM  AMOUNT-2 FEE SCHEDULE UPDATE INC9703           *
      * 1998-02-22 DPS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1998-05-20 LMB  PERFORMANCE - CONTROL READ AHEAD REQ4373       *
      * 2000-08-21 SKN  AMOUNT-1 TABLE EXPANDED CHG3773                *
      * 2000-06-03 CRP  AMOUNT-2 CUTOFF MOVED REQ8376                  *
      * 2000-03-06 GAF  AUDIT FINDING - CONTROL CONTROL TOTALS PRB0601 *
      * 2001-06-20 GAF  CONTROL CUTOFF MOVED CHG3650                   *
      * 2002-11-16 GAF  CONTROL CUTOFF MOVED INC8014                   *
      * 2005-04-03 RJT  AMOUNT-1 LIMIT REVISED REQ6246                 *
      * 2005-06-07 SKN  CONTROL CALCULATION CORRECTED REQ8725          *
      * 2008-02-02 TWH  CONTROL RERUN PROTECTION INC2005               *
      * 2008-07-14 DPS  AMOUNT-2 CALCULATION CORRECTED INC3284         *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YKBRCON.
       AUTHOR. T W HOLT.

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
           SELECT REPORT-FILE ASSIGN TO "YKBRCRPT"
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
                                           VALUE 23.000000.
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
      * WS-AMOUNT-1-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-AMOUNT-1-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000068000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000568500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010500000000366000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013000000000628500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014000000000650000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014250000000242500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014500000000512000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014750000000821000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014850000000384000'.
       01  WS-AMOUNT-1-TIER-TBL REDEFINES WS-AMOUNT-1-TIER-VALUES.
           05  WS-AMOUNT-1-TIER-ROW OCCURS 9 TIMES.
               10  WS-AMOUNT-1-TIER-LOW    PIC 9(09)V99.
               10  WS-AMOUNT-1-TIER-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-AMOUNT-1-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-AMOUNT-1-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000136500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000723000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005100000000617000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007600000000027000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007650000000333500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017650000000307500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020150000000559000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022650000000385000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00027650000000791500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00027900000000748000'.
       01  WS-AMOUNT-1-GRID-TBL REDEFINES WS-AMOUNT-1-GRID-VALUES.
           05  WS-AMOUNT-1-GRID-ROW OCCURS 10 TIMES.
               10  WS-AMOUNT-1-GRID-LOW    PIC 9(09)V99.
               10  WS-AMOUNT-1-GRID-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-AMOUNT-1-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-AMOUNT-1-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000306000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000109000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000300000000856500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000550000000204500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003050000000193500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005550000000100000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008050000000353500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018050000000364000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018550000000442500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018800000000516000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00019300000000511500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00019550000000655000'.
       01  WS-AMOUNT-1-SCHED-TBL REDEFINES WS-AMOUNT-1-SCHED-VALUES.
           05  WS-AMOUNT-1-SCHED-ROW OCCURS 12 TIMES.
               10  WS-AMOUNT-1-SCHED-LOW   PIC 9(09)V99.
               10  WS-AMOUNT-1-SCHED-RATE  PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  YKBRCON  DATE WINDOW BRIDGE'.
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
       COPY YKSTATP.

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
               PERFORM 3000-LIMIT-AMOUNT-2
               PERFORM 3010-ACCRUE-DISCOUNT
               PERFORM 3020-DESCRIBE-SOURCE
               PERFORM 3030-ACCRUE-AMOUNT-2
               PERFORM 3040-CHECK-MATURITY
               PERFORM 3050-PRORATE-AMOUNT-2
               PERFORM 3060-SPLIT-AMOUNT-2
               PERFORM 3070-SELECT-MARGIN
               PERFORM 3080-SELECT-DISCOUNT
               PERFORM 3090-ACCRUE-PENALTY-PCT
               PERFORM 3100-FLAG-CONTROL
               PERFORM 3110-SELECT-PENALTY-PCT
               PERFORM 3120-EDIT-MEMO
               PERFORM 3130-ACCRUE-CONTROL
               PERFORM 3140-AGE-POST
               PERFORM 3150-LIMIT-ITEM
               PERFORM 3160-LIMIT-CONTROL
               PERFORM 3170-AGE-NEXT-DUE
               PERFORM 3180-LIMIT-AMOUNT-2
               PERFORM 3190-ACCRUE-AMOUNT-2
               PERFORM 3200-ASSESS-REVERSAL
               PERFORM 3210-EDIT-DESC
               PERFORM 3220-LIMIT-AMOUNT-2
               PERFORM 3230-LIMIT-AMOUNT-2
               PERFORM 3240-ASSESS-REVERSAL
               PERFORM 3250-LIMIT-AMOUNT-1
               PERFORM 3260-FLAG-CONTROL
               PERFORM 3270-PRORATE-CONTROL
               PERFORM 3280-ACCRUE-CONTROL
               PERFORM 3290-FLAG-CONTROL
               PERFORM 3300-DESCRIBE-PIVOT
               PERFORM 3310-SET-REASON
               PERFORM 3320-AGE-LAST-MAINT
               PERFORM 3330-EDIT-MEMO
               PERFORM 3340-FLAG-ITEM
               PERFORM 3350-COUNT-PAYMENT
               PERFORM 3360-LIMIT-CONTROL
               PERFORM 3370-DESCRIBE-WINDOW-RULE
               PERFORM 3380-FLAG-CONTROL
               PERFORM 3390-DESCRIBE-CHANNEL
               PERFORM 3400-ASSESS-OVERRIDE
               PERFORM 3410-ASSESS-AUTO-RENEW
               PERFORM 3420-PRORATE-CONTROL
               PERFORM 3430-SELECT-MARGIN
               PERFORM 3440-LIMIT-AMOUNT-1
               PERFORM 3450-SELECT-PENALTY-PCT
               PERFORM 3460-LIMIT-AMOUNT-2
               PERFORM 3470-DESCRIBE-PIVOT
               PERFORM 3480-AGE-NOTICE
               PERFORM 3490-COUNT-RETRY
               PERFORM 3500-PRORATE-AMOUNT-2
               PERFORM 3510-COUNT-PAYMENT
               PERFORM 3520-CHECK-LAST-PAY
               PERFORM 3530-CHECK-NOTICE
               PERFORM 3540-FLAG-ITEM
               PERFORM 3550-EDIT-CONTACT
               PERFORM 3560-SPLIT-AMOUNT-2
               PERFORM 3570-COUNT-CYCLE
               PERFORM 3580-COUNT-SEQ
               PERFORM 3590-ASSESS-OVERRIDE
               PERFORM 3600-CHECK-EXPIRY
               PERFORM 3610-EDIT-ADDR
               PERFORM 3620-PRORATE-AMOUNT-2
               PERFORM 3630-ASSESS-AUTO-RENEW
               PERFORM 3640-FLAG-AMOUNT-2
               PERFORM 3650-FLAG-CONTROL
               PERFORM 3660-ACCRUE-DISCOUNT
               PERFORM 3670-PRORATE-CONTROL
               PERFORM 3680-EDIT-MEMO
               PERFORM 3690-SELECT-PENALTY-PCT
               PERFORM 3700-ASSESS-AUTO-RENEW
               PERFORM 3710-ASSESS-PREFERRED
               PERFORM 3720-COUNT-RETRY
               PERFORM 3730-ACCUM-WINDOW-RULE
               PERFORM 3740-DESCRIBE-REASON
               PERFORM 3750-DESCRIBE-SOURCE
               PERFORM 3760-DESCRIBE-PIVOT
               PERFORM 3770-SELECT-DISCOUNT
               PERFORM 3780-DESCRIBE-WINDOW-RULE
               PERFORM 3790-AGE-MATURITY
               PERFORM 3800-SPLIT-AMOUNT-1
               PERFORM 3810-SPLIT-AMOUNT-1
               PERFORM 3820-SPLIT-AMOUNT-1
               PERFORM 3830-FLAG-CONTROL
               PERFORM 3840-EDIT-NAME
               PERFORM 3850-EDIT-ADDR
               PERFORM 3860-LIMIT-CONTROL
               PERFORM 3870-SET-SOURCE
               PERFORM 3880-ACCRUE-CONTROL
               PERFORM 3890-EDIT-DESC
               PERFORM 3900-CHECK-LAST-PAY
               PERFORM 3910-SET-TRAN
               PERFORM 3920-CHECK-LAST-STMT
               PERFORM 3930-SET-REASON
               PERFORM 3940-AGE-LAST-MAINT
               PERFORM 3950-AGE-EFF
               PERFORM 3960-FLAG-AMOUNT-2
               PERFORM 3970-FLAG-AMOUNT-2
               PERFORM 3980-DESCRIBE-TRAN
               PERFORM 3990-PRORATE-AMOUNT-1
               PERFORM 4000-ACCUM-WINDOW-RULE
               PERFORM 4010-FLAG-AMOUNT-2
               PERFORM 4020-COUNT-RETRY
               PERFORM 4030-LIMIT-AMOUNT-2
               PERFORM 4040-ACCRUE-CONTROL
               PERFORM 4050-FLAG-AMOUNT-1
               PERFORM 4060-SELECT-PENALTY-PCT
               PERFORM 4070-DESCRIBE-WINDOW-RULE
               PERFORM 4080-EDIT-CONTACT
               PERFORM 4090-AGE-EFF
               PERFORM 4100-EDIT-ADDR
               PERFORM 4110-CHECK-OPEN
               PERFORM 4120-ACCRUE-PENALTY-PCT
               PERFORM 4130-ASSESS-REVIEW
               PERFORM 4140-CHECK-NOTICE
               PERFORM 4150-SELECT-PENALTY-PCT
               PERFORM 4160-AGE-LAST-STMT
               PERFORM 4170-ASSESS-EXEMPT
               PERFORM 4180-ASSESS-PREFERRED
               PERFORM 4190-COUNT-ITEM
               PERFORM 4200-SELECT-MARGIN
               PERFORM 4210-DESCRIBE-TRAN
               PERFORM 4220-SPLIT-AMOUNT-2
               PERFORM 4230-PRORATE-AMOUNT-1
               PERFORM 4240-ACCUM-SOURCE
               PERFORM 4250-PRORATE-AMOUNT-2
               PERFORM 4260-EDIT-OPERATOR
               PERFORM 4270-COUNT-SEQ
               PERFORM 4280-AGE-MATURITY
               PERFORM 4290-EDIT-REJECT
               PERFORM 4300-ACCRUE-CONTROL
               PERFORM 4310-PRORATE-CONTROL
               PERFORM 4320-SELECT-PENALTY-PCT
               PERFORM 4330-SPLIT-AMOUNT-1
               PERFORM 4340-EDIT-OPERATOR
               PERFORM 7040-CALL-YKLIM
               PERFORM 7020-CALL-YKSTAT
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
      * AMOUNT 2 AMT IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3000-LIMIT-AMOUNT-2.
           IF YDBM-AMOUNT-2-AMT < 0.00
               MOVE 0.00 TO YDBM-AMOUNT-2-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YDBM-AMOUNT-2-AMT > 500.00
               MOVE 500.00 TO YDBM-AMOUNT-2-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3010-ACCRUE-DISCOUNT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * YDBM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO YDBT-AMOUNT-1-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3020-DESCRIBE-SOURCE.
           EVALUATE YDBM-SOURCE-CD
               WHEN 'BNX'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'FTB'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'GAP'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'PAP'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'VGC'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'VJV'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON AMOUNT 2 YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3030-ACCRUE-AMOUNT-2.
           IF YDBM-AMOUNT-2-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   YDBM-AMOUNT-2-YTD * YDBM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO YDBM-AMOUNT-2-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 365 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3040-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF YDBM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YDBM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO YDBM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CONTROL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO AMOUNT 2 AMT.
      *----------------------------------------------------------------*
       3050-PRORATE-AMOUNT-2.
           IF WS-PERIOD-DAYS > 0
               COMPUTE YDBT-AMOUNT-2-AMT ROUNDED =
                   YDBT-CONTROL-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO YDBT-AMOUNT-2-AMT
           END-IF
           ADD YDBT-AMOUNT-2-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT AMOUNT 2 AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3060-SPLIT-AMOUNT-2.
           COMPUTE WS-WORK-AMT = YDBM-AMOUNT-2-AMT / 4
           COMPUTE WS-WORK-AMT-2 = YDBM-AMOUNT-2-AMT - (WS-WORK-AMT * 4)
           COMPUTE YDBM-AMOUNT-1-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE AMOUNT 1 TIER TIER FOR THE AMOUNT 2 AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3070-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-AMOUNT-1-TIER-RATE(1) TO YDBM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF YDBT-AMOUNT-2-AMT NOT < WS-AMOUNT-1-TIER-LOW(WS-TX)
                   MOVE WS-AMOUNT-1-TIER-RATE(WS-TX) TO YDBM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF YDBM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO YDBM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE AMOUNT 1 SCHED TIER FOR THE CONTROL YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3080-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-AMOUNT-1-SCHED-RATE(1) TO YDBM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF YDBM-CONTROL-YTD NOT < WS-AMOUNT-1-SCHED-LOW(WS-TX)
                   MOVE WS-AMOUNT-1-SCHED-RATE(WS-TX) TO YDBM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF YDBM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO YDBM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3090-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * YDBM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CONTROL YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3100-FLAG-CONTROL.
           IF YDBM-CONTROL-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE YDBM-CONTROL-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE AMOUNT 1 SCHED TIER FOR THE CONTROL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3110-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-AMOUNT-1-SCHED-RATE(1) TO YDBM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF YDBT-CONTROL-AMT NOT < WS-AMOUNT-1-SCHED-LOW(WS-TX)
                   MOVE WS-AMOUNT-1-SCHED-RATE(WS-TX) TO
                       YDBM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF YDBM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO YDBM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E317 MEMO REQUIRED
      * E585 CONTROL YTD NEGATIVE
      * E962 CONTROL YTD OVER 25000
      * E746 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3120-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YDBT-MEMO = SPACES
                   MOVE 'E317' TO WS-REJECT-CODE
               WHEN YDBM-CONTROL-YTD < 0
                   MOVE 'E585' TO WS-REJECT-CODE
               WHEN YDBM-CONTROL-YTD > 25000.00
                   MOVE 'E962' TO WS-REJECT-CODE
               WHEN YDBT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E746' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CONTROL AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3130-ACCRUE-CONTROL.
           IF YDBM-CONTROL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   YDBM-CONTROL-AMT * YDBM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO YDBM-AMOUNT-1-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-90, 91-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3140-AGE-POST.
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
                   ADD YDBM-AMOUNT-2-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD YDBM-AMOUNT-2-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD YDBM-AMOUNT-2-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD YDBM-AMOUNT-2-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD YDBM-AMOUNT-2-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3150-LIMIT-ITEM.
           IF WS-WORK-AMT < 25.00
               MOVE 25.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 2500.00
               MOVE 2500.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CONTROL AMT IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3160-LIMIT-CONTROL.
           IF YDBT-CONTROL-AMT < 50.00
               MOVE 50.00 TO YDBT-CONTROL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YDBT-CONTROL-AMT > 5000.00
               MOVE 5000.00 TO YDBT-CONTROL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3170-AGE-NEXT-DUE.
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
                   ADD YDBT-AMOUNT-2-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD YDBT-AMOUNT-2-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD YDBT-AMOUNT-2-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD YDBT-AMOUNT-2-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD YDBT-AMOUNT-2-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AMOUNT 2 YTD IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3180-LIMIT-AMOUNT-2.
           IF YDBM-AMOUNT-2-YTD < 0.00
               MOVE 0.00 TO YDBM-AMOUNT-2-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YDBM-AMOUNT-2-YTD > 1000.00
               MOVE 1000.00 TO YDBM-AMOUNT-2-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON AMOUNT 2 AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3190-ACCRUE-AMOUNT-2.
           IF YDBT-AMOUNT-2-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   YDBT-AMOUNT-2-AMT * YDBM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CONTROL YTD FEE ON THE AMOUNT 1 AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3200-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN YDBT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO YDBM-CONTROL-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YDBM-PREFERRED-FLAG = 'Y'
                   COMPUTE YDBM-CONTROL-YTD ROUNDED = YDBM-AMOUNT-1-AMT
                       * YDBM-PENALTY-PCT / 200
               WHEN YDBT-TRAN-CD = 'CV'
                   MOVE 15.00 TO YDBM-CONTROL-YTD
               WHEN YDBT-TRAN-CD = 'RV'
                   MOVE 20.00 TO YDBM-CONTROL-YTD
               WHEN OTHER
                   COMPUTE YDBM-CONTROL-YTD ROUNDED = YDBM-AMOUNT-1-AMT
                       * YDBM-PENALTY-PCT / 100
           END-EVALUATE
           IF YDBM-CONTROL-YTD > 0 AND YDBM-CONTROL-YTD < 10.00
               MOVE 10.00 TO YDBM-CONTROL-YTD
           END-IF
           IF YDBM-CONTROL-YTD > 25.00
               MOVE 25.00 TO YDBM-CONTROL-YTD
           END-IF
           ADD YDBM-CONTROL-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E410 DESC REQUIRED
      * E863 WORK AMT 2 NEGATIVE
      * E104 WORK AMT 2 OVER 250000
      * E593 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3210-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YDBM-DESC = SPACES
                   MOVE 'E410' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E863' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 250000.00
                   MOVE 'E104' TO WS-REJECT-CODE
               WHEN YDBM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E593' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT 2 AMT IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3220-LIMIT-AMOUNT-2.
           IF YDBM-AMOUNT-2-AMT < 10.00
               MOVE 10.00 TO YDBM-AMOUNT-2-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YDBM-AMOUNT-2-AMT > 500.00
               MOVE 500.00 TO YDBM-AMOUNT-2-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT 2 YTD IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3230-LIMIT-AMOUNT-2.
           IF YDBM-AMOUNT-2-YTD < 100.00
               MOVE 100.00 TO YDBM-AMOUNT-2-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YDBM-AMOUNT-2-YTD > 500.00
               MOVE 500.00 TO YDBM-AMOUNT-2-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE AMOUNT 2 AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3240-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN YDBT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YDBM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = YDBT-AMOUNT-2-AMT *
                       YDBM-DISCOUNT / 200
               WHEN YDBM-PIVOT-CD = 'KGT'
                   MOVE 3.00 TO WS-WORK-AMT
               WHEN YDBM-PIVOT-CD = 'NNG'
                   MOVE 15.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = YDBT-AMOUNT-2-AMT *
                       YDBM-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 150.00
               MOVE 150.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AMOUNT 1 AMT IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3250-LIMIT-AMOUNT-1.
           IF YDBT-AMOUNT-1-AMT < 10.00
               MOVE 10.00 TO YDBT-AMOUNT-1-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YDBT-AMOUNT-1-AMT > 50000.00
               MOVE 50000.00 TO YDBT-AMOUNT-1-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CONTROL YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3260-FLAG-CONTROL.
           IF YDBM-CONTROL-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE YDBM-CONTROL-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CONTROL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3270-PRORATE-CONTROL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   YDBT-CONTROL-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CONTROL AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3280-ACCRUE-CONTROL.
           IF YDBM-CONTROL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   YDBM-CONTROL-AMT * YDBM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO YDBM-CONTROL-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CONTROL YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3290-FLAG-CONTROL.
           IF YDBM-CONTROL-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE YDBM-CONTROL-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PIVOT CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3300-DESCRIBE-PIVOT.
           EVALUATE YDBM-PIVOT-CD
               WHEN 'KGT'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'NNG'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'PTC'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'WZW'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * AX -> LK WHEN POST DATE IS PAST
      * LK -> NT WHEN POST DATE IS PAST
      * NT -> UW WHEN POST DATE IS PAST
      * UW -> AX WHEN POST DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3310-SET-REASON.
           EVALUATE YDBM-REASON-CD
               WHEN 'AX'
                   IF YDBT-POST-DATE > 0 AND YDBT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LK' TO YDBM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LK'
                   IF YDBT-POST-DATE > 0 AND YDBT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NT' TO YDBM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NT'
                   IF YDBT-POST-DATE > 0 AND YDBT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UW' TO YDBM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UW'
                   IF YDBT-POST-DATE > 0 AND YDBT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AX' TO YDBM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3320-AGE-LAST-MAINT.
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
                   ADD YDBT-AMOUNT-1-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD YDBT-AMOUNT-1-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD YDBT-AMOUNT-1-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD YDBT-AMOUNT-1-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD YDBT-AMOUNT-1-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E349 MEMO REQUIRED
      * E056 AMOUNT 2 YTD NEGATIVE
      * E704 AMOUNT 2 YTD OVER 50000
      * E116 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3330-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YDBT-MEMO = SPACES
                   MOVE 'E349' TO WS-REJECT-CODE
               WHEN YDBM-AMOUNT-2-YTD < 0
                   MOVE 'E056' TO WS-REJECT-CODE
               WHEN YDBM-AMOUNT-2-YTD > 50000.00
                   MOVE 'E704' TO WS-REJECT-CODE
               WHEN YDBT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E116' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3340-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3350-COUNT-PAYMENT.
           IF YDBM-PAYMENT-CNT < 9999999
               ADD 1 TO YDBM-PAYMENT-CNT
           END-IF
           IF YDBM-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO YDBM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CONTROL AMT IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3360-LIMIT-CONTROL.
           IF YDBM-CONTROL-AMT < 10.00
               MOVE 10.00 TO YDBM-CONTROL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YDBM-CONTROL-AMT > 5000.00
               MOVE 5000.00 TO YDBM-CONTROL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE WINDOW RULE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3370-DESCRIBE-WINDOW-RULE.
           EVALUATE YDBM-WINDOW-RULE-CD
               WHEN 'A'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'D'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'S'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'T'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CONTROL AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3380-FLAG-CONTROL.
           IF YDBT-CONTROL-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE YDBT-CONTROL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3390-DESCRIBE-CHANNEL.
           EVALUATE YDBT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE CONTROL FEE ON THE AMOUNT 1 AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3400-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN YDBM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO YDBM-CONTROL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YDBM-REVIEW-FLAG = 'Y'
                   COMPUTE YDBM-CONTROL-AMT ROUNDED = YDBT-AMOUNT-1-AMT
                       * YDBM-MARGIN / 200
               WHEN YDBM-REGION-CD = 'C'
                   MOVE 3.00 TO YDBM-CONTROL-AMT
               WHEN YDBM-REGION-CD = 'H'
                   MOVE 15.00 TO YDBM-CONTROL-AMT
               WHEN OTHER
                   COMPUTE YDBM-CONTROL-AMT ROUNDED = YDBT-AMOUNT-1-AMT
                       * YDBM-MARGIN / 100
           END-EVALUATE
           IF YDBM-CONTROL-AMT > 0 AND YDBM-CONTROL-AMT < 10.00
               MOVE 10.00 TO YDBM-CONTROL-AMT
           END-IF
           IF YDBM-CONTROL-AMT > 100.00
               MOVE 100.00 TO YDBM-CONTROL-AMT
           END-IF
           ADD YDBM-CONTROL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE CONTROL FEE ON THE CONTROL YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3410-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN YDBM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO YDBT-CONTROL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YDBT-REVERSAL-FLAG = 'Y'
                   COMPUTE YDBT-CONTROL-AMT ROUNDED = YDBM-CONTROL-YTD *
                       YDBM-DISCOUNT / 200
               WHEN YDBM-SOURCE-CD = 'BNX'
                   MOVE 5.00 TO YDBT-CONTROL-AMT
               WHEN YDBM-SOURCE-CD = 'FTB'
                   MOVE 3.00 TO YDBT-CONTROL-AMT
               WHEN OTHER
                   COMPUTE YDBT-CONTROL-AMT ROUNDED = YDBM-CONTROL-YTD *
                       YDBM-DISCOUNT / 100
           END-EVALUATE
           IF YDBT-CONTROL-AMT > 0 AND YDBT-CONTROL-AMT < 1.00
               MOVE 1.00 TO YDBT-CONTROL-AMT
           END-IF
           IF YDBT-CONTROL-AMT > 150.00
               MOVE 150.00 TO YDBT-CONTROL-AMT
           END-IF
           ADD YDBT-CONTROL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE AMOUNT 1 AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CONTROL AMT.
      *----------------------------------------------------------------*
       3420-PRORATE-CONTROL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE YDBT-CONTROL-AMT ROUNDED =
                   YDBM-AMOUNT-1-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO YDBT-CONTROL-AMT
           END-IF
           ADD YDBT-CONTROL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE AMOUNT 1 SCHED TIER FOR THE AMOUNT 1 AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3430-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-AMOUNT-1-SCHED-RATE(1) TO YDBM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF YDBT-AMOUNT-1-AMT NOT < WS-AMOUNT-1-SCHED-LOW(WS-TX)
                   MOVE WS-AMOUNT-1-SCHED-RATE(WS-TX) TO YDBM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF YDBM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO YDBM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT 1 AMT IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3440-LIMIT-AMOUNT-1.
           IF YDBT-AMOUNT-1-AMT < 25.00
               MOVE 25.00 TO YDBT-AMOUNT-1-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YDBT-AMOUNT-1-AMT > 5000.00
               MOVE 5000.00 TO YDBT-AMOUNT-1-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE AMOUNT 1 GRID TIER FOR THE CONTROL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3450-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-AMOUNT-1-GRID-RATE(1) TO YDBM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF YDBT-CONTROL-AMT NOT < WS-AMOUNT-1-GRID-LOW(WS-TX)
                   MOVE WS-AMOUNT-1-GRID-RATE(WS-TX) TO YDBM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF YDBM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO YDBM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT 2 AMT IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3460-LIMIT-AMOUNT-2.
           IF YDBT-AMOUNT-2-AMT < 50.00
               MOVE 50.00 TO YDBT-AMOUNT-2-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YDBT-AMOUNT-2-AMT > 50000.00
               MOVE 50000.00 TO YDBT-AMOUNT-2-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PIVOT CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3470-DESCRIBE-PIVOT.
           EVALUATE YDBM-PIVOT-CD
               WHEN 'KGT'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'NNG'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'PTC'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'WZW'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3480-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF YDBM-NOTICE-DATE > 0 AND YDBM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YDBM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD YDBM-AMOUNT-1-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD YDBM-AMOUNT-1-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD YDBM-AMOUNT-1-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD YDBM-AMOUNT-1-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD YDBM-AMOUNT-1-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3490-COUNT-RETRY.
           IF YDBM-RETRY-CNT < 9999999
               ADD 1 TO YDBM-RETRY-CNT
           END-IF
           IF YDBM-RETRY-CNT NOT < 3
               MOVE 'Y' TO YDBT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE AMOUNT 2 YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO AMOUNT 2 AMT.
      *----------------------------------------------------------------*
       3500-PRORATE-AMOUNT-2.
           IF WS-PERIOD-DAYS > 0
               COMPUTE YDBT-AMOUNT-2-AMT ROUNDED =
                   YDBM-AMOUNT-2-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO YDBT-AMOUNT-2-AMT
           END-IF
           ADD YDBT-AMOUNT-2-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3510-COUNT-PAYMENT.
           IF YDBM-PAYMENT-CNT < 9999999
               ADD 1 TO YDBM-PAYMENT-CNT
           END-IF
           IF YDBM-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO YDBM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 45 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3520-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF YDBM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YDBM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO YDBM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3530-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF YDBM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YDBM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO YDBM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3540-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E144 CONTACT REQUIRED
      * E247 AMOUNT 1 YTD NEGATIVE
      * E311 AMOUNT 1 YTD OVER 25000
      * E268 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3550-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YDBM-CONTACT = SPACES
                   MOVE 'E144' TO WS-REJECT-CODE
               WHEN YDBM-AMOUNT-1-YTD < 0
                   MOVE 'E247' TO WS-REJECT-CODE
               WHEN YDBM-AMOUNT-1-YTD > 25000.00
                   MOVE 'E311' TO WS-REJECT-CODE
               WHEN YDBM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E268' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT AMOUNT 2 YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3560-SPLIT-AMOUNT-2.
           COMPUTE WS-WORK-AMT = YDBM-AMOUNT-2-YTD / 4
           COMPUTE WS-WORK-AMT-2 = YDBM-AMOUNT-2-YTD - (WS-WORK-AMT * 4)
           COMPUTE YDBM-CONTROL-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3570-COUNT-CYCLE.
           IF YDBM-CYCLE-CNT < 9999999
               ADD 1 TO YDBM-CYCLE-CNT
           END-IF
           IF YDBM-CYCLE-CNT NOT < 5
               MOVE 'Y' TO YDBM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3580-COUNT-SEQ.
           IF YDBT-SEQ < 9999999
               ADD 1 TO YDBT-SEQ
           END-IF
           IF YDBT-SEQ NOT < 10
               MOVE 'Y' TO YDBM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CONTROL FEE ON THE AMOUNT 1 AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3590-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN YDBM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO YDBM-CONTROL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YDBM-PREFERRED-FLAG = 'Y'
                   COMPUTE YDBM-CONTROL-AMT ROUNDED = YDBM-AMOUNT-1-AMT
                       * YDBM-DISCOUNT / 200
               WHEN YDBM-REGION-CD = 'C'
                   MOVE 15.00 TO YDBM-CONTROL-AMT
               WHEN YDBM-REGION-CD = 'H'
                   MOVE 15.00 TO YDBM-CONTROL-AMT
               WHEN OTHER
                   COMPUTE YDBM-CONTROL-AMT ROUNDED = YDBM-AMOUNT-1-AMT
                       * YDBM-DISCOUNT / 100
           END-EVALUATE
           IF YDBM-CONTROL-AMT > 0 AND YDBM-CONTROL-AMT < 1.00
               MOVE 1.00 TO YDBM-CONTROL-AMT
           END-IF
           IF YDBM-CONTROL-AMT > 25.00
               MOVE 25.00 TO YDBM-CONTROL-AMT
           END-IF
           ADD YDBM-CONTROL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 365 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3600-CHECK-EXPIRY.
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
           IF WS-DAYS > 365
               MOVE 'Y' TO YDBT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E842 ADDR REQUIRED
      * E251 CONTROL YTD NEGATIVE
      * E260 CONTROL YTD OVER 99999
      * E934 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3610-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YDBM-ADDR = SPACES
                   MOVE 'E842' TO WS-REJECT-CODE
               WHEN YDBM-CONTROL-YTD < 0
                   MOVE 'E251' TO WS-REJECT-CODE
               WHEN YDBM-CONTROL-YTD > 99999.00
                   MOVE 'E260' TO WS-REJECT-CODE
               WHEN YDBM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E934' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE AMOUNT 2 AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO AMOUNT 2 YTD.
      *----------------------------------------------------------------*
       3620-PRORATE-AMOUNT-2.
           IF WS-PERIOD-DAYS > 0
               COMPUTE YDBM-AMOUNT-2-YTD ROUNDED =
                   YDBT-AMOUNT-2-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO YDBM-AMOUNT-2-YTD
           END-IF
           ADD YDBM-AMOUNT-2-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE AMOUNT 2 FEE ON THE AMOUNT 1 AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3630-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN YDBM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO YDBT-AMOUNT-2-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YDBM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE YDBT-AMOUNT-2-AMT ROUNDED = YDBM-AMOUNT-1-AMT
                       * YDBM-DISCOUNT / 200
               WHEN YDBM-SOURCE-CD = 'BNX'
                   MOVE 12.00 TO YDBT-AMOUNT-2-AMT
               WHEN YDBM-SOURCE-CD = 'FTB'
                   MOVE 12.00 TO YDBT-AMOUNT-2-AMT
               WHEN OTHER
                   COMPUTE YDBT-AMOUNT-2-AMT ROUNDED = YDBM-AMOUNT-1-AMT
                       * YDBM-DISCOUNT / 100
           END-EVALUATE
           IF YDBT-AMOUNT-2-AMT > 0 AND YDBT-AMOUNT-2-AMT < 10.00
               MOVE 10.00 TO YDBT-AMOUNT-2-AMT
           END-IF
           IF YDBT-AMOUNT-2-AMT > 25.00
               MOVE 25.00 TO YDBT-AMOUNT-2-AMT
           END-IF
           ADD YDBT-AMOUNT-2-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AMOUNT 2 AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3640-FLAG-AMOUNT-2.
           IF YDBM-AMOUNT-2-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE YDBM-AMOUNT-2-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * CONTROL AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3650-FLAG-CONTROL.
           IF YDBT-CONTROL-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE YDBT-CONTROL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3660-ACCRUE-DISCOUNT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * YDBM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO YDBM-AMOUNT-1-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE AMOUNT 2 YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CONTROL AMT.
      *----------------------------------------------------------------*
       3670-PRORATE-CONTROL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE YDBM-CONTROL-AMT ROUNDED =
                   YDBM-AMOUNT-2-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO YDBM-CONTROL-AMT
           END-IF
           ADD YDBM-CONTROL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E717 MEMO REQUIRED
      * E408 WORK AMT 2 NEGATIVE
      * E786 WORK AMT 2 OVER 5000
      * E992 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3680-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YDBT-MEMO = SPACES
                   MOVE 'E717' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E408' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 5000.00
                   MOVE 'E786' TO WS-REJECT-CODE
               WHEN YDBM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E992' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE AMOUNT 1 TIER TIER FOR THE AMOUNT 1 AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3690-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-AMOUNT-1-TIER-RATE(1) TO YDBM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF YDBM-AMOUNT-1-AMT NOT < WS-AMOUNT-1-TIER-LOW(WS-TX)
                   MOVE WS-AMOUNT-1-TIER-RATE(WS-TX) TO YDBM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF YDBM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO YDBM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE AMOUNT 1 FEE ON THE AMOUNT 2 AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3700-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN YDBM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO YDBM-AMOUNT-1-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YDBM-PREFERRED-FLAG = 'Y'
                   COMPUTE YDBM-AMOUNT-1-AMT ROUNDED = YDBM-AMOUNT-2-AMT
                       * YDBM-PENALTY-PCT / 200
               WHEN YDBM-CYCLE-CD = 'FJ'
                   MOVE 5.00 TO YDBM-AMOUNT-1-AMT
               WHEN YDBM-CYCLE-CD = 'LA'
                   MOVE 5.00 TO YDBM-AMOUNT-1-AMT
               WHEN OTHER
                   COMPUTE YDBM-AMOUNT-1-AMT ROUNDED = YDBM-AMOUNT-2-AMT
                       * YDBM-PENALTY-PCT / 100
           END-EVALUATE
           IF YDBM-AMOUNT-1-AMT > 0 AND YDBM-AMOUNT-1-AMT < 1.00
               MOVE 1.00 TO YDBM-AMOUNT-1-AMT
           END-IF
           IF YDBM-AMOUNT-1-AMT > 50.00
               MOVE 50.00 TO YDBM-AMOUNT-1-AMT
           END-IF
           ADD YDBM-AMOUNT-1-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE AMOUNT 1 FEE ON THE AMOUNT 1 AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3710-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN YDBM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO YDBT-AMOUNT-1-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YDBM-REVIEW-FLAG = 'Y'
                   COMPUTE YDBT-AMOUNT-1-AMT ROUNDED = YDBM-AMOUNT-1-AMT
                       * YDBM-DISCOUNT / 200
               WHEN YDBM-REASON-CD = 'AX'
                   MOVE 7.00 TO YDBT-AMOUNT-1-AMT
               WHEN YDBM-REASON-CD = 'LK'
                   MOVE 3.00 TO YDBT-AMOUNT-1-AMT
               WHEN OTHER
                   COMPUTE YDBT-AMOUNT-1-AMT ROUNDED = YDBM-AMOUNT-1-AMT
                       * YDBM-DISCOUNT / 100
           END-EVALUATE
           IF YDBT-AMOUNT-1-AMT > 0 AND YDBT-AMOUNT-1-AMT < 10.00
               MOVE 10.00 TO YDBT-AMOUNT-1-AMT
           END-IF
           IF YDBT-AMOUNT-1-AMT > 50.00
               MOVE 50.00 TO YDBT-AMOUNT-1-AMT
           END-IF
           ADD YDBT-AMOUNT-1-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3720-COUNT-RETRY.
           IF YDBM-RETRY-CNT < 9999999
               ADD 1 TO YDBM-RETRY-CNT
           END-IF
           IF YDBM-RETRY-CNT NOT < 5
               MOVE 'Y' TO YDBM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE AMOUNT 2 AMT BY WINDOW RULE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3730-ACCUM-WINDOW-RULE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YDBM-WINDOW-RULE-CD
                   ADD YDBT-AMOUNT-2-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YDBM-WINDOW-RULE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE YDBT-AMOUNT-2-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3740-DESCRIBE-REASON.
           EVALUATE YDBM-REASON-CD
               WHEN 'AX'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'LK'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'NT'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'UW'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3750-DESCRIBE-SOURCE.
           EVALUATE YDBM-SOURCE-CD
               WHEN 'BNX'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'FTB'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'GAP'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'PAP'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'VGC'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'VJV'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE PIVOT CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3760-DESCRIBE-PIVOT.
           EVALUATE YDBM-PIVOT-CD
               WHEN 'KGT'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'NNG'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'PTC'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'WZW'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE AMOUNT 1 SCHED TIER FOR THE AMOUNT 2 YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3770-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-AMOUNT-1-SCHED-RATE(1) TO YDBM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF YDBM-AMOUNT-2-YTD NOT < WS-AMOUNT-1-SCHED-LOW(WS-TX)
                   MOVE WS-AMOUNT-1-SCHED-RATE(WS-TX) TO YDBM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF YDBM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO YDBM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE WINDOW RULE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3780-DESCRIBE-WINDOW-RULE.
           EVALUATE YDBM-WINDOW-RULE-CD
               WHEN 'A'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'D'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'S'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'T'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3790-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF YDBM-MATURITY-DATE > 0 AND YDBM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YDBM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD YDBT-CONTROL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD YDBT-CONTROL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD YDBT-CONTROL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD YDBT-CONTROL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD YDBT-CONTROL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT AMOUNT 1 AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3800-SPLIT-AMOUNT-1.
           COMPUTE WS-WORK-AMT = YDBM-AMOUNT-1-AMT / 3
           COMPUTE WS-WORK-AMT-2 = YDBM-AMOUNT-1-AMT - (WS-WORK-AMT * 3)
           COMPUTE YDBM-CONTROL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3810-SPLIT-AMOUNT-1.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 12)
           COMPUTE YDBM-AMOUNT-1-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT AMOUNT 1 AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3820-SPLIT-AMOUNT-1.
           COMPUTE WS-WORK-AMT = YDBM-AMOUNT-1-AMT / 3
           COMPUTE WS-WORK-AMT-2 = YDBM-AMOUNT-1-AMT - (WS-WORK-AMT * 3)
           COMPUTE YDBT-AMOUNT-2-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CONTROL AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3830-FLAG-CONTROL.
           IF YDBT-CONTROL-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE YDBT-CONTROL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E268 NAME REQUIRED
      * E734 AMOUNT 2 YTD NEGATIVE
      * E624 AMOUNT 2 YTD OVER 1000000
      * E159 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3840-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YDBM-NAME = SPACES
                   MOVE 'E268' TO WS-REJECT-CODE
               WHEN YDBM-AMOUNT-2-YTD < 0
                   MOVE 'E734' TO WS-REJECT-CODE
               WHEN YDBM-AMOUNT-2-YTD > 1000000.00
                   MOVE 'E624' TO WS-REJECT-CODE
               WHEN YDBT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E159' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E723 ADDR REQUIRED
      * E595 AMOUNT 1 YTD NEGATIVE
      * E950 AMOUNT 1 YTD OVER 99999
      * E831 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3850-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YDBM-ADDR = SPACES
                   MOVE 'E723' TO WS-REJECT-CODE
               WHEN YDBM-AMOUNT-1-YTD < 0
                   MOVE 'E595' TO WS-REJECT-CODE
               WHEN YDBM-AMOUNT-1-YTD > 99999.00
                   MOVE 'E950' TO WS-REJECT-CODE
               WHEN YDBT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E831' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CONTROL AMT IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3860-LIMIT-CONTROL.
           IF YDBM-CONTROL-AMT < 50.00
               MOVE 50.00 TO YDBM-CONTROL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YDBM-CONTROL-AMT > 500.00
               MOVE 500.00 TO YDBM-CONTROL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * BNX -> FTB WHEN LAST MAINT DATE IS PAST
      * FTB -> GAP WHEN LAST MAINT DATE IS PAST
      * GAP -> PAP WHEN LAST MAINT DATE IS PAST
      * PAP -> VGC WHEN LAST MAINT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3870-SET-SOURCE.
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
      * DAILY INTEREST ON CONTROL YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3880-ACCRUE-CONTROL.
           IF YDBM-CONTROL-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   YDBM-CONTROL-YTD * YDBM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E970 DESC REQUIRED
      * E112 WORK AMT NEGATIVE
      * E179 WORK AMT OVER 10000
      * E568 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3890-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YDBM-DESC = SPACES
                   MOVE 'E970' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E112' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 10000.00
                   MOVE 'E179' TO WS-REJECT-CODE
               WHEN YDBM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E568' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 10 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3900-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF YDBM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YDBM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO YDBT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * CV -> RV WHEN POST DATE IS PAST
      * RV -> TS WHEN POST DATE IS PAST
      * TS -> CV WHEN POST DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3910-SET-TRAN.
           EVALUATE YDBT-TRAN-CD
               WHEN 'CV'
                   IF YDBT-POST-DATE > 0 AND YDBT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RV' TO YDBT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RV'
                   IF YDBT-POST-DATE > 0 AND YDBT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TS' TO YDBT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TS'
                   IF YDBT-POST-DATE > 0 AND YDBT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CV' TO YDBT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 30 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3920-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF YDBM-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YDBM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO YDBM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * AX -> LK WHEN POST DATE IS PAST
      * LK -> NT WHEN POST DATE IS PAST
      * NT -> UW WHEN POST DATE IS PAST
      * UW -> AX WHEN POST DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3930-SET-REASON.
           EVALUATE YDBM-REASON-CD
               WHEN 'AX'
                   IF YDBT-POST-DATE > 0 AND YDBT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LK' TO YDBM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LK'
                   IF YDBT-POST-DATE > 0 AND YDBT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NT' TO YDBM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NT'
                   IF YDBT-POST-DATE > 0 AND YDBT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UW' TO YDBM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UW'
                   IF YDBT-POST-DATE > 0 AND YDBT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AX' TO YDBM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3940-AGE-LAST-MAINT.
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
               WHEN WS-DAYS NOT > 45
                   ADD YDBM-AMOUNT-1-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD YDBM-AMOUNT-1-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD YDBM-AMOUNT-1-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD YDBM-AMOUNT-1-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3950-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF YDBT-EFF-DATE > 0 AND YDBT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YDBT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD YDBT-AMOUNT-2-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD YDBT-AMOUNT-2-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD YDBT-AMOUNT-2-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD YDBT-AMOUNT-2-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD YDBT-AMOUNT-2-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AMOUNT 2 AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3960-FLAG-AMOUNT-2.
           IF YDBT-AMOUNT-2-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE YDBT-AMOUNT-2-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT 2 YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3970-FLAG-AMOUNT-2.
           IF YDBM-AMOUNT-2-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE YDBM-AMOUNT-2-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3980-DESCRIBE-TRAN.
           EVALUATE YDBT-TRAN-CD
               WHEN 'CV'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'RV'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'TS'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE AMOUNT 2 AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO AMOUNT 1 YTD.
      *----------------------------------------------------------------*
       3990-PRORATE-AMOUNT-1.
           IF WS-PERIOD-DAYS > 0
               COMPUTE YDBM-AMOUNT-1-YTD ROUNDED =
                   YDBT-AMOUNT-2-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO YDBM-AMOUNT-1-YTD
           END-IF
           ADD YDBM-AMOUNT-1-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE AMOUNT 2 AMT BY WINDOW RULE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       4000-ACCUM-WINDOW-RULE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YDBM-WINDOW-RULE-CD
                   ADD YDBT-AMOUNT-2-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YDBM-WINDOW-RULE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE YDBT-AMOUNT-2-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT 2 YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4010-FLAG-AMOUNT-2.
           IF YDBM-AMOUNT-2-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE YDBM-AMOUNT-2-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       4020-COUNT-RETRY.
           IF YDBM-RETRY-CNT < 9999999
               ADD 1 TO YDBM-RETRY-CNT
           END-IF
           IF YDBM-RETRY-CNT NOT < 5
               MOVE 'Y' TO YDBM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT 2 YTD IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4030-LIMIT-AMOUNT-2.
           IF YDBM-AMOUNT-2-YTD < 50.00
               MOVE 50.00 TO YDBM-AMOUNT-2-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YDBM-AMOUNT-2-YTD > 10000.00
               MOVE 10000.00 TO YDBM-AMOUNT-2-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CONTROL YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4040-ACCRUE-CONTROL.
           IF YDBM-CONTROL-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   YDBM-CONTROL-YTD * YDBM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO YDBM-AMOUNT-1-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT 1 YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4050-FLAG-AMOUNT-1.
           IF YDBM-AMOUNT-1-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE YDBM-AMOUNT-1-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE AMOUNT 1 SCHED TIER FOR THE CONTROL YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4060-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-AMOUNT-1-SCHED-RATE(1) TO YDBM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF YDBM-CONTROL-YTD NOT < WS-AMOUNT-1-SCHED-LOW(WS-TX)
                   MOVE WS-AMOUNT-1-SCHED-RATE(WS-TX) TO
                       YDBM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF YDBM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO YDBM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE WINDOW RULE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4070-DESCRIBE-WINDOW-RULE.
           EVALUATE YDBM-WINDOW-RULE-CD
               WHEN 'A'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'D'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'S'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'T'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E859 CONTACT REQUIRED
      * E356 AMOUNT 2 AMT NEGATIVE
      * E429 AMOUNT 2 AMT OVER 1000000
      * E956 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       4080-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YDBM-CONTACT = SPACES
                   MOVE 'E859' TO WS-REJECT-CODE
               WHEN YDBM-AMOUNT-2-AMT < 0
                   MOVE 'E356' TO WS-REJECT-CODE
               WHEN YDBM-AMOUNT-2-AMT > 1000000.00
                   MOVE 'E429' TO WS-REJECT-CODE
               WHEN YDBM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E956' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4090-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF YDBT-EFF-DATE > 0 AND YDBT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YDBT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD YDBM-CONTROL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD YDBM-CONTROL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD YDBM-CONTROL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD YDBM-CONTROL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD YDBM-CONTROL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E422 ADDR REQUIRED
      * E020 AMOUNT 1 AMT NEGATIVE
      * E515 AMOUNT 1 AMT OVER 5000
      * E452 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       4100-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YDBM-ADDR = SPACES
                   MOVE 'E422' TO WS-REJECT-CODE
               WHEN YDBT-AMOUNT-1-AMT < 0
                   MOVE 'E020' TO WS-REJECT-CODE
               WHEN YDBT-AMOUNT-1-AMT > 5000.00
                   MOVE 'E515' TO WS-REJECT-CODE
               WHEN YDBM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E452' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 180 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4110-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF YDBM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YDBM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO YDBM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4120-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * YDBM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE AMOUNT 1 AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       4130-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN YDBM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YDBT-REVERSAL-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = YDBM-AMOUNT-1-AMT *
                       YDBM-MARGIN / 200
               WHEN YDBM-SOURCE-CD = 'BNX'
                   MOVE 15.00 TO WS-WORK-AMT
               WHEN YDBM-SOURCE-CD = 'FTB'
                   MOVE 7.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = YDBM-AMOUNT-1-AMT *
                       YDBM-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 2.00
               MOVE 2.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 150.00
               MOVE 150.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4140-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF YDBM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YDBM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO YDBT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE AMOUNT 1 TIER TIER FOR THE AMOUNT 2 AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4150-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-AMOUNT-1-TIER-RATE(1) TO YDBM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF YDBT-AMOUNT-2-AMT NOT < WS-AMOUNT-1-TIER-LOW(WS-TX)
                   MOVE WS-AMOUNT-1-TIER-RATE(WS-TX) TO YDBM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF YDBM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO YDBM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       4160-AGE-LAST-STMT.
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
                   ADD YDBT-AMOUNT-1-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD YDBT-AMOUNT-1-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD YDBT-AMOUNT-1-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD YDBT-AMOUNT-1-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD YDBT-AMOUNT-1-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE AMOUNT 1 FEE ON THE AMOUNT 2 AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4170-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN YDBM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO YDBM-AMOUNT-1-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YDBM-REVIEW-FLAG = 'Y'
                   COMPUTE YDBM-AMOUNT-1-AMT ROUNDED = YDBM-AMOUNT-2-AMT
                       * YDBM-PENALTY-PCT / 200
               WHEN YDBT-TRAN-CD = 'CV'
                   MOVE 12.00 TO YDBM-AMOUNT-1-AMT
               WHEN YDBT-TRAN-CD = 'RV'
                   MOVE 5.00 TO YDBM-AMOUNT-1-AMT
               WHEN OTHER
                   COMPUTE YDBM-AMOUNT-1-AMT ROUNDED = YDBM-AMOUNT-2-AMT
                       * YDBM-PENALTY-PCT / 100
           END-EVALUATE
           IF YDBM-AMOUNT-1-AMT > 0 AND YDBM-AMOUNT-1-AMT < 5.00
               MOVE 5.00 TO YDBM-AMOUNT-1-AMT
           END-IF
           IF YDBM-AMOUNT-1-AMT > 25.00
               MOVE 25.00 TO YDBM-AMOUNT-1-AMT
           END-IF
           ADD YDBM-AMOUNT-1-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE AMOUNT 2 YTD FEE ON THE AMOUNT 1 AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4180-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN YDBM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO YDBM-AMOUNT-2-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YDBM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE YDBM-AMOUNT-2-YTD ROUNDED = YDBT-AMOUNT-1-AMT
                       * YDBM-MARGIN / 200
               WHEN YDBM-CYCLE-CD = 'FJ'
                   MOVE 10.00 TO YDBM-AMOUNT-2-YTD
               WHEN YDBM-CYCLE-CD = 'LA'
                   MOVE 15.00 TO YDBM-AMOUNT-2-YTD
               WHEN OTHER
                   COMPUTE YDBM-AMOUNT-2-YTD ROUNDED = YDBT-AMOUNT-1-AMT
                       * YDBM-MARGIN / 100
           END-EVALUATE
           IF YDBM-AMOUNT-2-YTD > 0 AND YDBM-AMOUNT-2-YTD < 10.00
               MOVE 10.00 TO YDBM-AMOUNT-2-YTD
           END-IF
           IF YDBM-AMOUNT-2-YTD > 50.00
               MOVE 50.00 TO YDBM-AMOUNT-2-YTD
           END-IF
           ADD YDBM-AMOUNT-2-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       4190-COUNT-ITEM.
           IF YDBM-ITEM-CNT < 9999999
               ADD 1 TO YDBM-ITEM-CNT
           END-IF
           IF YDBM-ITEM-CNT NOT < 5
               MOVE 'Y' TO YDBM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE AMOUNT 1 GRID TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4200-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-AMOUNT-1-GRID-RATE(1) TO YDBM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF WS-WORK-AMT-2 NOT < WS-AMOUNT-1-GRID-LOW(WS-TX)
                   MOVE WS-AMOUNT-1-GRID-RATE(WS-TX) TO YDBM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF YDBM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO YDBM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4210-DESCRIBE-TRAN.
           EVALUATE YDBT-TRAN-CD
               WHEN 'CV'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'RV'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'TS'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT AMOUNT 2 AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4220-SPLIT-AMOUNT-2.
           COMPUTE WS-WORK-AMT = YDBT-AMOUNT-2-AMT / 12
           COMPUTE WS-WORK-AMT-2 = YDBT-AMOUNT-2-AMT - (WS-WORK-AMT *
               12)
           COMPUTE YDBM-AMOUNT-2-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE AMOUNT 1 AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO AMOUNT 1 AMT.
      *----------------------------------------------------------------*
       4230-PRORATE-AMOUNT-1.
           IF WS-PERIOD-DAYS > 0
               COMPUTE YDBT-AMOUNT-1-AMT ROUNDED =
                   YDBM-AMOUNT-1-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO YDBT-AMOUNT-1-AMT
           END-IF
           ADD YDBT-AMOUNT-1-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE AMOUNT 2 YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4240-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YDBM-SOURCE-CD
                   ADD YDBM-AMOUNT-2-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YDBM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE YDBM-AMOUNT-2-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CONTROL YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO AMOUNT 2 YTD.
      *----------------------------------------------------------------*
       4250-PRORATE-AMOUNT-2.
           IF WS-PERIOD-DAYS > 0
               COMPUTE YDBM-AMOUNT-2-YTD ROUNDED =
                   YDBM-CONTROL-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO YDBM-AMOUNT-2-YTD
           END-IF
           ADD YDBM-AMOUNT-2-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E028 OPERATOR REQUIRED
      * E311 CONTROL AMT NEGATIVE
      * E739 CONTROL AMT OVER 250000
      * E245 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       4260-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YDBT-OPERATOR = SPACES
                   MOVE 'E028' TO WS-REJECT-CODE
               WHEN YDBT-CONTROL-AMT < 0
                   MOVE 'E311' TO WS-REJECT-CODE
               WHEN YDBT-CONTROL-AMT > 250000.00
                   MOVE 'E739' TO WS-REJECT-CODE
               WHEN YDBM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E245' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4270-COUNT-SEQ.
           IF YDBT-SEQ < 9999999
               ADD 1 TO YDBT-SEQ
           END-IF
           IF YDBT-SEQ NOT < 6
               MOVE 'Y' TO YDBM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-60, 61-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4280-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF YDBM-MATURITY-DATE > 0 AND YDBM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YDBM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E924 REJECT CD REQUIRED
      * E140 CONTROL YTD NEGATIVE
      * E057 CONTROL YTD OVER 1000000
      * E919 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       4290-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YDBT-REJECT-CD = SPACES
                   MOVE 'E924' TO WS-REJECT-CODE
               WHEN YDBM-CONTROL-YTD < 0
                   MOVE 'E140' TO WS-REJECT-CODE
               WHEN YDBM-CONTROL-YTD > 1000000.00
                   MOVE 'E057' TO WS-REJECT-CODE
               WHEN YDBM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E919' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CONTROL YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4300-ACCRUE-CONTROL.
           IF YDBM-CONTROL-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   YDBM-CONTROL-YTD * YDBM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO YDBM-AMOUNT-2-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE AMOUNT 1 AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CONTROL AMT.
      *----------------------------------------------------------------*
       4310-PRORATE-CONTROL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE YDBT-CONTROL-AMT ROUNDED =
                   YDBM-AMOUNT-1-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO YDBT-CONTROL-AMT
           END-IF
           ADD YDBT-CONTROL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE AMOUNT 1 GRID TIER FOR THE AMOUNT 1 AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4320-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-AMOUNT-1-GRID-RATE(1) TO YDBM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF YDBT-AMOUNT-1-AMT NOT < WS-AMOUNT-1-GRID-LOW(WS-TX)
                   MOVE WS-AMOUNT-1-GRID-RATE(WS-TX) TO YDBM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF YDBM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO YDBM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT AMOUNT 1 AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4330-SPLIT-AMOUNT-1.
           COMPUTE WS-WORK-AMT = YDBM-AMOUNT-1-AMT / 3
           COMPUTE WS-WORK-AMT-2 = YDBM-AMOUNT-1-AMT - (WS-WORK-AMT * 3)
           COMPUTE YDBT-AMOUNT-1-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E110 OPERATOR REQUIRED
      * E695 AMOUNT 2 AMT NEGATIVE
      * E913 AMOUNT 2 AMT OVER 10000
      * E276 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       4340-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YDBT-OPERATOR = SPACES
                   MOVE 'E110' TO WS-REJECT-CODE
               WHEN YDBT-AMOUNT-2-AMT < 0
                   MOVE 'E695' TO WS-REJECT-CODE
               WHEN YDBT-AMOUNT-2-AMT > 10000.00
                   MOVE 'E913' TO WS-REJECT-CODE
               WHEN YDBM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E276' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
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
                   MOVE YKLIM-CONTROL-AMT
                     TO YDBM-CONTROL-AMT
                   MOVE YKLIM-AMOUNT-1-AMT
                     TO YDBM-AMOUNT-1-AMT
                   MOVE YKLIM-AMOUNT-2-YTD
                     TO YDBM-AMOUNT-2-YTD
                   MOVE YKLIM-OVERRIDE-FLAG
                     TO YDBM-OVERRIDE-FLAG
                   MOVE YKLIM-NOTICE-SENT-FLAG
                     TO YDBM-NOTICE-SENT-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RLIM' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7040-CALL-YKLIM' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE YDB FIELDS TO YKSTAT AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7020-CALL-YKSTAT.
           INITIALIZE YKSTAT-PARMS
           MOVE YDBM-AMOUNT-2-YTD
             TO YKSTAT-AMOUNT-2-YTD
           MOVE YDBM-AMOUNT-2-AMT
             TO YKSTAT-AMOUNT-2-AMT
           MOVE YDBM-AMOUNT-1-AMT
             TO YKSTAT-AMOUNT-1-AMT
           MOVE YDBM-AMOUNT-1-YTD
             TO YKSTAT-AMOUNT-1-YTD
           MOVE YDBM-CONTROL-YTD
             TO YKSTAT-CONTROL-YTD
           MOVE YDBM-CONTROL-AMT
             TO YKSTAT-CONTROL-AMT
           MOVE YDBM-NEXT-DUE-DATE
             TO YKSTAT-NEXT-DUE-DATE
           MOVE YDBM-LAST-PAY-DATE
             TO YKSTAT-LAST-PAY-DATE
           MOVE YDBM-LAST-STMT-DATE
             TO YKSTAT-LAST-STMT-DATE
           MOVE YDBM-LAST-MAINT-DATE
             TO YKSTAT-LAST-MAINT-DATE
           MOVE YDBM-REGION-CD
             TO YKSTAT-REGION-CD
           MOVE YDBM-CYCLE-CD
             TO YKSTAT-CYCLE-CD
           MOVE YDBM-SOURCE-CD
             TO YKSTAT-SOURCE-CD
           MOVE YDBM-REASON-CD
             TO YKSTAT-REASON-CD
           MOVE YDBM-AUTO-RENEW-FLAG
             TO YKSTAT-AUTO-RENEW-FLAG
           MOVE YDBM-PREFERRED-FLAG
             TO YKSTAT-PREFERRED-FLAG
           MOVE YDBM-NOTICE-SENT-FLAG
             TO YKSTAT-NOTICE-SENT-FLAG
           MOVE YDBM-RETRY-CNT
             TO YKSTAT-RETRY-CNT
           MOVE YDBM-CYCLE-CNT
             TO YKSTAT-CYCLE-CNT
           MOVE YDBM-PENALTY-PCT
             TO YKSTAT-PENALTY-PCT
           MOVE YDBM-MARGIN
             TO YKSTAT-MARGIN
           MOVE YDBM-DESC
             TO YKSTAT-DESC
           MOVE YDBM-CUST-ID
             TO YKSTAT-CUST-ID
           CALL 'YKSTAT' USING YKSTAT-PARMS
           EVALUATE YKSTAT-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE YKSTAT-AMOUNT-2-YTD
                     TO YDBM-AMOUNT-2-YTD
                   MOVE YKSTAT-AMOUNT-2-AMT
                     TO YDBM-AMOUNT-2-AMT
                   MOVE YKSTAT-AMOUNT-1-AMT
                     TO YDBM-AMOUNT-1-AMT
                   MOVE YKSTAT-AMOUNT-1-YTD
                     TO YDBM-AMOUNT-1-YTD
                   MOVE YKSTAT-NOTICE-SENT-FLAG
                     TO YDBM-NOTICE-SENT-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RTAT' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-YKSTAT' TO ABND-PARAGRAPH
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
           DISPLAY 'YKBRCON READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'YKBRCON' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
