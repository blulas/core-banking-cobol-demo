      *================================================================*
      * PROGRAM  : PPYAUDT                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : POSITIVE PAY - AUDIT TRAIL. READS THE PPY           *
      *            TRANSACTIONS, UPDATES THE PPY MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. CORPORATE POSITIVE PAY ISSUE    *
      *            FILE MATCHING AND EXCEPTIONS                        *
      * INPUT    : PPYMST (KSDS), PPYTRN, BANKCTL                      *
      * OUTPUT   : PPYOUT, PPYAURPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, PPELG                   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-11-10 BHS  ORIGINAL                                       *
      * 1992-07-03 RJT  ISSUED CUTOFF MOVED REQ4171                    *
      * 1992-04-23 JLM  PAID ROUNDING - HALF UP PRB2437                *
      * 1994-09-05 PQR  PERFORMANCE - VOID READ AHEAD REQ3692          *
      * 1994-02-20 DPS  NEW VOID REPORT COLUMNS PRB5548                *
      * 1998-07-12 LMB  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-06-15 MKL  VOID FEE SCHEDULE UPDATE REQ7763               *
      * 2000-07-21 RJT  ISSUED CUTOFF MOVED PRB3658                    *
      * 2001-05-09 TWH  NEW PAID REPORT COLUMNS INC9961                *
      * 2001-03-04 JLM  EXCEPT ROUNDING - HALF UP INC3243              *
      * 2001-02-19 MKL  ISSUED CUTOFF MOVED CHG7891                    *
      * 2002-07-15 LMB  PAID RERUN PROTECTION INC1279                  *
      * 2003-01-16 DPS  NEW PAID REPORT COLUMNS INC3614                *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PPYAUDT.
       AUTHOR. C R PATEL.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "PPYMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS PPYM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "PPYTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "PPYOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "PPYAURPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY PPYMST.
       FD  TRAN-FILE.
       COPY PPYTRN.
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
                                           VALUE 11.000000.
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
      * WS-ISSUED-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-ISSUED-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000245500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000602500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005100000000213000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005600000000287500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005850000000749500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015850000000579500'.
       01  WS-ISSUED-TIER-TBL REDEFINES WS-ISSUED-TIER-VALUES.
           05  WS-ISSUED-TIER-ROW OCCURS 6 TIMES.
               10  WS-ISSUED-TIER-LOW      PIC 9(09)V99.
               10  WS-ISSUED-TIER-RATE     PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-EXCEPT-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-EXCEPT-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000221500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000858500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002600000000322500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005100000000748000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005200000000467000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010200000000399000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012700000000875500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012950000000733000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013200000000515000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013300000000180000'.
       01  WS-EXCEPT-BAND-TBL REDEFINES WS-EXCEPT-BAND-VALUES.
           05  WS-EXCEPT-BAND-ROW OCCURS 10 TIMES.
               10  WS-EXCEPT-BAND-LOW      PIC 9(09)V99.
               10  WS-EXCEPT-BAND-RATE     PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-ISSUED-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-ISSUED-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000483000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000048500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000750000000476000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005750000000162500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008250000000660000'.
       01  WS-ISSUED-GRID-TBL REDEFINES WS-ISSUED-GRID-VALUES.
           05  WS-ISSUED-GRID-ROW OCCURS 5 TIMES.
               10  WS-ISSUED-GRID-LOW      PIC 9(09)V99.
               10  WS-ISSUED-GRID-RATE     PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-STALE-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-STALE-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000677500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000420000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002600000000762000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002650000000306500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002900000000633500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005400000000552000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010400000000891000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010500000000720000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010600000000783000'.
       01  WS-STALE-SCHED-TBL REDEFINES WS-STALE-SCHED-VALUES.
           05  WS-STALE-SCHED-ROW OCCURS 9 TIMES.
               10  WS-STALE-SCHED-LOW      PIC 9(09)V99.
               10  WS-STALE-SCHED-RATE     PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  PPYAUDT  POSITIVE PAY'.
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
       COPY PPELGP.

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
               MOVE 'PPYMST' TO ABND-FILE-NAME
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
           MOVE PPYT-KEY TO PPYM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-SELECT-DISCOUNT
               PERFORM 3010-CHECK-NEXT-REVIEW
               PERFORM 3020-CHECK-LAST-ACT
               PERFORM 3030-SELECT-MARGIN
               PERFORM 3040-SELECT-RATE
               PERFORM 3050-SELECT-MARGIN
               PERFORM 3060-ACCRUE-STALE
               PERFORM 3070-SELECT-MARGIN
               PERFORM 3080-COUNT-PAYMENT
               PERFORM 3090-PRORATE-VOID
               PERFORM 3100-CHECK-EFFECTIVE
               PERFORM 3110-SET-TRAN
               PERFORM 3120-COUNT-CYCLE
               PERFORM 3130-ACCRUE-PAID
               PERFORM 3140-CHECK-NEXT-DUE
               PERFORM 3150-LIMIT-EXCEPT
               PERFORM 3160-COUNT-CYCLE
               PERFORM 3170-SELECT-RATE
               PERFORM 3180-ACCUM-STATUS
               PERFORM 3190-EDIT-CONTACT
               PERFORM 3200-SPLIT-ISSUED
               PERFORM 3210-SPLIT-VOID
               PERFORM 3220-ACCRUE-VOID
               PERFORM 3230-PRORATE-EXCEPT
               PERFORM 3240-SET-TRAN
               PERFORM 3250-ACCRUE-ISSUED
               PERFORM 3260-SET-MATCH-RULE
               PERFORM 3270-PRORATE-EXCEPT
               PERFORM 3280-ACCUM-REGION
               PERFORM 3290-FLAG-PAID
               PERFORM 3300-ACCRUE-STALE
               PERFORM 3310-ASSESS-AUTO-RENEW
               PERFORM 3320-FLAG-VOID
               PERFORM 3330-DESCRIBE-CHANNEL
               PERFORM 3340-SPLIT-STALE
               PERFORM 3350-SELECT-DISCOUNT
               PERFORM 3360-LIMIT-STALE
               PERFORM 3370-ASSESS-NOTICE-SENT
               PERFORM 3380-LIMIT-STALE
               PERFORM 3390-FLAG-PAID
               PERFORM 3400-ACCUM-REASON
               PERFORM 3410-SET-CHANNEL
               PERFORM 3420-FLAG-PAID
               PERFORM 3430-ASSESS-HOLD
               PERFORM 3440-LIMIT-PAID
               PERFORM 3450-AGE-EXPIRY
               PERFORM 3460-SPLIT-ISSUED
               PERFORM 3470-PRORATE-PAID
               PERFORM 3480-PRORATE-ISSUED
               PERFORM 3490-AGE-NEXT-DUE
               PERFORM 3500-FLAG-STALE
               PERFORM 3510-PRORATE-PAID
               PERFORM 3520-SPLIT-PAID
               PERFORM 3530-SET-DECISION
               PERFORM 3540-ACCUM-REASON
               PERFORM 3550-COUNT-NOTICE
               PERFORM 3560-FLAG-ISSUED
               PERFORM 3570-EDIT-REJECT
               PERFORM 3580-ACCUM-CYCLE
               PERFORM 3590-DESCRIBE-TRAN
               PERFORM 3600-DESCRIBE-TRAN
               PERFORM 3610-SPLIT-STALE
               PERFORM 3620-LIMIT-ITEM
               PERFORM 3630-LIMIT-STALE
               PERFORM 3640-ASSESS-HOLD
               PERFORM 3650-SPLIT-STALE
               PERFORM 3660-AGE-NEXT-DUE
               PERFORM 3670-LIMIT-STALE
               PERFORM 3680-EDIT-REF-NO
               PERFORM 3690-ASSESS-HOLD
               PERFORM 3700-CHECK-EXPIRY
               PERFORM 3710-ACCRUE-VOID
               PERFORM 3720-ACCRUE-ISSUED
               PERFORM 3730-LIMIT-STALE
               PERFORM 3740-ACCUM-DECISION
               PERFORM 3750-ACCRUE-PAID
               PERFORM 3760-EDIT-REF-NO
               PERFORM 3770-LIMIT-PAID
               PERFORM 3780-SPLIT-VOID
               PERFORM 3790-ACCRUE-PAID
               PERFORM 3800-CHECK-POST
               PERFORM 3810-ACCUM-CYCLE
               PERFORM 3820-EDIT-CUST-ID
               PERFORM 3830-AGE-OPEN
               PERFORM 3840-AGE-NEXT-REVIEW
               PERFORM 3850-AGE-NEXT-REVIEW
               PERFORM 3860-CHECK-LAST-PAY
               PERFORM 3870-SPLIT-EXCEPT
               PERFORM 3880-DESCRIBE-DECISION
               PERFORM 3890-SPLIT-VOID
               PERFORM 3900-AGE-LAST-MAINT
               PERFORM 3910-DESCRIBE-CYCLE
               PERFORM 3920-SET-CYCLE
               PERFORM 3930-SELECT-MARGIN
               PERFORM 3940-COUNT-EXCEPT
               PERFORM 3950-CHECK-EFFECTIVE
               PERFORM 3960-ACCRUE-STALE
               PERFORM 3970-CHECK-EFF
               PERFORM 3980-LIMIT-ISSUED
               PERFORM 3990-EDIT-DESC
               PERFORM 4000-AGE-LAST-PAY
               PERFORM 4010-SET-STATUS
               PERFORM 4020-ASSESS-REVERSAL
               PERFORM 4030-ACCUM-REGION
               PERFORM 4040-EDIT-OPERATOR
               PERFORM 4050-FLAG-EXCEPT
               PERFORM 4060-COUNT-EXCEPT
               PERFORM 4070-AGE-EFF
               PERFORM 4080-LIMIT-STALE
               PERFORM 4090-COUNT-PAYMENT
               PERFORM 4100-FLAG-ITEM
               PERFORM 4110-ASSESS-HOLD
               PERFORM 4120-ACCRUE-ISSUED
               PERFORM 4130-ASSESS-HOLD
               PERFORM 4140-ACCUM-CHANNEL
               PERFORM 4150-SET-MATCH-RULE
               PERFORM 4160-ACCRUE-ISSUED
               PERFORM 4170-EDIT-REF-NO
               PERFORM 4180-CHECK-NEXT-REVIEW
               PERFORM 4190-PRORATE-ISSUED
               PERFORM 4200-LIMIT-PAID
               PERFORM 4210-LIMIT-VOID
               PERFORM 4220-ACCRUE-PAID
               PERFORM 4230-EDIT-DESC
               PERFORM 4240-ASSESS-STOP
               PERFORM 4250-CHECK-OPEN
               PERFORM 4260-SELECT-RATE
               PERFORM 4270-ASSESS-NOTICE-SENT
               PERFORM 4280-SPLIT-STALE
               PERFORM 4290-COUNT-SEQ
               PERFORM 4300-SELECT-MARGIN
               PERFORM 4310-SPLIT-PAID
               PERFORM 4320-ASSESS-STOP
               PERFORM 4330-ASSESS-REVERSAL
               PERFORM 4340-PRORATE-STALE
               PERFORM 4350-COUNT-SEQ
               PERFORM 4360-SELECT-MARGIN
               PERFORM 4370-CHECK-NEXT-DUE
               PERFORM 4380-SELECT-MARGIN
               PERFORM 4390-SELECT-DISCOUNT
               PERFORM 4400-DESCRIBE-CHANNEL
               PERFORM 4410-FLAG-STALE
               PERFORM 4420-AGE-CLOSE
               PERFORM 7020-CALL-PPELG
               REWRITE PPY-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'PPYMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM PPY-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * SELECT THE ISSUED TIER TIER FOR THE ISSUED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3000-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-ISSUED-TIER-RATE(1) TO PPYM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF PPYM-ISSUED-YTD NOT < WS-ISSUED-TIER-LOW(WS-TX)
                   MOVE WS-ISSUED-TIER-RATE(WS-TX) TO PPYM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPYM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPYM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 15 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3010-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF PPYM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPYM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO PPYM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 45 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3020-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF PPYM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPYM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO PPYM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE EXCEPT BAND TIER FOR THE STALE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3030-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-EXCEPT-BAND-RATE(1) TO PPYM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF PPYM-STALE-YTD NOT < WS-EXCEPT-BAND-LOW(WS-TX)
                   MOVE WS-EXCEPT-BAND-RATE(WS-TX) TO PPYM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPYM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPYM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE EXCEPT BAND TIER FOR THE STALE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3040-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-EXCEPT-BAND-RATE(1) TO PPYM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF PPYM-STALE-YTD NOT < WS-EXCEPT-BAND-LOW(WS-TX)
                   MOVE WS-EXCEPT-BAND-RATE(WS-TX) TO PPYM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPYM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPYM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ISSUED GRID TIER FOR THE EXCEPT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3050-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-ISSUED-GRID-RATE(1) TO PPYM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF PPYM-EXCEPT-YTD NOT < WS-ISSUED-GRID-LOW(WS-TX)
                   MOVE WS-ISSUED-GRID-RATE(WS-TX) TO PPYM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPYM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPYM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STALE YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3060-ACCRUE-STALE.
           IF PPYM-STALE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPYM-STALE-YTD * PPYM-RATE / 100 / 360
               ADD WS-WORK-AMT TO PPYT-VOID-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE EXCEPT BAND TIER FOR THE EXCEPT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3070-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-EXCEPT-BAND-RATE(1) TO PPYM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF PPYM-EXCEPT-AMT NOT < WS-EXCEPT-BAND-LOW(WS-TX)
                   MOVE WS-EXCEPT-BAND-RATE(WS-TX) TO PPYM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPYM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPYM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3080-COUNT-PAYMENT.
           IF PPYM-PAYMENT-CNT < 9999999
               ADD 1 TO PPYM-PAYMENT-CNT
           END-IF
           IF PPYM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO PPYM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE VOID AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO VOID YTD.
      *----------------------------------------------------------------*
       3090-PRORATE-VOID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPYM-VOID-YTD ROUNDED =
                   PPYT-VOID-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PPYM-VOID-YTD
           END-IF
           ADD PPYM-VOID-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3100-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF PPYM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPYM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO PPYM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * IS -> PD WHEN NEXT DUE DATE IS PAST
      * PD -> EX WHEN NEXT DUE DATE IS PAST
      * EX -> VD WHEN NEXT DUE DATE IS PAST
      * VD -> PR WHEN NEXT DUE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3110-SET-TRAN.
           EVALUATE PPYT-TRAN-CD
               WHEN 'IS'
                   IF PPYM-NEXT-DUE-DATE > 0 AND PPYM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PD' TO PPYT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PD'
                   IF PPYM-NEXT-DUE-DATE > 0 AND PPYM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EX' TO PPYT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'EX'
                   IF PPYM-NEXT-DUE-DATE > 0 AND PPYM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VD' TO PPYT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VD'
                   IF PPYM-NEXT-DUE-DATE > 0 AND PPYM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PR' TO PPYT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3120-COUNT-CYCLE.
           IF PPYM-CYCLE-CNT < 9999999
               ADD 1 TO PPYM-CYCLE-CNT
           END-IF
           IF PPYM-CYCLE-CNT NOT < 10
               MOVE 'Y' TO PPYM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAID AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3130-ACCRUE-PAID.
           IF PPYT-PAID-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPYT-PAID-AMT * PPYM-RATE / 100 / 360
               ADD WS-WORK-AMT TO PPYT-VOID-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3140-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF PPYM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPYM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO PPYM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT AMT IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3150-LIMIT-EXCEPT.
           IF PPYM-EXCEPT-AMT < 25.00
               MOVE 25.00 TO PPYM-EXCEPT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PPYM-EXCEPT-AMT > 50000.00
               MOVE 50000.00 TO PPYM-EXCEPT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3160-COUNT-CYCLE.
           IF PPYM-CYCLE-CNT < 9999999
               ADD 1 TO PPYM-CYCLE-CNT
           END-IF
           IF PPYM-CYCLE-CNT NOT < 10
               MOVE 'Y' TO PPYM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE EXCEPT BAND TIER FOR THE VOID AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3170-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-EXCEPT-BAND-RATE(1) TO PPYM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF PPYT-VOID-AMT NOT < WS-EXCEPT-BAND-LOW(WS-TX)
                   MOVE WS-EXCEPT-BAND-RATE(WS-TX) TO PPYM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPYM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPYM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE EXCEPT AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3180-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPYM-STATUS-CD
                   ADD PPYM-EXCEPT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPYM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PPYM-EXCEPT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E254 CONTACT REQUIRED
      * E989 STALE AMT NEGATIVE
      * E944 STALE AMT OVER 10000
      * E179 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3190-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPYM-CONTACT = SPACES
                   MOVE 'E254' TO WS-REJECT-CODE
               WHEN PPYT-STALE-AMT < 0
                   MOVE 'E989' TO WS-REJECT-CODE
               WHEN PPYT-STALE-AMT > 10000.00
                   MOVE 'E944' TO WS-REJECT-CODE
               WHEN PPYM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E179' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ISSUED AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3200-SPLIT-ISSUED.
           COMPUTE WS-WORK-AMT = PPYT-ISSUED-AMT / 2
           COMPUTE WS-WORK-AMT-2 = PPYT-ISSUED-AMT - (WS-WORK-AMT * 2)
           COMPUTE PPYM-PAID-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT VOID AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3210-SPLIT-VOID.
           COMPUTE WS-WORK-AMT = PPYM-VOID-AMT / 12
           COMPUTE WS-WORK-AMT-2 = PPYM-VOID-AMT - (WS-WORK-AMT * 12)
           COMPUTE PPYM-PAID-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VOID AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3220-ACCRUE-VOID.
           IF PPYT-VOID-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPYT-VOID-AMT * PPYM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO PPYM-PAID-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE VOID YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO EXCEPT AMT.
      *----------------------------------------------------------------*
       3230-PRORATE-EXCEPT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPYM-EXCEPT-AMT ROUNDED =
                   PPYM-VOID-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PPYM-EXCEPT-AMT
           END-IF
           ADD PPYM-EXCEPT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * IS -> PD WHEN CLOSE DATE IS PAST
      * PD -> EX WHEN CLOSE DATE IS PAST
      * EX -> VD WHEN CLOSE DATE IS PAST
      * VD -> PR WHEN CLOSE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3240-SET-TRAN.
           EVALUATE PPYT-TRAN-CD
               WHEN 'IS'
                   IF PPYM-CLOSE-DATE > 0 AND PPYM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PD' TO PPYT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PD'
                   IF PPYM-CLOSE-DATE > 0 AND PPYM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EX' TO PPYT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'EX'
                   IF PPYM-CLOSE-DATE > 0 AND PPYM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VD' TO PPYT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VD'
                   IF PPYM-CLOSE-DATE > 0 AND PPYM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PR' TO PPYT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ISSUED AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3250-ACCRUE-ISSUED.
           IF PPYT-ISSUED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPYT-ISSUED-AMT * PPYM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR MATCH RULE CD:
      * A -> B WHEN POST DATE IS PAST
      * B -> F WHEN POST DATE IS PAST
      * F -> M WHEN POST DATE IS PAST
      * M -> S WHEN POST DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3260-SET-MATCH-RULE.
           EVALUATE PPYM-MATCH-RULE-CD
               WHEN 'A'
                   IF PPYT-POST-DATE > 0 AND PPYT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'B' TO PPYM-MATCH-RULE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'B'
                   IF PPYT-POST-DATE > 0 AND PPYT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'F' TO PPYM-MATCH-RULE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'F'
                   IF PPYT-POST-DATE > 0 AND PPYT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO PPYM-MATCH-RULE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF PPYT-POST-DATE > 0 AND PPYT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO PPYM-MATCH-RULE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE EXCEPT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3270-PRORATE-EXCEPT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPYM-EXCEPT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3280-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPYM-REGION-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPYM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PAID AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3290-FLAG-PAID.
           IF PPYM-PAID-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PPYM-PAID-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STALE AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3300-ACCRUE-STALE.
           IF PPYM-STALE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPYM-STALE-AMT * PPYM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ISSUED FEE ON THE STALE AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3310-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN PPYM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO PPYM-ISSUED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYM-EXEMPT-FLAG = 'Y'
                   COMPUTE PPYM-ISSUED-AMT ROUNDED = PPYT-STALE-AMT *
                       PPYM-MARGIN / 200
               WHEN PPYM-CYCLE-CD = 'A'
                   MOVE 5.00 TO PPYM-ISSUED-AMT
               WHEN PPYM-CYCLE-CD = 'B'
                   MOVE 3.00 TO PPYM-ISSUED-AMT
               WHEN OTHER
                   COMPUTE PPYM-ISSUED-AMT ROUNDED = PPYT-STALE-AMT *
                       PPYM-MARGIN / 100
           END-EVALUATE
           IF PPYM-ISSUED-AMT > 0 AND PPYM-ISSUED-AMT < 2.00
               MOVE 2.00 TO PPYM-ISSUED-AMT
           END-IF
           IF PPYM-ISSUED-AMT > 75.00
               MOVE 75.00 TO PPYM-ISSUED-AMT
           END-IF
           ADD PPYM-ISSUED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * VOID YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3320-FLAG-VOID.
           IF PPYM-VOID-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PPYM-VOID-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3330-DESCRIBE-CHANNEL.
           EVALUATE PPYT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3340-SPLIT-STALE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 3)
           COMPUTE PPYM-STALE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE EXCEPT BAND TIER FOR THE ISSUED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3350-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-EXCEPT-BAND-RATE(1) TO PPYM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF PPYM-ISSUED-AMT NOT < WS-EXCEPT-BAND-LOW(WS-TX)
                   MOVE WS-EXCEPT-BAND-RATE(WS-TX) TO PPYM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPYM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPYM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STALE AMT IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3360-LIMIT-STALE.
           IF PPYM-STALE-AMT < 25.00
               MOVE 25.00 TO PPYM-STALE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PPYM-STALE-AMT > 2500.00
               MOVE 2500.00 TO PPYM-STALE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE VOID FEE ON THE PAID AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3370-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN PPYM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO PPYT-VOID-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYM-HOLD-FLAG = 'Y'
                   COMPUTE PPYT-VOID-AMT ROUNDED = PPYM-PAID-AMT *
                       PPYM-RATE / 200
               WHEN PPYM-REASON-CD = 'B'
                   MOVE 7.00 TO PPYT-VOID-AMT
               WHEN PPYM-REASON-CD = 'M'
                   MOVE 10.00 TO PPYT-VOID-AMT
               WHEN OTHER
                   COMPUTE PPYT-VOID-AMT ROUNDED = PPYM-PAID-AMT *
                       PPYM-RATE / 100
           END-EVALUATE
           IF PPYT-VOID-AMT > 0 AND PPYT-VOID-AMT < 5.00
               MOVE 5.00 TO PPYT-VOID-AMT
           END-IF
           IF PPYT-VOID-AMT > 25.00
               MOVE 25.00 TO PPYT-VOID-AMT
           END-IF
           ADD PPYT-VOID-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STALE AMT IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3380-LIMIT-STALE.
           IF PPYT-STALE-AMT < 0.00
               MOVE 0.00 TO PPYT-STALE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PPYT-STALE-AMT > 50000.00
               MOVE 50000.00 TO PPYT-STALE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAID YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3390-FLAG-PAID.
           IF PPYM-PAID-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PPYM-PAID-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3400-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPYM-REASON-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPYM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN NEXT DUE DATE IS PAST
      * BAT -> MAL WHEN NEXT DUE DATE IS PAST
      * MAL -> TEL WHEN NEXT DUE DATE IS PAST
      * TEL -> ONL WHEN NEXT DUE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3410-SET-CHANNEL.
           EVALUATE PPYT-CHANNEL-CD
               WHEN 'BRN'
                   IF PPYM-NEXT-DUE-DATE > 0 AND PPYM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO PPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF PPYM-NEXT-DUE-DATE > 0 AND PPYM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO PPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF PPYM-NEXT-DUE-DATE > 0 AND PPYM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO PPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF PPYM-NEXT-DUE-DATE > 0 AND PPYM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO PPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAID AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3420-FLAG-PAID.
           IF PPYT-PAID-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PPYT-PAID-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE EXCEPT FEE ON THE STALE YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3430-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN PPYM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO PPYM-EXCEPT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYM-EXEMPT-FLAG = 'Y'
                   COMPUTE PPYM-EXCEPT-AMT ROUNDED = PPYM-STALE-YTD *
                       PPYM-DISCOUNT / 200
               WHEN PPYM-DECISION-CD = 'AND'
                   MOVE 7.00 TO PPYM-EXCEPT-AMT
               WHEN PPYM-DECISION-CD = 'BER'
                   MOVE 15.00 TO PPYM-EXCEPT-AMT
               WHEN OTHER
                   COMPUTE PPYM-EXCEPT-AMT ROUNDED = PPYM-STALE-YTD *
                       PPYM-DISCOUNT / 100
           END-EVALUATE
           IF PPYM-EXCEPT-AMT > 0 AND PPYM-EXCEPT-AMT < 1.00
               MOVE 1.00 TO PPYM-EXCEPT-AMT
           END-IF
           IF PPYM-EXCEPT-AMT > 50.00
               MOVE 50.00 TO PPYM-EXCEPT-AMT
           END-IF
           ADD PPYM-EXCEPT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PAID AMT IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3440-LIMIT-PAID.
           IF PPYT-PAID-AMT < 50.00
               MOVE 50.00 TO PPYT-PAID-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PPYT-PAID-AMT > 5000.00
               MOVE 5000.00 TO PPYT-PAID-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3450-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF PPYM-EXPIRY-DATE > 0 AND PPYM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPYM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PPYM-EXCEPT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PPYM-EXCEPT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD PPYM-EXCEPT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PPYM-EXCEPT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPYM-EXCEPT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT ISSUED AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3460-SPLIT-ISSUED.
           COMPUTE WS-WORK-AMT = PPYM-ISSUED-AMT / 3
           COMPUTE WS-WORK-AMT-2 = PPYM-ISSUED-AMT - (WS-WORK-AMT * 3)
           COMPUTE PPYM-EXCEPT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE EXCEPT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAID YTD.
      *----------------------------------------------------------------*
       3470-PRORATE-PAID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPYM-PAID-YTD ROUNDED =
                   PPYM-EXCEPT-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PPYM-PAID-YTD
           END-IF
           ADD PPYM-PAID-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE STALE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ISSUED YTD.
      *----------------------------------------------------------------*
       3480-PRORATE-ISSUED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPYM-ISSUED-YTD ROUNDED =
                   PPYT-STALE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PPYM-ISSUED-YTD
           END-IF
           ADD PPYM-ISSUED-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3490-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF PPYM-NEXT-DUE-DATE > 0 AND PPYM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPYM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PPYM-STALE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD PPYM-STALE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD PPYM-STALE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PPYM-STALE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPYM-STALE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STALE AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3500-FLAG-STALE.
           IF PPYM-STALE-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PPYM-STALE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE STALE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PAID AMT.
      *----------------------------------------------------------------*
       3510-PRORATE-PAID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPYM-PAID-AMT ROUNDED =
                   PPYM-STALE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PPYM-PAID-AMT
           END-IF
           ADD PPYM-PAID-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT PAID AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3520-SPLIT-PAID.
           COMPUTE WS-WORK-AMT = PPYM-PAID-AMT / 3
           COMPUTE WS-WORK-AMT-2 = PPYM-PAID-AMT - (WS-WORK-AMT * 3)
           COMPUTE PPYM-STALE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DECISION CD:
      * AND -> BER WHEN LAST ACT DATE IS PAST
      * BER -> CZW WHEN LAST ACT DATE IS PAST
      * CZW -> SBV WHEN LAST ACT DATE IS PAST
      * SBV -> ZNG WHEN LAST ACT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3530-SET-DECISION.
           EVALUATE PPYM-DECISION-CD
               WHEN 'AND'
                   IF PPYM-LAST-ACT-DATE > 0 AND PPYM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BER' TO PPYM-DECISION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BER'
                   IF PPYM-LAST-ACT-DATE > 0 AND PPYM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CZW' TO PPYM-DECISION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CZW'
                   IF PPYM-LAST-ACT-DATE > 0 AND PPYM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SBV' TO PPYM-DECISION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SBV'
                   IF PPYM-LAST-ACT-DATE > 0 AND PPYM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZNG' TO PPYM-DECISION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE EXCEPT AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3540-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPYM-REASON-CD
                   ADD PPYM-EXCEPT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPYM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PPYM-EXCEPT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3550-COUNT-NOTICE.
           IF PPYM-NOTICE-CNT < 9999999
               ADD 1 TO PPYM-NOTICE-CNT
           END-IF
           IF PPYM-NOTICE-CNT NOT < 3
               MOVE 'Y' TO PPYM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ISSUED AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3560-FLAG-ISSUED.
           IF PPYT-ISSUED-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PPYT-ISSUED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E379 REJECT CD REQUIRED
      * E170 VOID AMT NEGATIVE
      * E803 VOID AMT OVER 5000
      * E642 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3570-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPYT-REJECT-CD = SPACES
                   MOVE 'E379' TO WS-REJECT-CODE
               WHEN PPYM-VOID-AMT < 0
                   MOVE 'E170' TO WS-REJECT-CODE
               WHEN PPYM-VOID-AMT > 5000.00
                   MOVE 'E803' TO WS-REJECT-CODE
               WHEN PPYT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E642' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3580-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPYM-CYCLE-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPYM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3590-DESCRIBE-TRAN.
           EVALUATE PPYT-TRAN-CD
               WHEN 'IS'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'PD'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'EX'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'VD'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'PR'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3600-DESCRIBE-TRAN.
           EVALUATE PPYT-TRAN-CD
               WHEN 'IS'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'PD'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'EX'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'VD'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'PR'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3610-SPLIT-STALE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 3)
           COMPUTE PPYT-STALE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3620-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 10.00
               MOVE 10.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 5000.00
               MOVE 5000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STALE AMT IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3630-LIMIT-STALE.
           IF PPYM-STALE-AMT < 25.00
               MOVE 25.00 TO PPYM-STALE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PPYM-STALE-AMT > 2500.00
               MOVE 2500.00 TO PPYM-STALE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAID FEE ON THE ISSUED AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3640-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN PPYM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO PPYT-PAID-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE PPYT-PAID-AMT ROUNDED = PPYT-ISSUED-AMT *
                       PPYM-RATE / 200
               WHEN PPYM-STATUS-CD = 'DR'
                   MOVE 7.00 TO PPYT-PAID-AMT
               WHEN PPYM-STATUS-CD = 'TK'
                   MOVE 10.00 TO PPYT-PAID-AMT
               WHEN OTHER
                   COMPUTE PPYT-PAID-AMT ROUNDED = PPYT-ISSUED-AMT *
                       PPYM-RATE / 100
           END-EVALUATE
           IF PPYT-PAID-AMT > 0 AND PPYT-PAID-AMT < 10.00
               MOVE 10.00 TO PPYT-PAID-AMT
           END-IF
           IF PPYT-PAID-AMT > 35.00
               MOVE 35.00 TO PPYT-PAID-AMT
           END-IF
           ADD PPYT-PAID-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT STALE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3650-SPLIT-STALE.
           COMPUTE WS-WORK-AMT = PPYT-STALE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = PPYT-STALE-AMT - (WS-WORK-AMT * 4)
           COMPUTE PPYM-STALE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3660-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF PPYM-NEXT-DUE-DATE > 0 AND PPYM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPYM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PPYM-VOID-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD PPYM-VOID-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD PPYM-VOID-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD PPYM-VOID-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPYM-VOID-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STALE AMT IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3670-LIMIT-STALE.
           IF PPYM-STALE-AMT < 50.00
               MOVE 50.00 TO PPYM-STALE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PPYM-STALE-AMT > 500.00
               MOVE 500.00 TO PPYM-STALE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E040 REF NO REQUIRED
      * E846 EXCEPT AMT NEGATIVE
      * E511 EXCEPT AMT OVER 50000
      * E596 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3680-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPYM-REF-NO = SPACES
                   MOVE 'E040' TO WS-REJECT-CODE
               WHEN PPYM-EXCEPT-AMT < 0
                   MOVE 'E846' TO WS-REJECT-CODE
               WHEN PPYM-EXCEPT-AMT > 50000.00
                   MOVE 'E511' TO WS-REJECT-CODE
               WHEN PPYM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E596' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE EXCEPT FEE ON THE WORK AMT 2.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3690-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN PPYM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO PPYM-EXCEPT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYM-EXEMPT-FLAG = 'Y'
                   COMPUTE PPYM-EXCEPT-AMT ROUNDED = WS-WORK-AMT-2 *
                       PPYM-RATE / 200
               WHEN PPYT-TRAN-CD = 'IS'
                   MOVE 12.00 TO PPYM-EXCEPT-AMT
               WHEN PPYT-TRAN-CD = 'PD'
                   MOVE 10.00 TO PPYM-EXCEPT-AMT
               WHEN OTHER
                   COMPUTE PPYM-EXCEPT-AMT ROUNDED = WS-WORK-AMT-2 *
                       PPYM-RATE / 100
           END-EVALUATE
           IF PPYM-EXCEPT-AMT > 0 AND PPYM-EXCEPT-AMT < 5.00
               MOVE 5.00 TO PPYM-EXCEPT-AMT
           END-IF
           IF PPYM-EXCEPT-AMT > 100.00
               MOVE 100.00 TO PPYM-EXCEPT-AMT
           END-IF
           ADD PPYM-EXCEPT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 365 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3700-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF PPYM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPYM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO PPYM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VOID AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3710-ACCRUE-VOID.
           IF PPYM-VOID-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPYM-VOID-AMT * PPYM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO PPYM-ISSUED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ISSUED AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3720-ACCRUE-ISSUED.
           IF PPYM-ISSUED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPYM-ISSUED-AMT * PPYM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STALE AMT IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3730-LIMIT-STALE.
           IF PPYM-STALE-AMT < 50.00
               MOVE 50.00 TO PPYM-STALE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PPYM-STALE-AMT > 500.00
               MOVE 500.00 TO PPYM-STALE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ISSUED YTD BY DECISION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3740-ACCUM-DECISION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPYM-DECISION-CD
                   ADD PPYM-ISSUED-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPYM-DECISION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PPYM-ISSUED-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAID YTD AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3750-ACCRUE-PAID.
           IF PPYM-PAID-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPYM-PAID-YTD * PPYM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO PPYM-ISSUED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E471 REF NO REQUIRED
      * E058 EXCEPT YTD NEGATIVE
      * E167 EXCEPT YTD OVER 1000000
      * E451 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3760-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPYM-REF-NO = SPACES
                   MOVE 'E471' TO WS-REJECT-CODE
               WHEN PPYM-EXCEPT-YTD < 0
                   MOVE 'E058' TO WS-REJECT-CODE
               WHEN PPYM-EXCEPT-YTD > 1000000.00
                   MOVE 'E167' TO WS-REJECT-CODE
               WHEN PPYT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E451' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAID YTD IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3770-LIMIT-PAID.
           IF PPYM-PAID-YTD < 100.00
               MOVE 100.00 TO PPYM-PAID-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PPYM-PAID-YTD > 1000.00
               MOVE 1000.00 TO PPYM-PAID-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT VOID AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3780-SPLIT-VOID.
           COMPUTE WS-WORK-AMT = PPYT-VOID-AMT / 12
           COMPUTE WS-WORK-AMT-2 = PPYT-VOID-AMT - (WS-WORK-AMT * 12)
           COMPUTE PPYM-EXCEPT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAID AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3790-ACCRUE-PAID.
           IF PPYT-PAID-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPYT-PAID-AMT * PPYM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO PPYM-VOID-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 45 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3800-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF PPYT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPYT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO PPYT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE VOID YTD BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3810-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPYM-CYCLE-CD
                   ADD PPYM-VOID-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPYM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PPYM-VOID-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E523 CUST ID REQUIRED
      * E650 PAID AMT NEGATIVE
      * E105 PAID AMT OVER 5000
      * E534 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3820-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPYM-CUST-ID = SPACES
                   MOVE 'E523' TO WS-REJECT-CODE
               WHEN PPYM-PAID-AMT < 0
                   MOVE 'E650' TO WS-REJECT-CODE
               WHEN PPYM-PAID-AMT > 5000.00
                   MOVE 'E105' TO WS-REJECT-CODE
               WHEN PPYM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E534' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3830-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF PPYM-OPEN-DATE > 0 AND PPYM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPYM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PPYM-ISSUED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD PPYM-ISSUED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD PPYM-ISSUED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PPYM-ISSUED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPYM-ISSUED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3840-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF PPYM-NEXT-REVIEW-DATE > 0 AND PPYM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPYM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PPYM-EXCEPT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD PPYM-EXCEPT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD PPYM-EXCEPT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PPYM-EXCEPT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPYM-EXCEPT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3850-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF PPYM-NEXT-REVIEW-DATE > 0 AND PPYM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPYM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PPYT-ISSUED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PPYT-ISSUED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD PPYT-ISSUED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PPYT-ISSUED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPYT-ISSUED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 10 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3860-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF PPYM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPYM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO PPYM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT EXCEPT AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3870-SPLIT-EXCEPT.
           COMPUTE WS-WORK-AMT = PPYM-EXCEPT-AMT / 4
           COMPUTE WS-WORK-AMT-2 = PPYM-EXCEPT-AMT - (WS-WORK-AMT * 4)
           COMPUTE PPYM-PAID-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE DECISION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3880-DESCRIBE-DECISION.
           EVALUATE PPYM-DECISION-CD
               WHEN 'AND'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'BER'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'CZW'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'SBV'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'ZNG'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'ZPS'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT VOID AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3890-SPLIT-VOID.
           COMPUTE WS-WORK-AMT = PPYM-VOID-AMT / 2
           COMPUTE WS-WORK-AMT-2 = PPYM-VOID-AMT - (WS-WORK-AMT * 2)
           COMPUTE PPYM-STALE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3900-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF PPYM-LAST-MAINT-DATE > 0 AND PPYM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPYM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PPYM-EXCEPT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD PPYM-EXCEPT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD PPYM-EXCEPT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD PPYM-EXCEPT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPYM-EXCEPT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3910-DESCRIBE-CYCLE.
           EVALUATE PPYM-CYCLE-CD
               WHEN 'A'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'B'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'C'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'R'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'T'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'Y'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * A -> B WHEN LAST ACT DATE IS PAST
      * B -> C WHEN LAST ACT DATE IS PAST
      * C -> R WHEN LAST ACT DATE IS PAST
      * R -> T WHEN LAST ACT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3920-SET-CYCLE.
           EVALUATE PPYM-CYCLE-CD
               WHEN 'A'
                   IF PPYM-LAST-ACT-DATE > 0 AND PPYM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'B' TO PPYM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'B'
                   IF PPYM-LAST-ACT-DATE > 0 AND PPYM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'C' TO PPYM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'C'
                   IF PPYM-LAST-ACT-DATE > 0 AND PPYM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'R' TO PPYM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF PPYM-LAST-ACT-DATE > 0 AND PPYM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO PPYM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE STALE SCHED TIER FOR THE VOID AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3930-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-STALE-SCHED-RATE(1) TO PPYM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF PPYM-VOID-AMT NOT < WS-STALE-SCHED-LOW(WS-TX)
                   MOVE WS-STALE-SCHED-RATE(WS-TX) TO PPYM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPYM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPYM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3940-COUNT-EXCEPT.
           IF PPYM-EXCEPT-CNT < 9999999
               ADD 1 TO PPYM-EXCEPT-CNT
           END-IF
           IF PPYM-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO PPYM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3950-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF PPYM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPYM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO PPYM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STALE YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3960-ACCRUE-STALE.
           IF PPYM-STALE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPYM-STALE-YTD * PPYM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO PPYM-VOID-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 30 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3970-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF PPYT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPYT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO PPYM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ISSUED YTD IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3980-LIMIT-ISSUED.
           IF PPYM-ISSUED-YTD < 25.00
               MOVE 25.00 TO PPYM-ISSUED-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PPYM-ISSUED-YTD > 5000.00
               MOVE 5000.00 TO PPYM-ISSUED-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E739 DESC REQUIRED
      * E769 STALE AMT NEGATIVE
      * E606 STALE AMT OVER 25000
      * E532 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3990-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPYM-DESC = SPACES
                   MOVE 'E739' TO WS-REJECT-CODE
               WHEN PPYM-STALE-AMT < 0
                   MOVE 'E769' TO WS-REJECT-CODE
               WHEN PPYM-STALE-AMT > 25000.00
                   MOVE 'E606' TO WS-REJECT-CODE
               WHEN PPYM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E532' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       4000-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF PPYM-LAST-PAY-DATE > 0 AND PPYM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPYM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PPYM-PAID-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD PPYM-PAID-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD PPYM-PAID-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD PPYM-PAID-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPYM-PAID-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * DR -> TK WHEN EXPIRY DATE IS PAST
      * TK -> WX WHEN EXPIRY DATE IS PAST
      * WX -> DR WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4010-SET-STATUS.
           EVALUATE PPYM-STATUS-CD
               WHEN 'DR'
                   IF PPYM-EXPIRY-DATE > 0 AND PPYM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TK' TO PPYM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TK'
                   IF PPYM-EXPIRY-DATE > 0 AND PPYM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WX' TO PPYM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WX'
                   IF PPYM-EXPIRY-DATE > 0 AND PPYM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DR' TO PPYM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE ISSUED YTD FEE ON THE PAID AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4020-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN PPYT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO PPYM-ISSUED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE PPYM-ISSUED-YTD ROUNDED = PPYT-PAID-AMT *
                       PPYM-DISCOUNT / 200
               WHEN PPYM-REGION-CD = 'A'
                   MOVE 3.00 TO PPYM-ISSUED-YTD
               WHEN PPYM-REGION-CD = 'B'
                   MOVE 5.00 TO PPYM-ISSUED-YTD
               WHEN OTHER
                   COMPUTE PPYM-ISSUED-YTD ROUNDED = PPYT-PAID-AMT *
                       PPYM-DISCOUNT / 100
           END-EVALUATE
           IF PPYM-ISSUED-YTD > 0 AND PPYM-ISSUED-YTD < 2.00
               MOVE 2.00 TO PPYM-ISSUED-YTD
           END-IF
           IF PPYM-ISSUED-YTD > 25.00
               MOVE 25.00 TO PPYM-ISSUED-YTD
           END-IF
           ADD PPYM-ISSUED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE ISSUED YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4030-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPYM-REGION-CD
                   ADD PPYM-ISSUED-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPYM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PPYM-ISSUED-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E282 OPERATOR REQUIRED
      * E399 ISSUED AMT NEGATIVE
      * E678 ISSUED AMT OVER 10000
      * E231 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       4040-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPYT-OPERATOR = SPACES
                   MOVE 'E282' TO WS-REJECT-CODE
               WHEN PPYM-ISSUED-AMT < 0
                   MOVE 'E399' TO WS-REJECT-CODE
               WHEN PPYM-ISSUED-AMT > 10000.00
                   MOVE 'E678' TO WS-REJECT-CODE
               WHEN PPYM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E231' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4050-FLAG-EXCEPT.
           IF PPYM-EXCEPT-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PPYM-EXCEPT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       4060-COUNT-EXCEPT.
           IF PPYM-EXCEPT-CNT < 9999999
               ADD 1 TO PPYM-EXCEPT-CNT
           END-IF
           IF PPYM-EXCEPT-CNT NOT < 12
               MOVE 'Y' TO PPYM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-90, 91-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4070-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF PPYT-EFF-DATE > 0 AND PPYT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPYT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PPYM-STALE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD PPYM-STALE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD PPYM-STALE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PPYM-STALE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPYM-STALE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STALE AMT IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4080-LIMIT-STALE.
           IF PPYM-STALE-AMT < 25.00
               MOVE 25.00 TO PPYM-STALE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PPYM-STALE-AMT > 5000.00
               MOVE 5000.00 TO PPYM-STALE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       4090-COUNT-PAYMENT.
           IF PPYM-PAYMENT-CNT < 9999999
               ADD 1 TO PPYM-PAYMENT-CNT
           END-IF
           IF PPYM-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO PPYM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4100-FLAG-ITEM.
           IF WS-WORK-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE VOID FEE ON THE EXCEPT AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4110-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN PPYM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO PPYT-VOID-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYT-REVERSAL-FLAG = 'Y'
                   COMPUTE PPYT-VOID-AMT ROUNDED = PPYM-EXCEPT-AMT *
                       PPYM-MARGIN / 200
               WHEN PPYM-REGION-CD = 'A'
                   MOVE 3.00 TO PPYT-VOID-AMT
               WHEN PPYM-REGION-CD = 'B'
                   MOVE 10.00 TO PPYT-VOID-AMT
               WHEN OTHER
                   COMPUTE PPYT-VOID-AMT ROUNDED = PPYM-EXCEPT-AMT *
                       PPYM-MARGIN / 100
           END-EVALUATE
           IF PPYT-VOID-AMT > 0 AND PPYT-VOID-AMT < 5.00
               MOVE 5.00 TO PPYT-VOID-AMT
           END-IF
           IF PPYT-VOID-AMT > 75.00
               MOVE 75.00 TO PPYT-VOID-AMT
           END-IF
           ADD PPYT-VOID-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ISSUED AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4120-ACCRUE-ISSUED.
           IF PPYM-ISSUED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPYM-ISSUED-AMT * PPYM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO PPYM-ISSUED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAID YTD FEE ON THE EXCEPT AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4130-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN PPYM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO PPYM-PAID-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYM-EXEMPT-FLAG = 'Y'
                   COMPUTE PPYM-PAID-YTD ROUNDED = PPYM-EXCEPT-AMT *
                       PPYM-RATE / 200
               WHEN PPYT-TRAN-CD = 'IS'
                   MOVE 10.00 TO PPYM-PAID-YTD
               WHEN PPYT-TRAN-CD = 'PD'
                   MOVE 5.00 TO PPYM-PAID-YTD
               WHEN OTHER
                   COMPUTE PPYM-PAID-YTD ROUNDED = PPYM-EXCEPT-AMT *
                       PPYM-RATE / 100
           END-EVALUATE
           IF PPYM-PAID-YTD > 0 AND PPYM-PAID-YTD < 1.00
               MOVE 1.00 TO PPYM-PAID-YTD
           END-IF
           IF PPYM-PAID-YTD > 75.00
               MOVE 75.00 TO PPYM-PAID-YTD
           END-IF
           ADD PPYM-PAID-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4140-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPYT-CHANNEL-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPYT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR MATCH RULE CD:
      * A -> B WHEN LAST PAY DATE IS PAST
      * B -> F WHEN LAST PAY DATE IS PAST
      * F -> M WHEN LAST PAY DATE IS PAST
      * M -> S WHEN LAST PAY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4150-SET-MATCH-RULE.
           EVALUATE PPYM-MATCH-RULE-CD
               WHEN 'A'
                   IF PPYM-LAST-PAY-DATE > 0 AND PPYM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'B' TO PPYM-MATCH-RULE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'B'
                   IF PPYM-LAST-PAY-DATE > 0 AND PPYM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'F' TO PPYM-MATCH-RULE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'F'
                   IF PPYM-LAST-PAY-DATE > 0 AND PPYM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO PPYM-MATCH-RULE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF PPYM-LAST-PAY-DATE > 0 AND PPYM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO PPYM-MATCH-RULE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ISSUED AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4160-ACCRUE-ISSUED.
           IF PPYM-ISSUED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPYM-ISSUED-AMT * PPYM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO PPYM-ISSUED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E387 REF NO REQUIRED
      * E458 WORK AMT 2 NEGATIVE
      * E745 WORK AMT 2 OVER 1000000
      * E678 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       4170-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPYM-REF-NO = SPACES
                   MOVE 'E387' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E458' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 1000000.00
                   MOVE 'E745' TO WS-REJECT-CODE
               WHEN PPYM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E678' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 90 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4180-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF PPYM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPYM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO PPYM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE STALE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ISSUED AMT.
      *----------------------------------------------------------------*
       4190-PRORATE-ISSUED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPYM-ISSUED-AMT ROUNDED =
                   PPYM-STALE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PPYM-ISSUED-AMT
           END-IF
           ADD PPYM-ISSUED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PAID AMT IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4200-LIMIT-PAID.
           IF PPYT-PAID-AMT < 0.00
               MOVE 0.00 TO PPYT-PAID-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PPYT-PAID-AMT > 2500.00
               MOVE 2500.00 TO PPYT-PAID-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * VOID YTD IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4210-LIMIT-VOID.
           IF PPYM-VOID-YTD < 10.00
               MOVE 10.00 TO PPYM-VOID-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PPYM-VOID-YTD > 10000.00
               MOVE 10000.00 TO PPYM-VOID-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAID AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4220-ACCRUE-PAID.
           IF PPYT-PAID-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPYT-PAID-AMT * PPYM-RATE / 100 / 365
               ADD WS-WORK-AMT TO PPYM-STALE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E716 DESC REQUIRED
      * E833 ISSUED AMT NEGATIVE
      * E459 ISSUED AMT OVER 10000
      * E622 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       4230-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPYM-DESC = SPACES
                   MOVE 'E716' TO WS-REJECT-CODE
               WHEN PPYT-ISSUED-AMT < 0
                   MOVE 'E833' TO WS-REJECT-CODE
               WHEN PPYT-ISSUED-AMT > 10000.00
                   MOVE 'E459' TO WS-REJECT-CODE
               WHEN PPYM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E622' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAID FEE ON THE VOID YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4240-ASSESS-STOP.
           EVALUATE TRUE
               WHEN PPYM-STOP-FLAG = 'Y'
                   MOVE ZERO TO PPYT-PAID-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYT-REVERSAL-FLAG = 'Y'
                   COMPUTE PPYT-PAID-AMT ROUNDED = PPYM-VOID-YTD *
                       PPYM-DISCOUNT / 200
               WHEN PPYM-DECISION-CD = 'AND'
                   MOVE 20.00 TO PPYT-PAID-AMT
               WHEN PPYM-DECISION-CD = 'BER'
                   MOVE 20.00 TO PPYT-PAID-AMT
               WHEN OTHER
                   COMPUTE PPYT-PAID-AMT ROUNDED = PPYM-VOID-YTD *
                       PPYM-DISCOUNT / 100
           END-EVALUATE
           IF PPYT-PAID-AMT > 0 AND PPYT-PAID-AMT < 10.00
               MOVE 10.00 TO PPYT-PAID-AMT
           END-IF
           IF PPYT-PAID-AMT > 25.00
               MOVE 25.00 TO PPYT-PAID-AMT
           END-IF
           ADD PPYT-PAID-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 30 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4250-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF PPYM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPYM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO PPYM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ISSUED GRID TIER FOR THE VOID AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4260-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-ISSUED-GRID-RATE(1) TO PPYM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF PPYT-VOID-AMT NOT < WS-ISSUED-GRID-LOW(WS-TX)
                   MOVE WS-ISSUED-GRID-RATE(WS-TX) TO PPYM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPYM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPYM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE STALE FEE ON THE EXCEPT YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4270-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN PPYM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO PPYT-STALE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYM-OVERRIDE-FLAG = 'Y'
                   COMPUTE PPYT-STALE-AMT ROUNDED = PPYM-EXCEPT-YTD *
                       PPYM-DISCOUNT / 200
               WHEN PPYM-REGION-CD = 'A'
                   MOVE 7.00 TO PPYT-STALE-AMT
               WHEN PPYM-REGION-CD = 'B'
                   MOVE 7.00 TO PPYT-STALE-AMT
               WHEN OTHER
                   COMPUTE PPYT-STALE-AMT ROUNDED = PPYM-EXCEPT-YTD *
                       PPYM-DISCOUNT / 100
           END-EVALUATE
           IF PPYT-STALE-AMT > 0 AND PPYT-STALE-AMT < 1.00
               MOVE 1.00 TO PPYT-STALE-AMT
           END-IF
           IF PPYT-STALE-AMT > 35.00
               MOVE 35.00 TO PPYT-STALE-AMT
           END-IF
           ADD PPYT-STALE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4280-SPLIT-STALE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 3)
           COMPUTE PPYM-STALE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4290-COUNT-SEQ.
           IF PPYT-SEQ < 9999999
               ADD 1 TO PPYT-SEQ
           END-IF
           IF PPYT-SEQ NOT < 12
               MOVE 'Y' TO PPYM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ISSUED TIER TIER FOR THE ISSUED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4300-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-ISSUED-TIER-RATE(1) TO PPYM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF PPYM-ISSUED-AMT NOT < WS-ISSUED-TIER-LOW(WS-TX)
                   MOVE WS-ISSUED-TIER-RATE(WS-TX) TO PPYM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPYM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPYM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4310-SPLIT-PAID.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 12)
           COMPUTE PPYM-PAID-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAID YTD FEE ON THE STALE AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4320-ASSESS-STOP.
           EVALUATE TRUE
               WHEN PPYM-STOP-FLAG = 'Y'
                   MOVE ZERO TO PPYM-PAID-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYT-REVERSAL-FLAG = 'Y'
                   COMPUTE PPYM-PAID-YTD ROUNDED = PPYT-STALE-AMT *
                       PPYM-DISCOUNT / 200
               WHEN PPYM-REASON-CD = 'B'
                   MOVE 15.00 TO PPYM-PAID-YTD
               WHEN PPYM-REASON-CD = 'M'
                   MOVE 7.00 TO PPYM-PAID-YTD
               WHEN OTHER
                   COMPUTE PPYM-PAID-YTD ROUNDED = PPYT-STALE-AMT *
                       PPYM-DISCOUNT / 100
           END-EVALUATE
           IF PPYM-PAID-YTD > 0 AND PPYM-PAID-YTD < 1.00
               MOVE 1.00 TO PPYM-PAID-YTD
           END-IF
           IF PPYM-PAID-YTD > 50.00
               MOVE 50.00 TO PPYM-PAID-YTD
           END-IF
           ADD PPYM-PAID-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE VOID FEE ON THE PAID YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4330-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN PPYT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO PPYM-VOID-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYM-EXEMPT-FLAG = 'Y'
                   COMPUTE PPYM-VOID-AMT ROUNDED = PPYM-PAID-YTD *
                       PPYM-DISCOUNT / 200
               WHEN PPYM-STATUS-CD = 'DR'
                   MOVE 3.00 TO PPYM-VOID-AMT
               WHEN PPYM-STATUS-CD = 'TK'
                   MOVE 15.00 TO PPYM-VOID-AMT
               WHEN OTHER
                   COMPUTE PPYM-VOID-AMT ROUNDED = PPYM-PAID-YTD *
                       PPYM-DISCOUNT / 100
           END-EVALUATE
           IF PPYM-VOID-AMT > 0 AND PPYM-VOID-AMT < 2.00
               MOVE 2.00 TO PPYM-VOID-AMT
           END-IF
           IF PPYM-VOID-AMT > 35.00
               MOVE 35.00 TO PPYM-VOID-AMT
           END-IF
           ADD PPYM-VOID-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE EXCEPT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO STALE AMT.
      *----------------------------------------------------------------*
       4340-PRORATE-STALE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPYM-STALE-AMT ROUNDED =
                   PPYM-EXCEPT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PPYM-STALE-AMT
           END-IF
           ADD PPYM-STALE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       4350-COUNT-SEQ.
           IF PPYT-SEQ < 9999999
               ADD 1 TO PPYT-SEQ
           END-IF
           IF PPYT-SEQ NOT < 3
               MOVE 'Y' TO PPYM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ISSUED GRID TIER FOR THE PAID AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4360-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-ISSUED-GRID-RATE(1) TO PPYM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF PPYM-PAID-AMT NOT < WS-ISSUED-GRID-LOW(WS-TX)
                   MOVE WS-ISSUED-GRID-RATE(WS-TX) TO PPYM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPYM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPYM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4370-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF PPYM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPYM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO PPYM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STALE SCHED TIER FOR THE PAID YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4380-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-STALE-SCHED-RATE(1) TO PPYM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF PPYM-PAID-YTD NOT < WS-STALE-SCHED-LOW(WS-TX)
                   MOVE WS-STALE-SCHED-RATE(WS-TX) TO PPYM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPYM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPYM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ISSUED GRID TIER FOR THE STALE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4390-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-ISSUED-GRID-RATE(1) TO PPYM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF PPYM-STALE-YTD NOT < WS-ISSUED-GRID-LOW(WS-TX)
                   MOVE WS-ISSUED-GRID-RATE(WS-TX) TO PPYM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPYM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPYM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4400-DESCRIBE-CHANNEL.
           EVALUATE PPYT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STALE YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4410-FLAG-STALE.
           IF PPYM-STALE-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PPYM-STALE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4420-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF PPYM-CLOSE-DATE > 0 AND PPYM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPYM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PPYM-VOID-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD PPYM-VOID-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD PPYM-VOID-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD PPYM-VOID-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPYM-VOID-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE PPY FIELDS TO PPELG AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7020-CALL-PPELG.
           INITIALIZE PPELG-PARMS
           MOVE PPYM-PAID-AMT
             TO PPELG-PAID-AMT
           MOVE PPYM-PAID-YTD
             TO PPELG-PAID-YTD
           MOVE PPYM-VOID-AMT
             TO PPELG-VOID-AMT
           MOVE PPYM-ISSUED-YTD
             TO PPELG-ISSUED-YTD
           MOVE PPYM-EXCEPT-AMT
             TO PPELG-EXCEPT-AMT
           MOVE PPYM-STALE-AMT
             TO PPELG-STALE-AMT
           MOVE PPYM-NEXT-DUE-DATE
             TO PPELG-NEXT-DUE-DATE
           MOVE PPYM-EFFECTIVE-DATE
             TO PPELG-EFFECTIVE-DATE
           MOVE PPYM-EXPIRY-DATE
             TO PPELG-EXPIRY-DATE
           MOVE PPYM-CLOSE-DATE
             TO PPELG-CLOSE-DATE
           MOVE PPYM-STATUS-CD
             TO PPELG-STATUS-CD
           MOVE PPYM-REASON-CD
             TO PPELG-REASON-CD
           MOVE PPYM-CYCLE-CD
             TO PPELG-CYCLE-CD
           MOVE PPYM-REGION-CD
             TO PPELG-REGION-CD
           MOVE PPYM-HOLD-FLAG
             TO PPELG-HOLD-FLAG
           MOVE PPYM-STOP-FLAG
             TO PPELG-STOP-FLAG
           MOVE PPYM-NOTICE-SENT-FLAG
             TO PPELG-NOTICE-SENT-FLAG
           MOVE PPYM-EXCEPT-CNT
             TO PPELG-EXCEPT-CNT
           MOVE PPYM-CYCLE-CNT
             TO PPELG-CYCLE-CNT
           MOVE PPYM-DISCOUNT
             TO PPELG-DISCOUNT
           MOVE PPYM-MARGIN
             TO PPELG-MARGIN
           MOVE PPYM-REF-NO
             TO PPELG-REF-NO
           MOVE PPYM-CONTACT
             TO PPELG-CONTACT
           CALL 'PPELG' USING PPELG-PARMS
           EVALUATE PPELG-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE PPELG-PAID-AMT
                     TO PPYM-PAID-AMT
                   MOVE PPELG-ISSUED-YTD
                     TO PPYM-ISSUED-YTD
                   MOVE PPELG-EXCEPT-AMT
                     TO PPYM-EXCEPT-AMT
                   MOVE PPELG-HOLD-FLAG
                     TO PPYM-HOLD-FLAG
                   MOVE PPELG-STOP-FLAG
                     TO PPYM-STOP-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RELG' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-PPELG' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE PPYT-KEY TO RD-KEY
           MOVE PPYT-TRAN-CD TO RD-CODE
           MOVE PPYT-VOID-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE PPYT-KEY TO RD-KEY
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
           DISPLAY 'PPYAUDT READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'PPYAUDT' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
