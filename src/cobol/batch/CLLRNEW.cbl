      *================================================================*
      * PROGRAM  : CLLRNEW                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : COLLECTIONS QUEUE - RENEWAL. READS THE COL          *
      *            TRANSACTIONS, UPDATES THE COL MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. DELINQUENT ACCOUNT COLLECTION   *
      *            QUEUES, PROMISES AND LETTERS                        *
      * INPUT    : COLMST (KSDS), COLTRN, BANKCTL                      *
      * OUTPUT   : COLOUT, CLLRNRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, CLELG, CLTIER, CLLIM    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-11-26 EWS  ORIGINAL                                       *
      * 1995-09-26 TWH  PROMISED CALCULATION CORRECTED PRB0207         *
      * 1997-04-01 RJT  PERFORMANCE - FEE READ AHEAD INC2512           *
      * 1997-07-19 PQR  PAST-DUE TABLE EXPANDED INC4148                *
      * 1997-10-27 SKN  PERFORMANCE - FEE READ AHEAD REQ9154           *
      * 1998-06-07 LMB  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1998-08-21 GAF  WRITEOFF EXCEPTIONS TO OPS QUEUE CHG7440       *
      * 2000-03-25 LMB  AUDIT FINDING - PROMISED CONTROL TOTALS INC5994*
      * 2001-06-20 TWH  PAST-DUE TABLE EXPANDED CHG0588                *
      * 2002-04-03 CRP  COLLECTED TABLE EXPANDED INC0873               *
      * 2002-07-17 CRP  WRITEOFF ROUNDING - HALF UP PRB3911            *
      * 2004-01-24 DPS  PAST-DUE LIMIT REVISED PRB0704                 *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLLRNEW.
       AUTHOR. C R PATEL.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "COLMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS COLM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "COLTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "COLOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "CLLRNRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY COLMST.
       FD  TRAN-FILE.
       COPY COLTRN.
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
                                           VALUE 21.000000.
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
      * WS-WRITEOFF-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-WRITEOFF-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000436000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000372500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000457000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001050000000169500'.
       01  WS-WRITEOFF-TIER-TBL REDEFINES WS-WRITEOFF-TIER-VALUES.
           05  WS-WRITEOFF-TIER-ROW OCCURS 4 TIMES.
               10  WS-WRITEOFF-TIER-LOW    PIC 9(09)V99.
               10  WS-WRITEOFF-TIER-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-COLLECTE-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-COLLECTE-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000603000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000611000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003000000000664000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003250000000727500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003350000000443000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008350000000143000'.
       01  WS-COLLECTE-BAND-TBL REDEFINES WS-COLLECTE-BAND-VALUES.
           05  WS-COLLECTE-BAND-ROW OCCURS 6 TIMES.
               10  WS-COLLECTE-BAND-LOW    PIC 9(09)V99.
               10  WS-COLLECTE-BAND-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-FEE-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FEE-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000837000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000385000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010050000000321500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012550000000144000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012600000000656000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015100000000751000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017600000000070000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00027600000000900000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00028600000000159000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00028700000000385000'.
       01  WS-FEE-GRID-TBL REDEFINES WS-FEE-GRID-VALUES.
           05  WS-FEE-GRID-ROW OCCURS 10 TIMES.
               10  WS-FEE-GRID-LOW         PIC 9(09)V99.
               10  WS-FEE-GRID-RATE        PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-COLLECTE-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-COLLECTE-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000577500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000331500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000600000000873500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010600000000047500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013100000000127000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013200000000108000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013250000000096000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013750000000330000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014750000000166000'.
       01  WS-COLLECTE-SCHED-TBL REDEFINES WS-COLLECTE-SCHED-VALUES.
           05  WS-COLLECTE-SCHED-ROW OCCURS 9 TIMES.
               10  WS-COLLECTE-SCHED-LOW   PIC 9(09)V99.
               10  WS-COLLECTE-SCHED-RATE  PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  CLLRNEW  COLLECTIONS QUEUE'.
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
       COPY CLELGP.
       COPY CLTIERP.
       COPY CLLIMP.

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
               MOVE 'COLMST' TO ABND-FILE-NAME
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
           MOVE COLT-KEY TO COLM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-SET-QUEUE
               PERFORM 3010-ASSESS-PREFERRED
               PERFORM 3020-PRORATE-WRITEOFF
               PERFORM 3030-ACCRUE-PAST-DUE
               PERFORM 3040-PRORATE-COLLECTED
               PERFORM 3050-ACCUM-QUEUE
               PERFORM 3060-AGE-NEXT-REVIEW
               PERFORM 3070-DESCRIBE-CYCLE
               PERFORM 3080-SET-CYCLE
               PERFORM 3090-LIMIT-PROMISED
               PERFORM 3100-SET-REGION
               PERFORM 3110-ACCUM-REGION
               PERFORM 3120-ASSESS-NOTICE-SENT
               PERFORM 3130-ASSESS-EXEMPT
               PERFORM 3140-EDIT-OPERATOR
               PERFORM 3150-FLAG-ITEM
               PERFORM 3160-SELECT-PENALTY-PCT
               PERFORM 3170-DESCRIBE-CYCLE
               PERFORM 3180-FLAG-ITEM
               PERFORM 3190-ACCUM-CHANNEL
               PERFORM 3200-AGE-OPEN
               PERFORM 3210-ACCRUE-FEE
               PERFORM 3220-ACCRUE-COLLECTED
               PERFORM 3230-SELECT-PENALTY-PCT
               PERFORM 3240-SELECT-PENALTY-PCT
               PERFORM 3250-EDIT-ADDR
               PERFORM 3260-ACCRUE-PENALTY-PCT
               PERFORM 3270-EDIT-ADDR
               PERFORM 3280-SET-QUEUE
               PERFORM 3290-AGE-POST
               PERFORM 3300-ACCRUE-PROMISED
               PERFORM 3310-COUNT-SEQ
               PERFORM 3320-FLAG-COLLECTED
               PERFORM 3330-PRORATE-COLLECTED
               PERFORM 3340-ASSESS-NOTICE-SENT
               PERFORM 3350-PRORATE-WRITEOFF
               PERFORM 3360-FLAG-COLLECTED
               PERFORM 3370-SPLIT-COLLECTED
               PERFORM 3380-ACCUM-REGION
               PERFORM 3390-DESCRIBE-TRAN
               PERFORM 3400-SELECT-MARGIN
               PERFORM 3410-ACCRUE-PAST-DUE
               PERFORM 3420-AGE-EFF
               PERFORM 3430-CHECK-EXPIRY
               PERFORM 3440-COUNT-NOTICE
               PERFORM 3450-DESCRIBE-REGION
               PERFORM 3460-ACCRUE-FEE
               PERFORM 3470-ASSESS-PREFERRED
               PERFORM 3480-COUNT-RETRY
               PERFORM 3490-DESCRIBE-TRAN
               PERFORM 3500-SELECT-PENALTY-PCT
               PERFORM 3510-DESCRIBE-CYCLE
               PERFORM 3520-CHECK-NEXT-REVIEW
               PERFORM 3530-CHECK-EFF
               PERFORM 3540-EDIT-REJECT
               PERFORM 3550-CHECK-EFF
               PERFORM 3560-FLAG-ITEM
               PERFORM 3570-COUNT-RETRY
               PERFORM 3580-ACCUM-CYCLE
               PERFORM 3590-ACCUM-CYCLE
               PERFORM 3600-COUNT-NOTICE
               PERFORM 3610-ASSESS-SUSPEND
               PERFORM 3620-COUNT-ITEM
               PERFORM 3630-PRORATE-WRITEOFF
               PERFORM 3640-FLAG-PAST-DUE
               PERFORM 3650-AGE-OPEN
               PERFORM 3660-AGE-OPEN
               PERFORM 3670-ACCRUE-FEE-PCT
               PERFORM 3680-FLAG-PROMISED
               PERFORM 3690-SET-REGION
               PERFORM 3700-LIMIT-PAST-DUE
               PERFORM 3710-SELECT-MARGIN
               PERFORM 3720-EDIT-DESC
               PERFORM 3730-FLAG-WRITEOFF
               PERFORM 3740-CHECK-OPEN
               PERFORM 3750-EDIT-REJECT
               PERFORM 3760-AGE-LAST-PAY
               PERFORM 3770-PRORATE-FEE
               PERFORM 3780-CHECK-NEXT-DUE
               PERFORM 3790-ACCRUE-WRITEOFF
               PERFORM 3800-PRORATE-WRITEOFF
               PERFORM 3810-EDIT-DESC
               PERFORM 3820-ASSESS-REVIEW
               PERFORM 3830-FLAG-ITEM
               PERFORM 3840-ACCRUE-PAST-DUE
               PERFORM 3850-PRORATE-COLLECTED
               PERFORM 3860-ASSESS-SUSPEND
               PERFORM 3870-DESCRIBE-ACTION
               PERFORM 3880-EDIT-REJECT
               PERFORM 3890-FLAG-FEE
               PERFORM 3900-SPLIT-FEE
               PERFORM 3910-ACCUM-REASON
               PERFORM 3920-ACCUM-QUEUE
               PERFORM 3930-AGE-POST
               PERFORM 3940-SPLIT-COLLECTED
               PERFORM 3950-EDIT-CONTACT
               PERFORM 3960-ACCRUE-COLLECTED
               PERFORM 3970-AGE-EFF
               PERFORM 3980-COUNT-RETRY
               PERFORM 3990-PRORATE-WRITEOFF
               PERFORM 7000-CALL-CLELG
               PERFORM 7010-CALL-CLTIER
               PERFORM 7030-CALL-CLLIM
               REWRITE COL-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'COLMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM COL-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR QUEUE CD:
      * DL -> ER WHEN POST DATE IS PAST
      * ER -> HA WHEN POST DATE IS PAST
      * HA -> HW WHEN POST DATE IS PAST
      * HW -> ME WHEN POST DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3000-SET-QUEUE.
           EVALUATE COLM-QUEUE-CD
               WHEN 'DL'
                   IF COLT-POST-DATE > 0 AND COLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ER' TO COLM-QUEUE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ER'
                   IF COLT-POST-DATE > 0 AND COLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HA' TO COLM-QUEUE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HA'
                   IF COLT-POST-DATE > 0 AND COLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HW' TO COLM-QUEUE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HW'
                   IF COLT-POST-DATE > 0 AND COLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ME' TO COLM-QUEUE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE FEE FEE ON THE COLLECTED YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3010-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN COLM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO COLM-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLT-REVERSAL-FLAG = 'Y'
                   COMPUTE COLM-FEE-AMT ROUNDED = COLM-COLLECTED-YTD *
                       COLM-MARGIN / 200
               WHEN COLM-REASON-CD = 'AT'
                   MOVE 3.00 TO COLM-FEE-AMT
               WHEN COLM-REASON-CD = 'BJ'
                   MOVE 20.00 TO COLM-FEE-AMT
               WHEN OTHER
                   COMPUTE COLM-FEE-AMT ROUNDED = COLM-COLLECTED-YTD *
                       COLM-MARGIN / 100
           END-EVALUATE
           IF COLM-FEE-AMT > 0 AND COLM-FEE-AMT < 2.00
               MOVE 2.00 TO COLM-FEE-AMT
           END-IF
           IF COLM-FEE-AMT > 75.00
               MOVE 75.00 TO COLM-FEE-AMT
           END-IF
           ADD COLM-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE WRITEOFF AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3020-PRORATE-WRITEOFF.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLT-WRITEOFF-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAST DUE AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3030-ACCRUE-PAST-DUE.
           IF COLM-PAST-DUE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLM-PAST-DUE-AMT * COLM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO COLM-PROMISED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE COLLECTED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3040-PRORATE-COLLECTED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   COLM-COLLECTED-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE WRITEOFF AMT BY QUEUE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3050-ACCUM-QUEUE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = COLM-QUEUE-CD
                   ADD COLT-WRITEOFF-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE COLM-QUEUE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE COLT-WRITEOFF-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3060-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF COLM-NEXT-REVIEW-DATE > 0 AND COLM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD COLM-PROMISED-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD COLM-PROMISED-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD COLM-PROMISED-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD COLM-PROMISED-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLM-PROMISED-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3070-DESCRIBE-CYCLE.
           EVALUATE COLM-CYCLE-CD
               WHEN 'BE'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'GV'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'JM'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'LC'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'MK'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'MS'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'WW'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * BE -> GV WHEN OPEN DATE IS PAST
      * GV -> JM WHEN OPEN DATE IS PAST
      * JM -> LC WHEN OPEN DATE IS PAST
      * LC -> MK WHEN OPEN DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3080-SET-CYCLE.
           EVALUATE COLM-CYCLE-CD
               WHEN 'BE'
                   IF COLM-OPEN-DATE > 0 AND COLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GV' TO COLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GV'
                   IF COLM-OPEN-DATE > 0 AND COLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JM' TO COLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JM'
                   IF COLM-OPEN-DATE > 0 AND COLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LC' TO COLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LC'
                   IF COLM-OPEN-DATE > 0 AND COLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MK' TO COLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PROMISED AMT IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3090-LIMIT-PROMISED.
           IF COLM-PROMISED-AMT < 0.00
               MOVE 0.00 TO COLM-PROMISED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF COLM-PROMISED-AMT > 1000.00
               MOVE 1000.00 TO COLM-PROMISED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * AT -> AU WHEN LAST MAINT DATE IS PAST
      * AU -> XR WHEN LAST MAINT DATE IS PAST
      * XR -> XU WHEN LAST MAINT DATE IS PAST
      * XU -> AT WHEN LAST MAINT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3100-SET-REGION.
           EVALUATE COLM-REGION-CD
               WHEN 'AT'
                   IF COLM-LAST-MAINT-DATE > 0 AND COLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'AU' TO COLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AU'
                   IF COLM-LAST-MAINT-DATE > 0 AND COLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'XR' TO COLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XR'
                   IF COLM-LAST-MAINT-DATE > 0 AND COLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'XU' TO COLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XU'
                   IF COLM-LAST-MAINT-DATE > 0 AND COLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'AT' TO COLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE PROMISED YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3110-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = COLM-REGION-CD
                   ADD COLM-PROMISED-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE COLM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE COLM-PROMISED-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PROMISED YTD FEE ON THE WORK AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3120-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN COLM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO COLM-PROMISED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLM-PREFERRED-FLAG = 'Y'
                   COMPUTE COLM-PROMISED-YTD ROUNDED = WS-WORK-AMT *
                       COLM-MARGIN / 200
               WHEN COLM-STATUS-CD = 'PS'
                   MOVE 12.00 TO COLM-PROMISED-YTD
               WHEN COLM-STATUS-CD = 'UR'
                   MOVE 15.00 TO COLM-PROMISED-YTD
               WHEN OTHER
                   COMPUTE COLM-PROMISED-YTD ROUNDED = WS-WORK-AMT *
                       COLM-MARGIN / 100
           END-EVALUATE
           IF COLM-PROMISED-YTD > 0 AND COLM-PROMISED-YTD < 1.00
               MOVE 1.00 TO COLM-PROMISED-YTD
           END-IF
           IF COLM-PROMISED-YTD > 75.00
               MOVE 75.00 TO COLM-PROMISED-YTD
           END-IF
           ADD COLM-PROMISED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE FEE FEE ON THE COLLECTED YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3130-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN COLM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO COLT-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE COLT-FEE-AMT ROUNDED = COLM-COLLECTED-YTD *
                       COLM-MARGIN / 200
               WHEN COLM-QUEUE-CD = 'DL'
                   MOVE 3.00 TO COLT-FEE-AMT
               WHEN COLM-QUEUE-CD = 'ER'
                   MOVE 20.00 TO COLT-FEE-AMT
               WHEN OTHER
                   COMPUTE COLT-FEE-AMT ROUNDED = COLM-COLLECTED-YTD *
                       COLM-MARGIN / 100
           END-EVALUATE
           IF COLT-FEE-AMT > 0 AND COLT-FEE-AMT < 2.00
               MOVE 2.00 TO COLT-FEE-AMT
           END-IF
           IF COLT-FEE-AMT > 50.00
               MOVE 50.00 TO COLT-FEE-AMT
           END-IF
           ADD COLT-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E369 OPERATOR REQUIRED
      * E687 WORK AMT 2 NEGATIVE
      * E332 WORK AMT 2 OVER 250000
      * E662 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3140-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN COLT-OPERATOR = SPACES
                   MOVE 'E369' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E687' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 250000.00
                   MOVE 'E332' TO WS-REJECT-CODE
               WHEN COLM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E662' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3150-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE WRITEOFF TIER TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3160-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-WRITEOFF-TIER-RATE(1) TO COLM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF COLM-FEE-AMT NOT < WS-WRITEOFF-TIER-LOW(WS-TX)
                   MOVE WS-WRITEOFF-TIER-RATE(WS-TX) TO COLM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF COLM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO COLM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3170-DESCRIBE-CYCLE.
           EVALUATE COLM-CYCLE-CD
               WHEN 'BE'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'GV'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'JM'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'LC'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'MK'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'MS'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'WW'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3180-FLAG-ITEM.
           IF WS-WORK-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WRITEOFF AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3190-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = COLT-CHANNEL-CD
                   ADD COLM-WRITEOFF-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE COLT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE COLM-WRITEOFF-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3200-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF COLM-OPEN-DATE > 0 AND COLM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD COLM-COLLECTED-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD COLM-COLLECTED-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD COLM-COLLECTED-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD COLM-COLLECTED-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLM-COLLECTED-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE YTD AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3210-ACCRUE-FEE.
           IF COLM-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLM-FEE-YTD * COLM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO COLM-COLLECTED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON COLLECTED AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3220-ACCRUE-COLLECTED.
           IF COLT-COLLECTED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLT-COLLECTED-AMT * COLM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO COLM-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE WRITEOFF TIER TIER FOR THE PROMISED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3230-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-WRITEOFF-TIER-RATE(1) TO COLM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF COLM-PROMISED-YTD NOT < WS-WRITEOFF-TIER-LOW(WS-TX)
                   MOVE WS-WRITEOFF-TIER-RATE(WS-TX) TO COLM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF COLM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO COLM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE GRID TIER FOR THE FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3240-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-GRID-RATE(1) TO COLM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF COLM-FEE-YTD NOT < WS-FEE-GRID-LOW(WS-TX)
                   MOVE WS-FEE-GRID-RATE(WS-TX) TO COLM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF COLM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO COLM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E976 ADDR REQUIRED
      * E833 PAST DUE AMT NEGATIVE
      * E194 PAST DUE AMT OVER 5000
      * E882 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3250-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN COLM-ADDR = SPACES
                   MOVE 'E976' TO WS-REJECT-CODE
               WHEN COLM-PAST-DUE-AMT < 0
                   MOVE 'E833' TO WS-REJECT-CODE
               WHEN COLM-PAST-DUE-AMT > 5000.00
                   MOVE 'E194' TO WS-REJECT-CODE
               WHEN COLM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E882' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3260-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * COLM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO COLM-PAST-DUE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E042 ADDR REQUIRED
      * E714 WRITEOFF AMT NEGATIVE
      * E972 WRITEOFF AMT OVER 99999
      * E605 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3270-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN COLM-ADDR = SPACES
                   MOVE 'E042' TO WS-REJECT-CODE
               WHEN COLM-WRITEOFF-AMT < 0
                   MOVE 'E714' TO WS-REJECT-CODE
               WHEN COLM-WRITEOFF-AMT > 99999.00
                   MOVE 'E972' TO WS-REJECT-CODE
               WHEN COLM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E605' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR QUEUE CD:
      * DL -> ER WHEN LAST PAY DATE IS PAST
      * ER -> HA WHEN LAST PAY DATE IS PAST
      * HA -> HW WHEN LAST PAY DATE IS PAST
      * HW -> ME WHEN LAST PAY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3280-SET-QUEUE.
           EVALUATE COLM-QUEUE-CD
               WHEN 'DL'
                   IF COLM-LAST-PAY-DATE > 0 AND COLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ER' TO COLM-QUEUE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ER'
                   IF COLM-LAST-PAY-DATE > 0 AND COLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HA' TO COLM-QUEUE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HA'
                   IF COLM-LAST-PAY-DATE > 0 AND COLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HW' TO COLM-QUEUE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HW'
                   IF COLM-LAST-PAY-DATE > 0 AND COLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ME' TO COLM-QUEUE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3290-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF COLT-POST-DATE > 0 AND COLT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD COLM-COLLECTED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD COLM-COLLECTED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD COLM-COLLECTED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD COLM-COLLECTED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLM-COLLECTED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PROMISED YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3300-ACCRUE-PROMISED.
           IF COLM-PROMISED-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLM-PROMISED-YTD * COLM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO COLM-COLLECTED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3310-COUNT-SEQ.
           IF COLT-SEQ < 9999999
               ADD 1 TO COLT-SEQ
           END-IF
           IF COLT-SEQ NOT < 5
               MOVE 'Y' TO COLM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * COLLECTED AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3320-FLAG-COLLECTED.
           IF COLT-COLLECTED-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE COLT-COLLECTED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAST DUE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO COLLECTED AMT.
      *----------------------------------------------------------------*
       3330-PRORATE-COLLECTED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE COLT-COLLECTED-AMT ROUNDED =
                   COLM-PAST-DUE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO COLT-COLLECTED-AMT
           END-IF
           ADD COLT-COLLECTED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE PROMISED FEE ON THE PROMISED YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3340-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN COLM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO COLM-PROMISED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLM-OVERRIDE-FLAG = 'Y'
                   COMPUTE COLM-PROMISED-AMT ROUNDED = COLM-PROMISED-YTD
                       * COLM-FEE-PCT / 200
               WHEN COLM-STATUS-CD = 'PS'
                   MOVE 3.00 TO COLM-PROMISED-AMT
               WHEN COLM-STATUS-CD = 'UR'
                   MOVE 20.00 TO COLM-PROMISED-AMT
               WHEN OTHER
                   COMPUTE COLM-PROMISED-AMT ROUNDED = COLM-PROMISED-YTD
                       * COLM-FEE-PCT / 100
           END-EVALUATE
           IF COLM-PROMISED-AMT > 0 AND COLM-PROMISED-AMT < 5.00
               MOVE 5.00 TO COLM-PROMISED-AMT
           END-IF
           IF COLM-PROMISED-AMT > 25.00
               MOVE 25.00 TO COLM-PROMISED-AMT
           END-IF
           ADD COLM-PROMISED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE WRITEOFF AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3350-PRORATE-WRITEOFF.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   COLT-WRITEOFF-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * COLLECTED AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3360-FLAG-COLLECTED.
           IF COLT-COLLECTED-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE COLT-COLLECTED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT COLLECTED AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3370-SPLIT-COLLECTED.
           COMPUTE WS-WORK-AMT = COLT-COLLECTED-AMT / 4
           COMPUTE WS-WORK-AMT-2 = COLT-COLLECTED-AMT - (WS-WORK-AMT *
               4)
           COMPUTE COLT-PAST-DUE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3380-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = COLM-REGION-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE COLM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3390-DESCRIBE-TRAN.
           EVALUATE COLT-TRAN-CD
               WHEN 'PR'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'CL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'LT'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'WO'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'RF'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE FEE GRID TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3400-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-GRID-RATE(1) TO COLM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF WS-WORK-AMT-2 NOT < WS-FEE-GRID-LOW(WS-TX)
                   MOVE WS-FEE-GRID-RATE(WS-TX) TO COLM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF COLM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO COLM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAST DUE AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3410-ACCRUE-PAST-DUE.
           IF COLM-PAST-DUE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLM-PAST-DUE-AMT * COLM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO COLT-PAST-DUE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3420-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF COLT-EFF-DATE > 0 AND COLT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD COLM-WRITEOFF-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD COLM-WRITEOFF-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD COLM-WRITEOFF-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD COLM-WRITEOFF-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLM-WRITEOFF-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3430-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF COLM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO COLM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3440-COUNT-NOTICE.
           IF COLM-NOTICE-CNT < 9999999
               ADD 1 TO COLM-NOTICE-CNT
           END-IF
           IF COLM-NOTICE-CNT NOT < 12
               MOVE 'Y' TO COLM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3450-DESCRIBE-REGION.
           EVALUATE COLM-REGION-CD
               WHEN 'AT'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'AU'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'XR'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'XU'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3460-ACCRUE-FEE.
           IF COLM-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLM-FEE-AMT * COLM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO COLM-PROMISED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAST DUE FEE ON THE FEE YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3470-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN COLM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO COLM-PAST-DUE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLM-OVERRIDE-FLAG = 'Y'
                   COMPUTE COLM-PAST-DUE-AMT ROUNDED = COLM-FEE-YTD *
                       COLM-FEE-PCT / 200
               WHEN COLT-TRAN-CD = 'PR'
                   MOVE 20.00 TO COLM-PAST-DUE-AMT
               WHEN COLT-TRAN-CD = 'CL'
                   MOVE 20.00 TO COLM-PAST-DUE-AMT
               WHEN OTHER
                   COMPUTE COLM-PAST-DUE-AMT ROUNDED = COLM-FEE-YTD *
                       COLM-FEE-PCT / 100
           END-EVALUATE
           IF COLM-PAST-DUE-AMT > 0 AND COLM-PAST-DUE-AMT < 5.00
               MOVE 5.00 TO COLM-PAST-DUE-AMT
           END-IF
           IF COLM-PAST-DUE-AMT > 50.00
               MOVE 50.00 TO COLM-PAST-DUE-AMT
           END-IF
           ADD COLM-PAST-DUE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3480-COUNT-RETRY.
           IF COLM-RETRY-CNT < 9999999
               ADD 1 TO COLM-RETRY-CNT
           END-IF
           IF COLM-RETRY-CNT NOT < 10
               MOVE 'Y' TO COLM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3490-DESCRIBE-TRAN.
           EVALUATE COLT-TRAN-CD
               WHEN 'PR'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'CL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'LT'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'WO'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'RF'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE COLLECTE SCHED TIER FOR THE PAST DUE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3500-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-COLLECTE-SCHED-RATE(1) TO COLM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF COLM-PAST-DUE-AMT NOT < WS-COLLECTE-SCHED-LOW(WS-TX)
                   MOVE WS-COLLECTE-SCHED-RATE(WS-TX) TO
                       COLM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF COLM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO COLM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3510-DESCRIBE-CYCLE.
           EVALUATE COLM-CYCLE-CD
               WHEN 'BE'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'GV'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'JM'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'LC'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'MK'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'MS'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'WW'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 180 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3520-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF COLM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO COLM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 180 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3530-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF COLT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO COLM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E223 REJECT CD REQUIRED
      * E075 WORK AMT 2 NEGATIVE
      * E260 WORK AMT 2 OVER 50000
      * E838 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3540-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN COLT-REJECT-CD = SPACES
                   MOVE 'E223' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E075' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 50000.00
                   MOVE 'E260' TO WS-REJECT-CODE
               WHEN COLM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E838' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 60 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3550-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF COLT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO COLM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3560-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3570-COUNT-RETRY.
           IF COLM-RETRY-CNT < 9999999
               ADD 1 TO COLM-RETRY-CNT
           END-IF
           IF COLM-RETRY-CNT NOT < 12
               MOVE 'Y' TO COLM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3580-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = COLM-CYCLE-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE COLM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PROMISED YTD BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3590-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = COLM-CYCLE-CD
                   ADD COLM-PROMISED-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE COLM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE COLM-PROMISED-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3600-COUNT-NOTICE.
           IF COLM-NOTICE-CNT < 9999999
               ADD 1 TO COLM-NOTICE-CNT
           END-IF
           IF COLM-NOTICE-CNT NOT < 12
               MOVE 'Y' TO COLM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE PAST DUE AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3610-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN COLM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLM-OVERRIDE-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = COLT-PAST-DUE-AMT *
                       COLM-FEE-PCT / 200
               WHEN COLM-REGION-CD = 'AT'
                   MOVE 3.00 TO WS-WORK-AMT-2
               WHEN COLM-REGION-CD = 'AU'
                   MOVE 20.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = COLT-PAST-DUE-AMT *
                       COLM-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 5.00
               MOVE 5.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 35.00
               MOVE 35.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3620-COUNT-ITEM.
           IF COLM-ITEM-CNT < 9999999
               ADD 1 TO COLM-ITEM-CNT
           END-IF
           IF COLM-ITEM-CNT NOT < 3
               MOVE 'Y' TO COLM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE COLLECTED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WRITEOFF AMT.
      *----------------------------------------------------------------*
       3630-PRORATE-WRITEOFF.
           IF WS-PERIOD-DAYS > 0
               COMPUTE COLT-WRITEOFF-AMT ROUNDED =
                   COLM-COLLECTED-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO COLT-WRITEOFF-AMT
           END-IF
           ADD COLT-WRITEOFF-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PAST DUE AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3640-FLAG-PAST-DUE.
           IF COLT-PAST-DUE-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE COLT-PAST-DUE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3650-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF COLM-OPEN-DATE > 0 AND COLM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD COLM-WRITEOFF-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD COLM-WRITEOFF-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD COLM-WRITEOFF-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD COLM-WRITEOFF-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLM-WRITEOFF-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3660-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF COLM-OPEN-DATE > 0 AND COLM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD COLT-WRITEOFF-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD COLT-WRITEOFF-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD COLT-WRITEOFF-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD COLT-WRITEOFF-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLT-WRITEOFF-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3670-ACCRUE-FEE-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * COLM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO COLT-COLLECTED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PROMISED AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3680-FLAG-PROMISED.
           IF COLM-PROMISED-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE COLM-PROMISED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * AT -> AU WHEN NEXT REVIEW DATE IS PAST
      * AU -> XR WHEN NEXT REVIEW DATE IS PAST
      * XR -> XU WHEN NEXT REVIEW DATE IS PAST
      * XU -> AT WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3690-SET-REGION.
           EVALUATE COLM-REGION-CD
               WHEN 'AT'
                   IF COLM-NEXT-REVIEW-DATE > 0 AND
                       COLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'AU' TO COLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AU'
                   IF COLM-NEXT-REVIEW-DATE > 0 AND
                       COLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'XR' TO COLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XR'
                   IF COLM-NEXT-REVIEW-DATE > 0 AND
                       COLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'XU' TO COLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XU'
                   IF COLM-NEXT-REVIEW-DATE > 0 AND
                       COLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'AT' TO COLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAST DUE YTD IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3700-LIMIT-PAST-DUE.
           IF COLM-PAST-DUE-YTD < 10.00
               MOVE 10.00 TO COLM-PAST-DUE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF COLM-PAST-DUE-YTD > 500.00
               MOVE 500.00 TO COLM-PAST-DUE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE GRID TIER FOR THE WRITEOFF YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3710-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-GRID-RATE(1) TO COLM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF COLM-WRITEOFF-YTD NOT < WS-FEE-GRID-LOW(WS-TX)
                   MOVE WS-FEE-GRID-RATE(WS-TX) TO COLM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF COLM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO COLM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E597 DESC REQUIRED
      * E745 PROMISED AMT NEGATIVE
      * E874 PROMISED AMT OVER 250000
      * E269 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3720-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN COLM-DESC = SPACES
                   MOVE 'E597' TO WS-REJECT-CODE
               WHEN COLM-PROMISED-AMT < 0
                   MOVE 'E745' TO WS-REJECT-CODE
               WHEN COLM-PROMISED-AMT > 250000.00
                   MOVE 'E874' TO WS-REJECT-CODE
               WHEN COLM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E269' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WRITEOFF AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3730-FLAG-WRITEOFF.
           IF COLT-WRITEOFF-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE COLT-WRITEOFF-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 15 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3740-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF COLM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO COLT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E992 REJECT CD REQUIRED
      * E489 COLLECTED AMT NEGATIVE
      * E167 COLLECTED AMT OVER 25000
      * E263 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3750-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN COLT-REJECT-CD = SPACES
                   MOVE 'E992' TO WS-REJECT-CODE
               WHEN COLT-COLLECTED-AMT < 0
                   MOVE 'E489' TO WS-REJECT-CODE
               WHEN COLT-COLLECTED-AMT > 25000.00
                   MOVE 'E167' TO WS-REJECT-CODE
               WHEN COLM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E263' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3760-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF COLM-LAST-PAY-DATE > 0 AND COLM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD COLM-COLLECTED-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD COLM-COLLECTED-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD COLM-COLLECTED-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD COLM-COLLECTED-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLM-COLLECTED-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE PROMISED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FEE AMT.
      *----------------------------------------------------------------*
       3770-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE COLT-FEE-AMT ROUNDED =
                   COLM-PROMISED-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO COLT-FEE-AMT
           END-IF
           ADD COLT-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3780-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF COLM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO COLM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WRITEOFF AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3790-ACCRUE-WRITEOFF.
           IF COLM-WRITEOFF-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLM-WRITEOFF-AMT * COLM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO COLT-PAST-DUE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WRITEOFF AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3800-PRORATE-WRITEOFF.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLM-WRITEOFF-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E565 DESC REQUIRED
      * E562 WORK AMT 2 NEGATIVE
      * E867 WORK AMT 2 OVER 50000
      * E938 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3810-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN COLM-DESC = SPACES
                   MOVE 'E565' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E562' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 50000.00
                   MOVE 'E867' TO WS-REJECT-CODE
               WHEN COLM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E938' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WRITEOFF FEE ON THE FEE YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3820-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN COLM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO COLM-WRITEOFF-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLM-PREFERRED-FLAG = 'Y'
                   COMPUTE COLM-WRITEOFF-AMT ROUNDED = COLM-FEE-YTD *
                       COLM-MARGIN / 200
               WHEN COLT-CHANNEL-CD = 'BRN'
                   MOVE 5.00 TO COLM-WRITEOFF-AMT
               WHEN COLT-CHANNEL-CD = 'BAT'
                   MOVE 3.00 TO COLM-WRITEOFF-AMT
               WHEN OTHER
                   COMPUTE COLM-WRITEOFF-AMT ROUNDED = COLM-FEE-YTD *
                       COLM-MARGIN / 100
           END-EVALUATE
           IF COLM-WRITEOFF-AMT > 0 AND COLM-WRITEOFF-AMT < 2.00
               MOVE 2.00 TO COLM-WRITEOFF-AMT
           END-IF
           IF COLM-WRITEOFF-AMT > 100.00
               MOVE 100.00 TO COLM-WRITEOFF-AMT
           END-IF
           ADD COLM-WRITEOFF-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3830-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAST DUE AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3840-ACCRUE-PAST-DUE.
           IF COLT-PAST-DUE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLT-PAST-DUE-AMT * COLM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO COLM-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PROMISED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO COLLECTED YTD.
      *----------------------------------------------------------------*
       3850-PRORATE-COLLECTED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE COLM-COLLECTED-YTD ROUNDED =
                   COLM-PROMISED-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO COLM-COLLECTED-YTD
           END-IF
           ADD COLM-COLLECTED-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE COLLECTED FEE ON THE WORK AMT 2.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3860-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN COLM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO COLT-COLLECTED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLM-REVIEW-FLAG = 'Y'
                   COMPUTE COLT-COLLECTED-AMT ROUNDED = WS-WORK-AMT-2 *
                       COLM-PENALTY-PCT / 200
               WHEN COLT-CHANNEL-CD = 'BRN'
                   MOVE 20.00 TO COLT-COLLECTED-AMT
               WHEN COLT-CHANNEL-CD = 'BAT'
                   MOVE 12.00 TO COLT-COLLECTED-AMT
               WHEN OTHER
                   COMPUTE COLT-COLLECTED-AMT ROUNDED = WS-WORK-AMT-2 *
                       COLM-PENALTY-PCT / 100
           END-EVALUATE
           IF COLT-COLLECTED-AMT > 0 AND COLT-COLLECTED-AMT < 2.00
               MOVE 2.00 TO COLT-COLLECTED-AMT
           END-IF
           IF COLT-COLLECTED-AMT > 25.00
               MOVE 25.00 TO COLT-COLLECTED-AMT
           END-IF
           ADD COLT-COLLECTED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE ACTION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3870-DESCRIBE-ACTION.
           EVALUATE COLM-ACTION-CD
               WHEN 'KJ'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'TM'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'VL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'ZS'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E012 REJECT CD REQUIRED
      * E281 WRITEOFF AMT NEGATIVE
      * E775 WRITEOFF AMT OVER 1000000
      * E416 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3880-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN COLT-REJECT-CD = SPACES
                   MOVE 'E012' TO WS-REJECT-CODE
               WHEN COLM-WRITEOFF-AMT < 0
                   MOVE 'E281' TO WS-REJECT-CODE
               WHEN COLM-WRITEOFF-AMT > 1000000.00
                   MOVE 'E775' TO WS-REJECT-CODE
               WHEN COLM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E416' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3890-FLAG-FEE.
           IF COLT-FEE-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE COLT-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3900-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = COLM-FEE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = COLM-FEE-AMT - (WS-WORK-AMT * 2)
           COMPUTE COLM-WRITEOFF-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAST DUE AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3910-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = COLM-REASON-CD
                   ADD COLM-PAST-DUE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE COLM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE COLM-PAST-DUE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAST DUE YTD BY QUEUE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3920-ACCUM-QUEUE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = COLM-QUEUE-CD
                   ADD COLM-PAST-DUE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE COLM-QUEUE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE COLM-PAST-DUE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3930-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF COLT-POST-DATE > 0 AND COLT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD COLM-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD COLM-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD COLM-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD COLM-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLM-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3940-SPLIT-COLLECTED.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 12)
           COMPUTE COLM-COLLECTED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E046 CONTACT REQUIRED
      * E264 PROMISED AMT NEGATIVE
      * E979 PROMISED AMT OVER 99999
      * E906 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3950-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN COLM-CONTACT = SPACES
                   MOVE 'E046' TO WS-REJECT-CODE
               WHEN COLM-PROMISED-AMT < 0
                   MOVE 'E264' TO WS-REJECT-CODE
               WHEN COLM-PROMISED-AMT > 99999.00
                   MOVE 'E979' TO WS-REJECT-CODE
               WHEN COLM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E906' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON COLLECTED AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3960-ACCRUE-COLLECTED.
           IF COLT-COLLECTED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLT-COLLECTED-AMT * COLM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO COLT-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-90, 91-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3970-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF COLT-EFF-DATE > 0 AND COLT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD COLT-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD COLT-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD COLT-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD COLT-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLT-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3980-COUNT-RETRY.
           IF COLM-RETRY-CNT < 9999999
               ADD 1 TO COLM-RETRY-CNT
           END-IF
           IF COLM-RETRY-CNT NOT < 10
               MOVE 'Y' TO COLM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WRITEOFF AMT.
      *----------------------------------------------------------------*
       3990-PRORATE-WRITEOFF.
           IF WS-PERIOD-DAYS > 0
               COMPUTE COLM-WRITEOFF-AMT ROUNDED =
                   COLT-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO COLM-WRITEOFF-AMT
           END-IF
           ADD COLM-WRITEOFF-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PASS THE COL FIELDS TO CLELG AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7000-CALL-CLELG.
           INITIALIZE CLELG-PARMS
           MOVE COLM-WRITEOFF-AMT
             TO CLELG-WRITEOFF-AMT
           MOVE COLM-COLLECTED-AMT
             TO CLELG-COLLECTED-AMT
           MOVE COLM-FEE-AMT
             TO CLELG-FEE-AMT
           MOVE COLM-WRITEOFF-YTD
             TO CLELG-WRITEOFF-YTD
           MOVE COLM-PAST-DUE-AMT
             TO CLELG-PAST-DUE-AMT
           MOVE COLM-FEE-YTD
             TO CLELG-FEE-YTD
           MOVE COLM-EFFECTIVE-DATE
             TO CLELG-EFFECTIVE-DATE
           MOVE COLM-EXPIRY-DATE
             TO CLELG-EXPIRY-DATE
           MOVE COLM-LAST-MAINT-DATE
             TO CLELG-LAST-MAINT-DATE
           MOVE COLM-NEXT-DUE-DATE
             TO CLELG-NEXT-DUE-DATE
           MOVE COLM-CYCLE-CD
             TO CLELG-CYCLE-CD
           MOVE COLM-QUEUE-CD
             TO CLELG-QUEUE-CD
           MOVE COLM-STATUS-CD
             TO CLELG-STATUS-CD
           MOVE COLM-REASON-CD
             TO CLELG-REASON-CD
           MOVE COLM-NOTICE-SENT-FLAG
             TO CLELG-NOTICE-SENT-FLAG
           MOVE COLM-EXEMPT-FLAG
             TO CLELG-EXEMPT-FLAG
           MOVE COLM-OVERRIDE-FLAG
             TO CLELG-OVERRIDE-FLAG
           MOVE COLM-NOTICE-CNT
             TO CLELG-NOTICE-CNT
           MOVE COLM-ITEM-CNT
             TO CLELG-ITEM-CNT
           MOVE COLM-MARGIN
             TO CLELG-MARGIN
           MOVE COLM-FEE-PCT
             TO CLELG-FEE-PCT
           MOVE COLM-DESC
             TO CLELG-DESC
           MOVE COLM-ADDR
             TO CLELG-ADDR
           CALL 'CLELG' USING CLELG-PARMS
           EVALUATE CLELG-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE CLELG-COLLECTED-AMT
                     TO COLM-COLLECTED-AMT
                   MOVE CLELG-PAST-DUE-AMT
                     TO COLM-PAST-DUE-AMT
                   MOVE CLELG-FEE-YTD
                     TO COLM-FEE-YTD
                   MOVE CLELG-EXEMPT-FLAG
                     TO COLM-EXEMPT-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RELG' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-CLELG' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE COL FIELDS TO CLTIER AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7010-CALL-CLTIER.
           INITIALIZE CLTIER-PARMS
           MOVE COLM-COLLECTED-YTD
             TO CLTIER-COLLECTED-YTD
           MOVE COLM-PAST-DUE-YTD
             TO CLTIER-PAST-DUE-YTD
           MOVE COLM-PROMISED-AMT
             TO CLTIER-PROMISED-AMT
           MOVE COLM-PAST-DUE-AMT
             TO CLTIER-PAST-DUE-AMT
           MOVE COLM-COLLECTED-AMT
             TO CLTIER-COLLECTED-AMT
           MOVE COLM-PROMISED-YTD
             TO CLTIER-PROMISED-YTD
           MOVE COLM-NEXT-DUE-DATE
             TO CLTIER-NEXT-DUE-DATE
           MOVE COLM-OPEN-DATE
             TO CLTIER-OPEN-DATE
           MOVE COLM-LAST-PAY-DATE
             TO CLTIER-LAST-PAY-DATE
           MOVE COLM-CLOSE-DATE
             TO CLTIER-CLOSE-DATE
           MOVE COLM-STATUS-CD
             TO CLTIER-STATUS-CD
           MOVE COLM-REASON-CD
             TO CLTIER-REASON-CD
           MOVE COLM-ACTION-CD
             TO CLTIER-ACTION-CD
           MOVE COLM-CYCLE-CD
             TO CLTIER-CYCLE-CD
           MOVE COLM-EXEMPT-FLAG
             TO CLTIER-EXEMPT-FLAG
           MOVE COLM-SUSPEND-FLAG
             TO CLTIER-SUSPEND-FLAG
           MOVE COLM-OVERRIDE-FLAG
             TO CLTIER-OVERRIDE-FLAG
           MOVE COLM-CYCLE-CNT
             TO CLTIER-CYCLE-CNT
           MOVE COLM-RETRY-CNT
             TO CLTIER-RETRY-CNT
           MOVE COLM-FEE-PCT
             TO CLTIER-FEE-PCT
           MOVE COLM-MARGIN
             TO CLTIER-MARGIN
           MOVE COLM-CONTACT
             TO CLTIER-CONTACT
           MOVE COLM-CUST-ID
             TO CLTIER-CUST-ID
           CALL 'CLTIER' USING CLTIER-PARMS
           EVALUATE CLTIER-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE CLTIER-PAST-DUE-AMT
                     TO COLM-PAST-DUE-AMT
                   MOVE CLTIER-PROMISED-YTD
                     TO COLM-PROMISED-YTD
                   MOVE CLTIER-EXEMPT-FLAG
                     TO COLM-EXEMPT-FLAG
                   MOVE CLTIER-SUSPEND-FLAG
                     TO COLM-SUSPEND-FLAG
                   MOVE CLTIER-OVERRIDE-FLAG
                     TO COLM-OVERRIDE-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RIER' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-CLTIER' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE COL FIELDS TO CLLIM AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7030-CALL-CLLIM.
           INITIALIZE CLLIM-PARMS
           MOVE COLM-COLLECTED-YTD
             TO CLLIM-COLLECTED-YTD
           MOVE COLM-COLLECTED-AMT
             TO CLLIM-COLLECTED-AMT
           MOVE COLM-PAST-DUE-AMT
             TO CLLIM-PAST-DUE-AMT
           MOVE COLM-PROMISED-YTD
             TO CLLIM-PROMISED-YTD
           MOVE COLM-WRITEOFF-YTD
             TO CLLIM-WRITEOFF-YTD
           MOVE COLM-FEE-YTD
             TO CLLIM-FEE-YTD
           MOVE COLM-CLOSE-DATE
             TO CLLIM-CLOSE-DATE
           MOVE COLM-NOTICE-DATE
             TO CLLIM-NOTICE-DATE
           MOVE COLM-OPEN-DATE
             TO CLLIM-OPEN-DATE
           MOVE COLM-LAST-PAY-DATE
             TO CLLIM-LAST-PAY-DATE
           MOVE COLM-CYCLE-CD
             TO CLLIM-CYCLE-CD
           MOVE COLM-REASON-CD
             TO CLLIM-REASON-CD
           MOVE COLM-QUEUE-CD
             TO CLLIM-QUEUE-CD
           MOVE COLM-REGION-CD
             TO CLLIM-REGION-CD
           MOVE COLM-REVIEW-FLAG
             TO CLLIM-REVIEW-FLAG
           MOVE COLM-OVERRIDE-FLAG
             TO CLLIM-OVERRIDE-FLAG
           MOVE COLM-PREFERRED-FLAG
             TO CLLIM-PREFERRED-FLAG
           MOVE COLM-ITEM-CNT
             TO CLLIM-ITEM-CNT
           MOVE COLM-NOTICE-CNT
             TO CLLIM-NOTICE-CNT
           MOVE COLM-FEE-PCT
             TO CLLIM-FEE-PCT
           MOVE COLM-MARGIN
             TO CLLIM-MARGIN
           MOVE COLM-CONTACT
             TO CLLIM-CONTACT
           MOVE COLM-NAME
             TO CLLIM-NAME
           CALL 'CLLIM' USING CLLIM-PARMS
           EVALUATE CLLIM-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE CLLIM-WRITEOFF-YTD
                     TO COLM-WRITEOFF-YTD
                   MOVE CLLIM-FEE-YTD
                     TO COLM-FEE-YTD
                   MOVE CLLIM-REVIEW-FLAG
                     TO COLM-REVIEW-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RLIM' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7030-CALL-CLLIM' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE COLT-KEY TO RD-KEY
           MOVE COLT-TRAN-CD TO RD-CODE
           MOVE COLT-COLLECTED-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE COLT-KEY TO RD-KEY
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
           DISPLAY 'CLLRNEW READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'CLLRNEW' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
