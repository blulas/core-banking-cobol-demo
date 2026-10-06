      *================================================================*
      * PROGRAM  : CLLCALC                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : COLLECTIONS QUEUE - PERIODIC CALCULATION. READS THE *
      *            COL TRANSACTIONS, UPDATES THE COL MASTER AND        *
      *            PRODUCES THE CONTROL REPORT. DELINQUENT ACCOUNT     *
      *            COLLECTION QUEUES, PROMISES AND LETTERS             *
      * INPUT    : COLMST (KSDS), COLTRN, BANKCTL                      *
      * OUTPUT   : COLOUT, CLLCARPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, CLLIM                   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-04-26 PQR  ORIGINAL                                       *
      * 1997-05-08 SKN  PROMISED ROUNDING - HALF UP INC0688            *
      * 1998-09-28 JLM  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-11-21 LMB  WRITEOFF FEE SCHEDULE UPDATE INC6803           *
      * 2003-04-24 DPS  PAST-DUE TABLE EXPANDED CHG9043                *
      * 2003-02-08 GAF  PAST-DUE TABLE EXPANDED REQ8849                *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLLCALC.
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
           SELECT REPORT-FILE ASSIGN TO "CLLCARPT"
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
                                           VALUE 15.000000.
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
      * WS-WRITEOFF-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-WRITEOFF-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000731000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000246000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000300000000850500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000550000000236000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010550000000768000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011550000000903500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012050000000560000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012300000000680000'.
       01  WS-WRITEOFF-SCHED-TBL REDEFINES WS-WRITEOFF-SCHED-VALUES.
           05  WS-WRITEOFF-SCHED-ROW OCCURS 8 TIMES.
               10  WS-WRITEOFF-SCHED-LOW   PIC 9(09)V99.
               10  WS-WRITEOFF-SCHED-RATE  PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-FEE-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FEE-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000490500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000204500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001050000000702500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002050000000528000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002150000000837000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002250000000589000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002300000000738500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002800000000229500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002850000000093500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003350000000424500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013350000000576500'.
       01  WS-FEE-SCHED-TBL REDEFINES WS-FEE-SCHED-VALUES.
           05  WS-FEE-SCHED-ROW OCCURS 11 TIMES.
               10  WS-FEE-SCHED-LOW        PIC 9(09)V99.
               10  WS-FEE-SCHED-RATE       PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-PROMISED-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PROMISED-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000219000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000384000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005050000000798000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007550000000069000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007800000000689000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017800000000289500'.
       01  WS-PROMISED-GRID-TBL REDEFINES WS-PROMISED-GRID-VALUES.
           05  WS-PROMISED-GRID-ROW OCCURS 6 TIMES.
               10  WS-PROMISED-GRID-LOW    PIC 9(09)V99.
               10  WS-PROMISED-GRID-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-WRITEOFF-SCHEDX - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-WRITEOFF-SCHEDX-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000584000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000633000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000150000000662500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000065500'.
       01  WS-WRITEOFF-SCHEDX-TBL REDEFINES WS-WRITEOFF-SCHEDX-VALUES.
           05  WS-WRITEOFF-SCHEDX-ROW OCCURS 4 TIMES.
               10  WS-WRITEOFF-SCHEDX-LOW  PIC 9(09)V99.
               10  WS-WRITEOFF-SCHEDX-RATE PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  CLLCALC  COLLECTIONS QUEUE'.
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
               PERFORM 3000-FLAG-FEE
               PERFORM 3010-LIMIT-COLLECTED
               PERFORM 3020-SELECT-MARGIN
               PERFORM 3030-SPLIT-ITEM
               PERFORM 3040-SPLIT-PAST-DUE
               PERFORM 3050-AGE-EXPIRY
               PERFORM 3060-FLAG-PAST-DUE
               PERFORM 3070-DESCRIBE-ACTION
               PERFORM 3080-AGE-NEXT-REVIEW
               PERFORM 3090-PRORATE-COLLECTED
               PERFORM 3100-ACCRUE-PAST-DUE
               PERFORM 3110-CHECK-NEXT-DUE
               PERFORM 3120-ACCUM-QUEUE
               PERFORM 3130-SELECT-PENALTY-PCT
               PERFORM 3140-EDIT-OPERATOR
               PERFORM 3150-ASSESS-PREFERRED
               PERFORM 3160-EDIT-REF-NO
               PERFORM 3170-PRORATE-COLLECTED
               PERFORM 3180-SPLIT-WRITEOFF
               PERFORM 3190-PRORATE-WRITEOFF
               PERFORM 3200-ACCRUE-COLLECTED
               PERFORM 3210-PRORATE-FEE
               PERFORM 3220-EDIT-OPERATOR
               PERFORM 3230-COUNT-SEQ
               PERFORM 3240-SET-QUEUE
               PERFORM 3250-EDIT-DESC
               PERFORM 3260-ASSESS-SUSPEND
               PERFORM 3270-LIMIT-COLLECTED
               PERFORM 3280-PRORATE-WRITEOFF
               PERFORM 3290-ACCRUE-PAST-DUE
               PERFORM 3300-CHECK-OPEN
               PERFORM 3310-COUNT-CYCLE
               PERFORM 3320-ASSESS-SUSPEND
               PERFORM 3330-PRORATE-WRITEOFF
               PERFORM 3340-LIMIT-PAST-DUE
               PERFORM 3350-ACCUM-QUEUE
               PERFORM 3360-COUNT-NOTICE
               PERFORM 3370-ACCUM-CHANNEL
               PERFORM 3380-EDIT-CONTACT
               PERFORM 3390-DESCRIBE-TRAN
               PERFORM 3400-SELECT-PENALTY-PCT
               PERFORM 3410-SET-QUEUE
               PERFORM 3420-FLAG-WRITEOFF
               PERFORM 3430-ACCRUE-PAST-DUE
               PERFORM 3440-ASSESS-EXEMPT
               PERFORM 3450-SPLIT-PAST-DUE
               PERFORM 3460-SET-TRAN
               PERFORM 3470-SELECT-MARGIN
               PERFORM 3480-ACCUM-CHANNEL
               PERFORM 3490-DESCRIBE-CYCLE
               PERFORM 3500-DESCRIBE-QUEUE
               PERFORM 3510-CHECK-LAST-PAY
               PERFORM 3520-SET-STATUS
               PERFORM 3530-DESCRIBE-REGION
               PERFORM 3540-LIMIT-COLLECTED
               PERFORM 3550-ACCUM-CYCLE
               PERFORM 3560-ACCRUE-PROMISED
               PERFORM 3570-PRORATE-FEE
               PERFORM 3580-ACCRUE-FEE
               PERFORM 3590-PRORATE-FEE
               PERFORM 3600-LIMIT-ITEM
               PERFORM 3610-SPLIT-COLLECTED
               PERFORM 3620-DESCRIBE-QUEUE
               PERFORM 3630-ACCRUE-WRITEOFF
               PERFORM 3640-FLAG-FEE
               PERFORM 3650-COUNT-ITEM
               PERFORM 3660-SET-TRAN
               PERFORM 3670-ASSESS-PREFERRED
               PERFORM 3680-PRORATE-PAST-DUE
               PERFORM 3690-LIMIT-FEE
               PERFORM 3700-EDIT-CONTACT
               PERFORM 3710-SET-REASON
               PERFORM 3720-LIMIT-PAST-DUE
               PERFORM 3730-ACCUM-TRAN
               PERFORM 3740-EDIT-OPERATOR
               PERFORM 3750-LIMIT-WRITEOFF
               PERFORM 3760-ASSESS-NOTICE-SENT
               PERFORM 3770-DESCRIBE-QUEUE
               PERFORM 3780-ACCUM-QUEUE
               PERFORM 3790-SPLIT-PROMISED
               PERFORM 3800-ASSESS-EXEMPT
               PERFORM 3810-SELECT-MARGIN
               PERFORM 3820-ACCUM-CYCLE
               PERFORM 3830-FLAG-WRITEOFF
               PERFORM 3840-LIMIT-FEE
               PERFORM 3850-LIMIT-PROMISED
               PERFORM 3860-LIMIT-PROMISED
               PERFORM 3870-ASSESS-SUSPEND
               PERFORM 3880-COUNT-RETRY
               PERFORM 3890-ACCUM-STATUS
               PERFORM 3900-ACCRUE-PAST-DUE
               PERFORM 3910-ACCRUE-PAST-DUE
               PERFORM 3920-DESCRIBE-TRAN
               PERFORM 3930-ACCRUE-COLLECTED
               PERFORM 3940-FLAG-PAST-DUE
               PERFORM 3950-SPLIT-FEE
               PERFORM 3960-CHECK-NEXT-DUE
               PERFORM 3970-PRORATE-FEE
               PERFORM 3980-SELECT-FEE-PCT
               PERFORM 3990-ASSESS-EXEMPT
               PERFORM 4000-CHECK-POST
               PERFORM 4010-LIMIT-PROMISED
               PERFORM 4020-ASSESS-EXEMPT
               PERFORM 4030-SPLIT-FEE
               PERFORM 4040-PRORATE-COLLECTED
               PERFORM 4050-FLAG-COLLECTED
               PERFORM 4060-ACCRUE-PENALTY-PCT
               PERFORM 4070-SET-QUEUE
               PERFORM 4080-PRORATE-COLLECTED
               PERFORM 4090-SPLIT-COLLECTED
               PERFORM 4100-SELECT-MARGIN
               PERFORM 4110-ACCUM-QUEUE
               PERFORM 4120-COUNT-RETRY
               PERFORM 4130-COUNT-SEQ
               PERFORM 4140-AGE-POST
               PERFORM 4150-EDIT-REF-NO
               PERFORM 4160-AGE-CLOSE
               PERFORM 4170-ACCUM-CYCLE
               PERFORM 4180-COUNT-SEQ
               PERFORM 4190-SELECT-FEE-PCT
               PERFORM 4200-SELECT-FEE-PCT
               PERFORM 4210-SPLIT-COLLECTED
               PERFORM 4220-COUNT-ITEM
               PERFORM 4230-DESCRIBE-REASON
               PERFORM 4240-DESCRIBE-QUEUE
               PERFORM 4250-SPLIT-COLLECTED
               PERFORM 4260-ASSESS-PREFERRED
               PERFORM 4270-AGE-NEXT-DUE
               PERFORM 4280-CHECK-EFFECTIVE
               PERFORM 4290-SPLIT-COLLECTED
               PERFORM 4300-SET-TRAN
               PERFORM 4310-ASSESS-REVIEW
               PERFORM 4320-ACCRUE-COLLECTED
               PERFORM 4330-SPLIT-WRITEOFF
               PERFORM 4340-ACCUM-CHANNEL
               PERFORM 4350-SELECT-FEE-PCT
               PERFORM 4360-ASSESS-REVERSAL
               PERFORM 4370-CHECK-CLOSE
               PERFORM 4380-PRORATE-FEE
               PERFORM 4390-CHECK-EFF
               PERFORM 4400-SET-CHANNEL
               PERFORM 4410-ASSESS-SUSPEND
               PERFORM 4420-DESCRIBE-ACTION
               PERFORM 4430-EDIT-CUST-ID
               PERFORM 4440-EDIT-OPERATOR
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
      * FEE YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3000-FLAG-FEE.
           IF COLM-FEE-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE COLM-FEE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * COLLECTED AMT IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3010-LIMIT-COLLECTED.
           IF COLM-COLLECTED-AMT < 100.00
               MOVE 100.00 TO COLM-COLLECTED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF COLM-COLLECTED-AMT > 500.00
               MOVE 500.00 TO COLM-COLLECTED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PROMISED GRID TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3020-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PROMISED-GRID-RATE(1) TO COLM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF WS-WORK-AMT-2 NOT < WS-PROMISED-GRID-LOW(WS-TX)
                   MOVE WS-PROMISED-GRID-RATE(WS-TX) TO COLM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF COLM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO COLM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3030-SPLIT-ITEM.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PAST DUE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3040-SPLIT-PAST-DUE.
           COMPUTE WS-WORK-AMT = COLT-PAST-DUE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = COLT-PAST-DUE-AMT - (WS-WORK-AMT *
               12)
           COMPUTE COLM-PAST-DUE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3050-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF COLM-EXPIRY-DATE > 0 AND COLM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-EXPIRY-DATE TO DTU-DATE-1
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
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAST DUE AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3060-FLAG-PAST-DUE.
           IF COLM-PAST-DUE-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE COLM-PAST-DUE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE ACTION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3070-DESCRIBE-ACTION.
           EVALUATE COLM-ACTION-CD
               WHEN 'KJ'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'TM'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'VL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'ZS'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-90, 91-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3080-AGE-NEXT-REVIEW.
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
                   ADD COLT-WRITEOFF-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD COLT-WRITEOFF-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD COLT-WRITEOFF-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD COLT-WRITEOFF-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLT-WRITEOFF-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE COLLECTED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO COLLECTED AMT.
      *----------------------------------------------------------------*
       3090-PRORATE-COLLECTED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE COLT-COLLECTED-AMT ROUNDED =
                   COLM-COLLECTED-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO COLT-COLLECTED-AMT
           END-IF
           ADD COLT-COLLECTED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAST DUE AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3100-ACCRUE-PAST-DUE.
           IF COLT-PAST-DUE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLT-PAST-DUE-AMT * COLM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3110-CHECK-NEXT-DUE.
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
           IF WS-DAYS > 180
               MOVE 'Y' TO COLT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PROMISED YTD BY QUEUE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3120-ACCUM-QUEUE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = COLM-QUEUE-CD
                   ADD COLM-PROMISED-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE COLM-QUEUE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE COLM-PROMISED-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE SCHED TIER FOR THE PAST DUE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3130-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-SCHED-RATE(1) TO COLM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF COLM-PAST-DUE-AMT NOT < WS-FEE-SCHED-LOW(WS-TX)
                   MOVE WS-FEE-SCHED-RATE(WS-TX) TO COLM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF COLM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO COLM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E297 OPERATOR REQUIRED
      * E077 COLLECTED YTD NEGATIVE
      * E404 COLLECTED YTD OVER 10000
      * E937 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3140-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN COLT-OPERATOR = SPACES
                   MOVE 'E297' TO WS-REJECT-CODE
               WHEN COLM-COLLECTED-YTD < 0
                   MOVE 'E077' TO WS-REJECT-CODE
               WHEN COLM-COLLECTED-YTD > 10000.00
                   MOVE 'E404' TO WS-REJECT-CODE
               WHEN COLT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E937' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FEE FEE ON THE PAST DUE AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3150-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN COLM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO COLM-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLM-SUSPEND-FLAG = 'Y'
                   COMPUTE COLM-FEE-AMT ROUNDED = COLM-PAST-DUE-AMT *
                       COLM-MARGIN / 200
               WHEN COLM-CYCLE-CD = 'BE'
                   MOVE 12.00 TO COLM-FEE-AMT
               WHEN COLM-CYCLE-CD = 'GV'
                   MOVE 15.00 TO COLM-FEE-AMT
               WHEN OTHER
                   COMPUTE COLM-FEE-AMT ROUNDED = COLM-PAST-DUE-AMT *
                       COLM-MARGIN / 100
           END-EVALUATE
           IF COLM-FEE-AMT > 0 AND COLM-FEE-AMT < 2.00
               MOVE 2.00 TO COLM-FEE-AMT
           END-IF
           IF COLM-FEE-AMT > 100.00
               MOVE 100.00 TO COLM-FEE-AMT
           END-IF
           ADD COLM-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E139 REF NO REQUIRED
      * E375 WRITEOFF YTD NEGATIVE
      * E527 WRITEOFF YTD OVER 5000
      * E662 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3160-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN COLM-REF-NO = SPACES
                   MOVE 'E139' TO WS-REJECT-CODE
               WHEN COLM-WRITEOFF-YTD < 0
                   MOVE 'E375' TO WS-REJECT-CODE
               WHEN COLM-WRITEOFF-YTD > 5000.00
                   MOVE 'E527' TO WS-REJECT-CODE
               WHEN COLM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E662' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PROMISED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO COLLECTED AMT.
      *----------------------------------------------------------------*
       3170-PRORATE-COLLECTED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE COLM-COLLECTED-AMT ROUNDED =
                   COLM-PROMISED-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO COLM-COLLECTED-AMT
           END-IF
           ADD COLM-COLLECTED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT WRITEOFF AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3180-SPLIT-WRITEOFF.
           COMPUTE WS-WORK-AMT = COLT-WRITEOFF-AMT / 3
           COMPUTE WS-WORK-AMT-2 = COLT-WRITEOFF-AMT - (WS-WORK-AMT * 3)
           COMPUTE COLM-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PROMISED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WRITEOFF YTD.
      *----------------------------------------------------------------*
       3190-PRORATE-WRITEOFF.
           IF WS-PERIOD-DAYS > 0
               COMPUTE COLM-WRITEOFF-YTD ROUNDED =
                   COLM-PROMISED-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO COLM-WRITEOFF-YTD
           END-IF
           ADD COLM-WRITEOFF-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON COLLECTED AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3200-ACCRUE-COLLECTED.
           IF COLM-COLLECTED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLM-COLLECTED-AMT * COLM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO COLT-PAST-DUE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAST DUE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FEE YTD.
      *----------------------------------------------------------------*
       3210-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE COLM-FEE-YTD ROUNDED =
                   COLM-PAST-DUE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO COLM-FEE-YTD
           END-IF
           ADD COLM-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E643 OPERATOR REQUIRED
      * E046 WRITEOFF AMT NEGATIVE
      * E941 WRITEOFF AMT OVER 1000000
      * E263 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3220-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN COLT-OPERATOR = SPACES
                   MOVE 'E643' TO WS-REJECT-CODE
               WHEN COLM-WRITEOFF-AMT < 0
                   MOVE 'E046' TO WS-REJECT-CODE
               WHEN COLM-WRITEOFF-AMT > 1000000.00
                   MOVE 'E941' TO WS-REJECT-CODE
               WHEN COLM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E263' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3230-COUNT-SEQ.
           IF COLT-SEQ < 9999999
               ADD 1 TO COLT-SEQ
           END-IF
           IF COLT-SEQ NOT < 5
               MOVE 'Y' TO COLM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR QUEUE CD:
      * DL -> ER WHEN EXPIRY DATE IS PAST
      * ER -> HA WHEN EXPIRY DATE IS PAST
      * HA -> HW WHEN EXPIRY DATE IS PAST
      * HW -> ME WHEN EXPIRY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3240-SET-QUEUE.
           EVALUATE COLM-QUEUE-CD
               WHEN 'DL'
                   IF COLM-EXPIRY-DATE > 0 AND COLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ER' TO COLM-QUEUE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ER'
                   IF COLM-EXPIRY-DATE > 0 AND COLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HA' TO COLM-QUEUE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HA'
                   IF COLM-EXPIRY-DATE > 0 AND COLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HW' TO COLM-QUEUE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HW'
                   IF COLM-EXPIRY-DATE > 0 AND COLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ME' TO COLM-QUEUE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E270 DESC REQUIRED
      * E320 WRITEOFF AMT NEGATIVE
      * E882 WRITEOFF AMT OVER 10000
      * E373 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3250-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN COLM-DESC = SPACES
                   MOVE 'E270' TO WS-REJECT-CODE
               WHEN COLM-WRITEOFF-AMT < 0
                   MOVE 'E320' TO WS-REJECT-CODE
               WHEN COLM-WRITEOFF-AMT > 10000.00
                   MOVE 'E882' TO WS-REJECT-CODE
               WHEN COLT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E373' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAST DUE YTD FEE ON THE COLLECTED YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3260-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN COLM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO COLM-PAST-DUE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE COLM-PAST-DUE-YTD ROUNDED =
                       COLM-COLLECTED-YTD * COLM-FEE-PCT / 200
               WHEN COLM-ACTION-CD = 'KJ'
                   MOVE 3.00 TO COLM-PAST-DUE-YTD
               WHEN COLM-ACTION-CD = 'TM'
                   MOVE 10.00 TO COLM-PAST-DUE-YTD
               WHEN OTHER
                   COMPUTE COLM-PAST-DUE-YTD ROUNDED =
                       COLM-COLLECTED-YTD * COLM-FEE-PCT / 100
           END-EVALUATE
           IF COLM-PAST-DUE-YTD > 0 AND COLM-PAST-DUE-YTD < 10.00
               MOVE 10.00 TO COLM-PAST-DUE-YTD
           END-IF
           IF COLM-PAST-DUE-YTD > 50.00
               MOVE 50.00 TO COLM-PAST-DUE-YTD
           END-IF
           ADD COLM-PAST-DUE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * COLLECTED AMT IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3270-LIMIT-COLLECTED.
           IF COLM-COLLECTED-AMT < 0.00
               MOVE 0.00 TO COLM-COLLECTED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF COLM-COLLECTED-AMT > 50000.00
               MOVE 50000.00 TO COLM-COLLECTED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAST DUE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WRITEOFF AMT.
      *----------------------------------------------------------------*
       3280-PRORATE-WRITEOFF.
           IF WS-PERIOD-DAYS > 0
               COMPUTE COLT-WRITEOFF-AMT ROUNDED =
                   COLM-PAST-DUE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO COLT-WRITEOFF-AMT
           END-IF
           ADD COLT-WRITEOFF-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAST DUE AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3290-ACCRUE-PAST-DUE.
           IF COLM-PAST-DUE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLM-PAST-DUE-AMT * COLM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO COLT-COLLECTED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 45 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3300-CHECK-OPEN.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO COLM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3310-COUNT-CYCLE.
           IF COLM-CYCLE-CNT < 9999999
               ADD 1 TO COLM-CYCLE-CNT
           END-IF
           IF COLM-CYCLE-CNT NOT < 10
               MOVE 'Y' TO COLM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAST DUE FEE ON THE PAST DUE YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3320-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN COLM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO COLT-PAST-DUE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLT-REVERSAL-FLAG = 'Y'
                   COMPUTE COLT-PAST-DUE-AMT ROUNDED = COLM-PAST-DUE-YTD
                       * COLM-MARGIN / 200
               WHEN COLM-QUEUE-CD = 'DL'
                   MOVE 10.00 TO COLT-PAST-DUE-AMT
               WHEN COLM-QUEUE-CD = 'ER'
                   MOVE 5.00 TO COLT-PAST-DUE-AMT
               WHEN OTHER
                   COMPUTE COLT-PAST-DUE-AMT ROUNDED = COLM-PAST-DUE-YTD
                       * COLM-MARGIN / 100
           END-EVALUATE
           IF COLT-PAST-DUE-AMT > 0 AND COLT-PAST-DUE-AMT < 5.00
               MOVE 5.00 TO COLT-PAST-DUE-AMT
           END-IF
           IF COLT-PAST-DUE-AMT > 50.00
               MOVE 50.00 TO COLT-PAST-DUE-AMT
           END-IF
           ADD COLT-PAST-DUE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE PAST DUE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WRITEOFF YTD.
      *----------------------------------------------------------------*
       3330-PRORATE-WRITEOFF.
           IF WS-PERIOD-DAYS > 0
               COMPUTE COLM-WRITEOFF-YTD ROUNDED =
                   COLM-PAST-DUE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO COLM-WRITEOFF-YTD
           END-IF
           ADD COLM-WRITEOFF-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PAST DUE AMT IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3340-LIMIT-PAST-DUE.
           IF COLT-PAST-DUE-AMT < 50.00
               MOVE 50.00 TO COLT-PAST-DUE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF COLT-PAST-DUE-AMT > 50000.00
               MOVE 50000.00 TO COLT-PAST-DUE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE COLLECTED AMT BY QUEUE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3350-ACCUM-QUEUE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = COLM-QUEUE-CD
                   ADD COLM-COLLECTED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE COLM-QUEUE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE COLM-COLLECTED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3360-COUNT-NOTICE.
           IF COLM-NOTICE-CNT < 9999999
               ADD 1 TO COLM-NOTICE-CNT
           END-IF
           IF COLM-NOTICE-CNT NOT < 3
               MOVE 'Y' TO COLM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PROMISED AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3370-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = COLT-CHANNEL-CD
                   ADD COLM-PROMISED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE COLT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE COLM-PROMISED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E291 CONTACT REQUIRED
      * E376 WRITEOFF AMT NEGATIVE
      * E815 WRITEOFF AMT OVER 250000
      * E785 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3380-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN COLM-CONTACT = SPACES
                   MOVE 'E291' TO WS-REJECT-CODE
               WHEN COLM-WRITEOFF-AMT < 0
                   MOVE 'E376' TO WS-REJECT-CODE
               WHEN COLM-WRITEOFF-AMT > 250000.00
                   MOVE 'E815' TO WS-REJECT-CODE
               WHEN COLM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E785' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3390-DESCRIBE-TRAN.
           EVALUATE COLT-TRAN-CD
               WHEN 'PR'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'CL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'LT'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'WO'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'RF'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE FEE SCHED TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3400-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-SCHED-RATE(1) TO COLM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF WS-WORK-AMT NOT < WS-FEE-SCHED-LOW(WS-TX)
                   MOVE WS-FEE-SCHED-RATE(WS-TX) TO COLM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF COLM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO COLM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR QUEUE CD:
      * DL -> ER WHEN NEXT REVIEW DATE IS PAST
      * ER -> HA WHEN NEXT REVIEW DATE IS PAST
      * HA -> HW WHEN NEXT REVIEW DATE IS PAST
      * HW -> ME WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3410-SET-QUEUE.
           EVALUATE COLM-QUEUE-CD
               WHEN 'DL'
                   IF COLM-NEXT-REVIEW-DATE > 0 AND
                       COLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'ER' TO COLM-QUEUE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ER'
                   IF COLM-NEXT-REVIEW-DATE > 0 AND
                       COLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'HA' TO COLM-QUEUE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HA'
                   IF COLM-NEXT-REVIEW-DATE > 0 AND
                       COLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'HW' TO COLM-QUEUE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HW'
                   IF COLM-NEXT-REVIEW-DATE > 0 AND
                       COLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'ME' TO COLM-QUEUE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WRITEOFF AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3420-FLAG-WRITEOFF.
           IF COLM-WRITEOFF-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE COLM-WRITEOFF-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAST DUE AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3430-ACCRUE-PAST-DUE.
           IF COLT-PAST-DUE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLT-PAST-DUE-AMT * COLM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO COLM-PAST-DUE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FEE FEE ON THE WRITEOFF AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3440-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN COLM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO COLT-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLT-REVERSAL-FLAG = 'Y'
                   COMPUTE COLT-FEE-AMT ROUNDED = COLT-WRITEOFF-AMT *
                       COLM-PENALTY-PCT / 200
               WHEN COLM-STATUS-CD = 'PS'
                   MOVE 12.00 TO COLT-FEE-AMT
               WHEN COLM-STATUS-CD = 'UR'
                   MOVE 5.00 TO COLT-FEE-AMT
               WHEN OTHER
                   COMPUTE COLT-FEE-AMT ROUNDED = COLT-WRITEOFF-AMT *
                       COLM-PENALTY-PCT / 100
           END-EVALUATE
           IF COLT-FEE-AMT > 0 AND COLT-FEE-AMT < 5.00
               MOVE 5.00 TO COLT-FEE-AMT
           END-IF
           IF COLT-FEE-AMT > 25.00
               MOVE 25.00 TO COLT-FEE-AMT
           END-IF
           ADD COLT-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT PAST DUE YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3450-SPLIT-PAST-DUE.
           COMPUTE WS-WORK-AMT = COLM-PAST-DUE-YTD / 2
           COMPUTE WS-WORK-AMT-2 = COLM-PAST-DUE-YTD - (WS-WORK-AMT * 2)
           COMPUTE COLM-PROMISED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * PR -> CL WHEN LAST MAINT DATE IS PAST
      * CL -> LT WHEN LAST MAINT DATE IS PAST
      * LT -> WO WHEN LAST MAINT DATE IS PAST
      * WO -> RF WHEN LAST MAINT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3460-SET-TRAN.
           EVALUATE COLT-TRAN-CD
               WHEN 'PR'
                   IF COLM-LAST-MAINT-DATE > 0 AND COLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'CL' TO COLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CL'
                   IF COLM-LAST-MAINT-DATE > 0 AND COLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'LT' TO COLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LT'
                   IF COLM-LAST-MAINT-DATE > 0 AND COLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'WO' TO COLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WO'
                   IF COLM-LAST-MAINT-DATE > 0 AND COLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'RF' TO COLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE WRITEOFF SCHED TIER FOR THE PROMISED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3470-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-WRITEOFF-SCHED-RATE(1) TO COLM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF COLM-PROMISED-AMT NOT < WS-WRITEOFF-SCHED-LOW(WS-TX)
                   MOVE WS-WRITEOFF-SCHED-RATE(WS-TX) TO COLM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF COLM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO COLM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE COLLECTED AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3480-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = COLT-CHANNEL-CD
                   ADD COLM-COLLECTED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE COLT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE COLM-COLLECTED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3490-DESCRIBE-CYCLE.
           EVALUATE COLM-CYCLE-CD
               WHEN 'BE'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'GV'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'JM'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'LC'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'MK'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'MS'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'WW'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE QUEUE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3500-DESCRIBE-QUEUE.
           EVALUATE COLM-QUEUE-CD
               WHEN 'DL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'ER'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'HA'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'HW'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'ME'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'PX'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'ZY'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 90 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3510-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF COLM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO COLM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * PS -> UR WHEN OPEN DATE IS PAST
      * UR -> XM WHEN OPEN DATE IS PAST
      * XM -> YA WHEN OPEN DATE IS PAST
      * YA -> PS WHEN OPEN DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3520-SET-STATUS.
           EVALUATE COLM-STATUS-CD
               WHEN 'PS'
                   IF COLM-OPEN-DATE > 0 AND COLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UR' TO COLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UR'
                   IF COLM-OPEN-DATE > 0 AND COLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XM' TO COLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XM'
                   IF COLM-OPEN-DATE > 0 AND COLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YA' TO COLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YA'
                   IF COLM-OPEN-DATE > 0 AND COLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PS' TO COLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3530-DESCRIBE-REGION.
           EVALUATE COLM-REGION-CD
               WHEN 'AT'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'AU'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'XR'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'XU'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * COLLECTED AMT IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3540-LIMIT-COLLECTED.
           IF COLT-COLLECTED-AMT < 50.00
               MOVE 50.00 TO COLT-COLLECTED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF COLT-COLLECTED-AMT > 5000.00
               MOVE 5000.00 TO COLT-COLLECTED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAST DUE AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3550-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = COLM-CYCLE-CD
                   ADD COLT-PAST-DUE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE COLM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE COLT-PAST-DUE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PROMISED AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3560-ACCRUE-PROMISED.
           IF COLM-PROMISED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLM-PROMISED-AMT * COLM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO COLM-PROMISED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE COLLECTED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FEE YTD.
      *----------------------------------------------------------------*
       3570-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE COLM-FEE-YTD ROUNDED =
                   COLM-COLLECTED-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO COLM-FEE-YTD
           END-IF
           ADD COLM-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3580-ACCRUE-FEE.
           IF COLT-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLT-FEE-AMT * COLM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO COLT-PAST-DUE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3590-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLM-FEE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3600-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 25.00
               MOVE 25.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 2500.00
               MOVE 2500.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT COLLECTED YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3610-SPLIT-COLLECTED.
           COMPUTE WS-WORK-AMT = COLM-COLLECTED-YTD / 2
           COMPUTE WS-WORK-AMT-2 = COLM-COLLECTED-YTD - (WS-WORK-AMT *
               2)
           COMPUTE COLM-PROMISED-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE QUEUE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3620-DESCRIBE-QUEUE.
           EVALUATE COLM-QUEUE-CD
               WHEN 'DL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ER'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'HA'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'HW'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'ME'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'PX'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ZY'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WRITEOFF AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3630-ACCRUE-WRITEOFF.
           IF COLM-WRITEOFF-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLM-WRITEOFF-AMT * COLM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO COLM-COLLECTED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3640-FLAG-FEE.
           IF COLT-FEE-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE COLT-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3650-COUNT-ITEM.
           IF COLM-ITEM-CNT < 9999999
               ADD 1 TO COLM-ITEM-CNT
           END-IF
           IF COLM-ITEM-CNT NOT < 3
               MOVE 'Y' TO COLM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * PR -> CL WHEN POST DATE IS PAST
      * CL -> LT WHEN POST DATE IS PAST
      * LT -> WO WHEN POST DATE IS PAST
      * WO -> RF WHEN POST DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3660-SET-TRAN.
           EVALUATE COLT-TRAN-CD
               WHEN 'PR'
                   IF COLT-POST-DATE > 0 AND COLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CL' TO COLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CL'
                   IF COLT-POST-DATE > 0 AND COLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LT' TO COLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LT'
                   IF COLT-POST-DATE > 0 AND COLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WO' TO COLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WO'
                   IF COLT-POST-DATE > 0 AND COLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RF' TO COLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE PAST DUE AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3670-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN COLM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLT-REVERSAL-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = COLM-PAST-DUE-AMT *
                       COLM-PENALTY-PCT / 200
               WHEN COLM-ACTION-CD = 'KJ'
                   MOVE 15.00 TO WS-WORK-AMT
               WHEN COLM-ACTION-CD = 'TM'
                   MOVE 5.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = COLM-PAST-DUE-AMT *
                       COLM-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 1.00
               MOVE 1.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 25.00
               MOVE 25.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PAST DUE AMT.
      *----------------------------------------------------------------*
       3680-PRORATE-PAST-DUE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE COLM-PAST-DUE-AMT ROUNDED =
                   COLM-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO COLM-PAST-DUE-AMT
           END-IF
           ADD COLM-PAST-DUE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FEE YTD IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3690-LIMIT-FEE.
           IF COLM-FEE-YTD < 0.00
               MOVE 0.00 TO COLM-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF COLM-FEE-YTD > 10000.00
               MOVE 10000.00 TO COLM-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E476 CONTACT REQUIRED
      * E797 PAST DUE AMT NEGATIVE
      * E931 PAST DUE AMT OVER 50000
      * E505 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3700-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN COLM-CONTACT = SPACES
                   MOVE 'E476' TO WS-REJECT-CODE
               WHEN COLM-PAST-DUE-AMT < 0
                   MOVE 'E797' TO WS-REJECT-CODE
               WHEN COLM-PAST-DUE-AMT > 50000.00
                   MOVE 'E931' TO WS-REJECT-CODE
               WHEN COLM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E505' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * AT -> BJ WHEN NEXT REVIEW DATE IS PAST
      * BJ -> GG WHEN NEXT REVIEW DATE IS PAST
      * GG -> ZR WHEN NEXT REVIEW DATE IS PAST
      * ZR -> AT WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3710-SET-REASON.
           EVALUATE COLM-REASON-CD
               WHEN 'AT'
                   IF COLM-NEXT-REVIEW-DATE > 0 AND
                       COLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'BJ' TO COLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BJ'
                   IF COLM-NEXT-REVIEW-DATE > 0 AND
                       COLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'GG' TO COLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GG'
                   IF COLM-NEXT-REVIEW-DATE > 0 AND
                       COLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'ZR' TO COLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZR'
                   IF COLM-NEXT-REVIEW-DATE > 0 AND
                       COLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'AT' TO COLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAST DUE YTD IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3720-LIMIT-PAST-DUE.
           IF COLM-PAST-DUE-YTD < 100.00
               MOVE 100.00 TO COLM-PAST-DUE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF COLM-PAST-DUE-YTD > 50000.00
               MOVE 50000.00 TO COLM-PAST-DUE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3730-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = COLT-TRAN-CD
                   ADD COLT-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE COLT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE COLT-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E534 OPERATOR REQUIRED
      * E927 WORK AMT 2 NEGATIVE
      * E650 WORK AMT 2 OVER 250000
      * E114 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3740-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN COLT-OPERATOR = SPACES
                   MOVE 'E534' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E927' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 250000.00
                   MOVE 'E650' TO WS-REJECT-CODE
               WHEN COLM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E114' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WRITEOFF YTD IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3750-LIMIT-WRITEOFF.
           IF COLM-WRITEOFF-YTD < 10.00
               MOVE 10.00 TO COLM-WRITEOFF-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF COLM-WRITEOFF-YTD > 5000.00
               MOVE 5000.00 TO COLM-WRITEOFF-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FEE FEE ON THE PAST DUE YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3760-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN COLM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO COLM-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLM-OVERRIDE-FLAG = 'Y'
                   COMPUTE COLM-FEE-AMT ROUNDED = COLM-PAST-DUE-YTD *
                       COLM-MARGIN / 200
               WHEN COLM-REGION-CD = 'AT'
                   MOVE 3.00 TO COLM-FEE-AMT
               WHEN COLM-REGION-CD = 'AU'
                   MOVE 3.00 TO COLM-FEE-AMT
               WHEN OTHER
                   COMPUTE COLM-FEE-AMT ROUNDED = COLM-PAST-DUE-YTD *
                       COLM-MARGIN / 100
           END-EVALUATE
           IF COLM-FEE-AMT > 0 AND COLM-FEE-AMT < 10.00
               MOVE 10.00 TO COLM-FEE-AMT
           END-IF
           IF COLM-FEE-AMT > 250.00
               MOVE 250.00 TO COLM-FEE-AMT
           END-IF
           ADD COLM-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE QUEUE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3770-DESCRIBE-QUEUE.
           EVALUATE COLM-QUEUE-CD
               WHEN 'DL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ER'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'HA'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'HW'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ME'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'PX'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'ZY'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE COLLECTED YTD BY QUEUE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3780-ACCUM-QUEUE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = COLM-QUEUE-CD
                   ADD COLM-COLLECTED-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE COLM-QUEUE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE COLM-COLLECTED-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PROMISED AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3790-SPLIT-PROMISED.
           COMPUTE WS-WORK-AMT = COLM-PROMISED-AMT / 12
           COMPUTE WS-WORK-AMT-2 = COLM-PROMISED-AMT - (WS-WORK-AMT *
               12)
           COMPUTE COLT-PAST-DUE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WRITEOFF FEE ON THE FEE YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3800-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN COLM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO COLM-WRITEOFF-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLM-PREFERRED-FLAG = 'Y'
                   COMPUTE COLM-WRITEOFF-AMT ROUNDED = COLM-FEE-YTD *
                       COLM-FEE-PCT / 200
               WHEN COLT-TRAN-CD = 'PR'
                   MOVE 20.00 TO COLM-WRITEOFF-AMT
               WHEN COLT-TRAN-CD = 'CL'
                   MOVE 12.00 TO COLM-WRITEOFF-AMT
               WHEN OTHER
                   COMPUTE COLM-WRITEOFF-AMT ROUNDED = COLM-FEE-YTD *
                       COLM-FEE-PCT / 100
           END-EVALUATE
           IF COLM-WRITEOFF-AMT > 0 AND COLM-WRITEOFF-AMT < 10.00
               MOVE 10.00 TO COLM-WRITEOFF-AMT
           END-IF
           IF COLM-WRITEOFF-AMT > 75.00
               MOVE 75.00 TO COLM-WRITEOFF-AMT
           END-IF
           ADD COLM-WRITEOFF-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE WRITEOFF SCHEDX TIER FOR THE PROMISED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3810-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-WRITEOFF-SCHEDX-RATE(1) TO COLM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF COLM-PROMISED-YTD NOT < WS-WRITEOFF-SCHEDX-LOW(WS-TX)
                   MOVE WS-WRITEOFF-SCHEDX-RATE(WS-TX) TO COLM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF COLM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO COLM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAST DUE AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3820-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = COLM-CYCLE-CD
                   ADD COLT-PAST-DUE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE COLM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE COLT-PAST-DUE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * WRITEOFF YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3830-FLAG-WRITEOFF.
           IF COLM-WRITEOFF-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE COLM-WRITEOFF-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FEE YTD IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3840-LIMIT-FEE.
           IF COLM-FEE-YTD < 10.00
               MOVE 10.00 TO COLM-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF COLM-FEE-YTD > 50000.00
               MOVE 50000.00 TO COLM-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PROMISED AMT IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3850-LIMIT-PROMISED.
           IF COLM-PROMISED-AMT < 50.00
               MOVE 50.00 TO COLM-PROMISED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF COLM-PROMISED-AMT > 5000.00
               MOVE 5000.00 TO COLM-PROMISED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PROMISED YTD IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3860-LIMIT-PROMISED.
           IF COLM-PROMISED-YTD < 0.00
               MOVE 0.00 TO COLM-PROMISED-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF COLM-PROMISED-YTD > 2500.00
               MOVE 2500.00 TO COLM-PROMISED-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAST DUE FEE ON THE PAST DUE AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3870-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN COLM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO COLM-PAST-DUE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLM-OVERRIDE-FLAG = 'Y'
                   COMPUTE COLM-PAST-DUE-AMT ROUNDED = COLT-PAST-DUE-AMT
                       * COLM-FEE-PCT / 200
               WHEN COLM-QUEUE-CD = 'DL'
                   MOVE 10.00 TO COLM-PAST-DUE-AMT
               WHEN COLM-QUEUE-CD = 'ER'
                   MOVE 12.00 TO COLM-PAST-DUE-AMT
               WHEN OTHER
                   COMPUTE COLM-PAST-DUE-AMT ROUNDED = COLT-PAST-DUE-AMT
                       * COLM-FEE-PCT / 100
           END-EVALUATE
           IF COLM-PAST-DUE-AMT > 0 AND COLM-PAST-DUE-AMT < 1.00
               MOVE 1.00 TO COLM-PAST-DUE-AMT
           END-IF
           IF COLM-PAST-DUE-AMT > 35.00
               MOVE 35.00 TO COLM-PAST-DUE-AMT
           END-IF
           ADD COLM-PAST-DUE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3880-COUNT-RETRY.
           IF COLM-RETRY-CNT < 9999999
               ADD 1 TO COLM-RETRY-CNT
           END-IF
           IF COLM-RETRY-CNT NOT < 5
               MOVE 'Y' TO COLM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE COLLECTED YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3890-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = COLM-STATUS-CD
                   ADD COLM-COLLECTED-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE COLM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE COLM-COLLECTED-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAST DUE YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3900-ACCRUE-PAST-DUE.
           IF COLM-PAST-DUE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLM-PAST-DUE-YTD * COLM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAST DUE YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3910-ACCRUE-PAST-DUE.
           IF COLM-PAST-DUE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLM-PAST-DUE-YTD * COLM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3920-DESCRIBE-TRAN.
           EVALUATE COLT-TRAN-CD
               WHEN 'PR'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'CL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'LT'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'WO'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'RF'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON COLLECTED YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3930-ACCRUE-COLLECTED.
           IF COLM-COLLECTED-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLM-COLLECTED-YTD * COLM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO COLM-COLLECTED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAST DUE YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3940-FLAG-PAST-DUE.
           IF COLM-PAST-DUE-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE COLM-PAST-DUE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3950-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = COLM-FEE-YTD / 3
           COMPUTE WS-WORK-AMT-2 = COLM-FEE-YTD - (WS-WORK-AMT * 3)
           COMPUTE COLM-PAST-DUE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3960-CHECK-NEXT-DUE.
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
           IF WS-DAYS > 30
               MOVE 'Y' TO COLM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FEE YTD.
      *----------------------------------------------------------------*
       3970-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE COLM-FEE-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO COLM-FEE-YTD
           END-IF
           ADD COLM-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE WRITEOFF SCHEDX TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3980-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-WRITEOFF-SCHEDX-RATE(1) TO COLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF WS-WORK-AMT NOT < WS-WRITEOFF-SCHEDX-LOW(WS-TX)
                   MOVE WS-WRITEOFF-SCHEDX-RATE(WS-TX) TO COLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF COLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO COLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PROMISED FEE ON THE WORK AMT 2.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3990-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN COLM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO COLM-PROMISED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLM-REVIEW-FLAG = 'Y'
                   COMPUTE COLM-PROMISED-AMT ROUNDED = WS-WORK-AMT-2 *
                       COLM-MARGIN / 200
               WHEN COLM-ACTION-CD = 'KJ'
                   MOVE 7.00 TO COLM-PROMISED-AMT
               WHEN COLM-ACTION-CD = 'TM'
                   MOVE 15.00 TO COLM-PROMISED-AMT
               WHEN OTHER
                   COMPUTE COLM-PROMISED-AMT ROUNDED = WS-WORK-AMT-2 *
                       COLM-MARGIN / 100
           END-EVALUATE
           IF COLM-PROMISED-AMT > 0 AND COLM-PROMISED-AMT < 1.00
               MOVE 1.00 TO COLM-PROMISED-AMT
           END-IF
           IF COLM-PROMISED-AMT > 100.00
               MOVE 100.00 TO COLM-PROMISED-AMT
           END-IF
           ADD COLM-PROMISED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 120 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4000-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF COLT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO COLM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PROMISED YTD IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4010-LIMIT-PROMISED.
           IF COLM-PROMISED-YTD < 100.00
               MOVE 100.00 TO COLM-PROMISED-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF COLM-PROMISED-YTD > 10000.00
               MOVE 10000.00 TO COLM-PROMISED-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WRITEOFF FEE ON THE COLLECTED AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4020-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN COLM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO COLT-WRITEOFF-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLM-OVERRIDE-FLAG = 'Y'
                   COMPUTE COLT-WRITEOFF-AMT ROUNDED =
                       COLM-COLLECTED-AMT * COLM-MARGIN / 200
               WHEN COLM-QUEUE-CD = 'DL'
                   MOVE 20.00 TO COLT-WRITEOFF-AMT
               WHEN COLM-QUEUE-CD = 'ER'
                   MOVE 12.00 TO COLT-WRITEOFF-AMT
               WHEN OTHER
                   COMPUTE COLT-WRITEOFF-AMT ROUNDED =
                       COLM-COLLECTED-AMT * COLM-MARGIN / 100
           END-EVALUATE
           IF COLT-WRITEOFF-AMT > 0 AND COLT-WRITEOFF-AMT < 1.00
               MOVE 1.00 TO COLT-WRITEOFF-AMT
           END-IF
           IF COLT-WRITEOFF-AMT > 35.00
               MOVE 35.00 TO COLT-WRITEOFF-AMT
           END-IF
           ADD COLT-WRITEOFF-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT FEE YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4030-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = COLM-FEE-YTD / 4
           COMPUTE WS-WORK-AMT-2 = COLM-FEE-YTD - (WS-WORK-AMT * 4)
           COMPUTE COLM-PAST-DUE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE COLLECTED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO COLLECTED YTD.
      *----------------------------------------------------------------*
       4040-PRORATE-COLLECTED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE COLM-COLLECTED-YTD ROUNDED =
                   COLT-COLLECTED-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO COLM-COLLECTED-YTD
           END-IF
           ADD COLM-COLLECTED-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * COLLECTED AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4050-FLAG-COLLECTED.
           IF COLT-COLLECTED-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE COLT-COLLECTED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4060-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * COLM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO COLT-WRITEOFF-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR QUEUE CD:
      * DL -> ER WHEN EFF DATE IS PAST
      * ER -> HA WHEN EFF DATE IS PAST
      * HA -> HW WHEN EFF DATE IS PAST
      * HW -> ME WHEN EFF DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4070-SET-QUEUE.
           EVALUATE COLM-QUEUE-CD
               WHEN 'DL'
                   IF COLT-EFF-DATE > 0 AND COLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ER' TO COLM-QUEUE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ER'
                   IF COLT-EFF-DATE > 0 AND COLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HA' TO COLM-QUEUE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HA'
                   IF COLT-EFF-DATE > 0 AND COLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HW' TO COLM-QUEUE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HW'
                   IF COLT-EFF-DATE > 0 AND COLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ME' TO COLM-QUEUE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE COLLECTED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       4080-PRORATE-COLLECTED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   COLM-COLLECTED-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4090-SPLIT-COLLECTED.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 12)
           COMPUTE COLM-COLLECTED-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE WRITEOFF SCHEDX TIER FOR THE PAST DUE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4100-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-WRITEOFF-SCHEDX-RATE(1) TO COLM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF COLM-PAST-DUE-AMT NOT < WS-WRITEOFF-SCHEDX-LOW(WS-TX)
                   MOVE WS-WRITEOFF-SCHEDX-RATE(WS-TX) TO COLM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF COLM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO COLM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE YTD BY QUEUE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4110-ACCUM-QUEUE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = COLM-QUEUE-CD
                   ADD COLM-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE COLM-QUEUE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE COLM-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       4120-COUNT-RETRY.
           IF COLM-RETRY-CNT < 9999999
               ADD 1 TO COLM-RETRY-CNT
           END-IF
           IF COLM-RETRY-CNT NOT < 6
               MOVE 'Y' TO COLM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4130-COUNT-SEQ.
           IF COLT-SEQ < 9999999
               ADD 1 TO COLT-SEQ
           END-IF
           IF COLT-SEQ NOT < 5
               MOVE 'Y' TO COLM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4140-AGE-POST.
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
               WHEN WS-DAYS NOT > 30
                   ADD COLM-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD COLM-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD COLM-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLM-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E631 REF NO REQUIRED
      * E014 WORK AMT NEGATIVE
      * E230 WORK AMT OVER 25000
      * E808 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       4150-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN COLM-REF-NO = SPACES
                   MOVE 'E631' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E014' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 25000.00
                   MOVE 'E230' TO WS-REJECT-CODE
               WHEN COLM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E808' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4160-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF COLM-CLOSE-DATE > 0 AND COLM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-CLOSE-DATE TO DTU-DATE-1
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
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE PROMISED YTD BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4170-ACCUM-CYCLE.
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
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       4180-COUNT-SEQ.
           IF COLT-SEQ < 9999999
               ADD 1 TO COLT-SEQ
           END-IF
           IF COLT-SEQ NOT < 5
               MOVE 'Y' TO COLM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE WRITEOFF SCHED TIER FOR THE PROMISED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4190-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-WRITEOFF-SCHED-RATE(1) TO COLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF COLM-PROMISED-YTD NOT < WS-WRITEOFF-SCHED-LOW(WS-TX)
                   MOVE WS-WRITEOFF-SCHED-RATE(WS-TX) TO COLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF COLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO COLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE WRITEOFF SCHED TIER FOR THE COLLECTED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4200-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-WRITEOFF-SCHED-RATE(1) TO COLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF COLM-COLLECTED-YTD NOT < WS-WRITEOFF-SCHED-LOW(WS-TX)
                   MOVE WS-WRITEOFF-SCHED-RATE(WS-TX) TO COLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF COLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO COLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT COLLECTED AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4210-SPLIT-COLLECTED.
           COMPUTE WS-WORK-AMT = COLT-COLLECTED-AMT / 12
           COMPUTE WS-WORK-AMT-2 = COLT-COLLECTED-AMT - (WS-WORK-AMT *
               12)
           COMPUTE COLM-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       4220-COUNT-ITEM.
           IF COLM-ITEM-CNT < 9999999
               ADD 1 TO COLM-ITEM-CNT
           END-IF
           IF COLM-ITEM-CNT NOT < 6
               MOVE 'Y' TO COLM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4230-DESCRIBE-REASON.
           EVALUATE COLM-REASON-CD
               WHEN 'AT'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'BJ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'GG'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'ZR'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE QUEUE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4240-DESCRIBE-QUEUE.
           EVALUATE COLM-QUEUE-CD
               WHEN 'DL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'ER'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'HA'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'HW'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ME'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'PX'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'ZY'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT COLLECTED AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4250-SPLIT-COLLECTED.
           COMPUTE WS-WORK-AMT = COLT-COLLECTED-AMT / 2
           COMPUTE WS-WORK-AMT-2 = COLT-COLLECTED-AMT - (WS-WORK-AMT *
               2)
           COMPUTE COLM-WRITEOFF-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE FEE YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4260-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN COLM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLM-SUSPEND-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = COLM-FEE-YTD *
                       COLM-PENALTY-PCT / 200
               WHEN COLT-TRAN-CD = 'PR'
                   MOVE 20.00 TO WS-WORK-AMT
               WHEN COLT-TRAN-CD = 'CL'
                   MOVE 7.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = COLM-FEE-YTD *
                       COLM-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 5.00
               MOVE 5.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 100.00
               MOVE 100.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4270-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF COLM-NEXT-DUE-DATE > 0 AND COLM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD COLM-WRITEOFF-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD COLM-WRITEOFF-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD COLM-WRITEOFF-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD COLM-WRITEOFF-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLM-WRITEOFF-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4280-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF COLM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO COLM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4290-SPLIT-COLLECTED.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 3)
           COMPUTE COLM-COLLECTED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * PR -> CL WHEN NOTICE DATE IS PAST
      * CL -> LT WHEN NOTICE DATE IS PAST
      * LT -> WO WHEN NOTICE DATE IS PAST
      * WO -> RF WHEN NOTICE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4300-SET-TRAN.
           EVALUATE COLT-TRAN-CD
               WHEN 'PR'
                   IF COLM-NOTICE-DATE > 0 AND COLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CL' TO COLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CL'
                   IF COLM-NOTICE-DATE > 0 AND COLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LT' TO COLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LT'
                   IF COLM-NOTICE-DATE > 0 AND COLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WO' TO COLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WO'
                   IF COLM-NOTICE-DATE > 0 AND COLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RF' TO COLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE PROMISED YTD FEE ON THE WRITEOFF AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4310-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN COLM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO COLM-PROMISED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLM-EXEMPT-FLAG = 'Y'
                   COMPUTE COLM-PROMISED-YTD ROUNDED = COLT-WRITEOFF-AMT
                       * COLM-FEE-PCT / 200
               WHEN COLM-STATUS-CD = 'PS'
                   MOVE 20.00 TO COLM-PROMISED-YTD
               WHEN COLM-STATUS-CD = 'UR'
                   MOVE 5.00 TO COLM-PROMISED-YTD
               WHEN OTHER
                   COMPUTE COLM-PROMISED-YTD ROUNDED = COLT-WRITEOFF-AMT
                       * COLM-FEE-PCT / 100
           END-EVALUATE
           IF COLM-PROMISED-YTD > 0 AND COLM-PROMISED-YTD < 10.00
               MOVE 10.00 TO COLM-PROMISED-YTD
           END-IF
           IF COLM-PROMISED-YTD > 35.00
               MOVE 35.00 TO COLM-PROMISED-YTD
           END-IF
           ADD COLM-PROMISED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON COLLECTED YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4320-ACCRUE-COLLECTED.
           IF COLM-COLLECTED-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLM-COLLECTED-YTD * COLM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO COLM-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4330-SPLIT-WRITEOFF.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 12)
           COMPUTE COLT-WRITEOFF-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4340-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = COLT-CHANNEL-CD
                   ADD COLT-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE COLT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE COLT-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PROMISED GRID TIER FOR THE COLLECTED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4350-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PROMISED-GRID-RATE(1) TO COLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF COLT-COLLECTED-AMT NOT < WS-PROMISED-GRID-LOW(WS-TX)
                   MOVE WS-PROMISED-GRID-RATE(WS-TX) TO COLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF COLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO COLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FEE FEE ON THE FEE AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4360-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN COLT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO COLM-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLM-SUSPEND-FLAG = 'Y'
                   COMPUTE COLM-FEE-AMT ROUNDED = COLT-FEE-AMT *
                       COLM-MARGIN / 200
               WHEN COLM-ACTION-CD = 'KJ'
                   MOVE 12.00 TO COLM-FEE-AMT
               WHEN COLM-ACTION-CD = 'TM'
                   MOVE 10.00 TO COLM-FEE-AMT
               WHEN OTHER
                   COMPUTE COLM-FEE-AMT ROUNDED = COLT-FEE-AMT *
                       COLM-MARGIN / 100
           END-EVALUATE
           IF COLM-FEE-AMT > 0 AND COLM-FEE-AMT < 5.00
               MOVE 5.00 TO COLM-FEE-AMT
           END-IF
           IF COLM-FEE-AMT > 35.00
               MOVE 35.00 TO COLM-FEE-AMT
           END-IF
           ADD COLM-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4370-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF COLM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-CLOSE-DATE TO DTU-DATE-1
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
      * PRORATE COLLECTED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FEE AMT.
      *----------------------------------------------------------------*
       4380-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE COLT-FEE-AMT ROUNDED =
                   COLM-COLLECTED-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO COLT-FEE-AMT
           END-IF
           ADD COLT-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 180 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4390-CHECK-EFF.
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
               MOVE 'Y' TO COLM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN NEXT DUE DATE IS PAST
      * BAT -> MAL WHEN NEXT DUE DATE IS PAST
      * MAL -> TEL WHEN NEXT DUE DATE IS PAST
      * TEL -> ONL WHEN NEXT DUE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4400-SET-CHANNEL.
           EVALUATE COLT-CHANNEL-CD
               WHEN 'BRN'
                   IF COLM-NEXT-DUE-DATE > 0 AND COLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO COLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF COLM-NEXT-DUE-DATE > 0 AND COLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO COLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF COLM-NEXT-DUE-DATE > 0 AND COLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO COLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF COLM-NEXT-DUE-DATE > 0 AND COLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO COLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE PROMISED YTD FEE ON THE COLLECTED AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4410-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN COLM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO COLM-PROMISED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLM-REVIEW-FLAG = 'Y'
                   COMPUTE COLM-PROMISED-YTD ROUNDED =
                       COLT-COLLECTED-AMT * COLM-MARGIN / 200
               WHEN COLM-QUEUE-CD = 'DL'
                   MOVE 7.00 TO COLM-PROMISED-YTD
               WHEN COLM-QUEUE-CD = 'ER'
                   MOVE 20.00 TO COLM-PROMISED-YTD
               WHEN OTHER
                   COMPUTE COLM-PROMISED-YTD ROUNDED =
                       COLT-COLLECTED-AMT * COLM-MARGIN / 100
           END-EVALUATE
           IF COLM-PROMISED-YTD > 0 AND COLM-PROMISED-YTD < 2.00
               MOVE 2.00 TO COLM-PROMISED-YTD
           END-IF
           IF COLM-PROMISED-YTD > 25.00
               MOVE 25.00 TO COLM-PROMISED-YTD
           END-IF
           ADD COLM-PROMISED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE ACTION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4420-DESCRIBE-ACTION.
           EVALUATE COLM-ACTION-CD
               WHEN 'KJ'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'TM'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'VL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'ZS'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E660 CUST ID REQUIRED
      * E176 PAST DUE YTD NEGATIVE
      * E075 PAST DUE YTD OVER 250000
      * E187 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       4430-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN COLM-CUST-ID = SPACES
                   MOVE 'E660' TO WS-REJECT-CODE
               WHEN COLM-PAST-DUE-YTD < 0
                   MOVE 'E176' TO WS-REJECT-CODE
               WHEN COLM-PAST-DUE-YTD > 250000.00
                   MOVE 'E075' TO WS-REJECT-CODE
               WHEN COLM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E187' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E945 OPERATOR REQUIRED
      * E973 WORK AMT NEGATIVE
      * E151 WORK AMT OVER 99999
      * E726 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       4440-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN COLT-OPERATOR = SPACES
                   MOVE 'E945' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E973' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 99999.00
                   MOVE 'E151' TO WS-REJECT-CODE
               WHEN COLM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E726' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

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
                   MOVE CLLIM-PAST-DUE-AMT
                     TO COLM-PAST-DUE-AMT
                   MOVE CLLIM-WRITEOFF-YTD
                     TO COLM-WRITEOFF-YTD
                   MOVE CLLIM-OVERRIDE-FLAG
                     TO COLM-OVERRIDE-FLAG
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
           DISPLAY 'CLLCALC READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'CLLCALC' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
