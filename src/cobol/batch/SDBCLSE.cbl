      *================================================================*
      * PROGRAM  : SDBCLSE                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : SAFE DEPOSIT BOXES - CLOSURE PROCESSING. READS THE  *
      *            SDB TRANSACTIONS, UPDATES THE SDB MASTER AND        *
      *            PRODUCES THE CONTROL REPORT. SAFE DEPOSIT BOX RENTAL*
      *            BILLING, ACCESS LOG AND DRILLING                    *
      * INPUT    : SDBMST (KSDS), SDBTRN, BANKCTL                      *
      * OUTPUT   : SDBOUT, SDBCLRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, SDLIM, SDRISK           *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1993-06-04 TWH  ORIGINAL                                       *
      * 2004-05-09 PQR  AUDIT FINDING - DRILL CONTROL TOTALS CHG7113   *
      * 2004-03-05 MKL  AUDIT FINDING - LATE-FEE CONTROL TOTALS CHG0481*
      * 2007-06-14 TWH  DEPOSIT NOTICE TEXT PER LEGAL INC5228          *
      * 2008-11-20 JLM  PERFORMANCE - DEPOSIT READ AHEAD INC6139       *
      * 2009-03-04 GAF  DRILL EXCEPTIONS TO OPS QUEUE REQ8504          *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SDBCLSE.
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
           SELECT REPORT-FILE ASSIGN TO "SDBCLRPT"
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
                                           VALUE 10.000000.
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
      * WS-RENT-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-RENT-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000678500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000290000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000550000000778000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010550000000865000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010650000000640500'.
       01  WS-RENT-SCHED-TBL REDEFINES WS-RENT-SCHED-VALUES.
           05  WS-RENT-SCHED-ROW OCCURS 5 TIMES.
               10  WS-RENT-SCHED-LOW       PIC 9(09)V99.
               10  WS-RENT-SCHED-RATE      PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-LATE-FEE-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-LATE-FEE-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000107000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000076000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000075500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001100000000607000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001200000000463000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001300000000367500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011300000000053000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011550000000077000'.
       01  WS-LATE-FEE-BAND-TBL REDEFINES WS-LATE-FEE-BAND-VALUES.
           05  WS-LATE-FEE-BAND-ROW OCCURS 8 TIMES.
               10  WS-LATE-FEE-BAND-LOW    PIC 9(09)V99.
               10  WS-LATE-FEE-BAND-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-KEY-FEE-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-KEY-FEE-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000172500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000067000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001100000000081000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003600000000228500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006100000000827000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016100000000689000'.
       01  WS-KEY-FEE-GRID-TBL REDEFINES WS-KEY-FEE-GRID-VALUES.
           05  WS-KEY-FEE-GRID-ROW OCCURS 6 TIMES.
               10  WS-KEY-FEE-GRID-LOW     PIC 9(09)V99.
               10  WS-KEY-FEE-GRID-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-PREPAID-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PREPAID-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000380500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000237000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000026500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000350000000782500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000600000000628500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000700000000308500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003200000000466000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003250000000716500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003300000000797500'.
       01  WS-PREPAID-BAND-TBL REDEFINES WS-PREPAID-BAND-VALUES.
           05  WS-PREPAID-BAND-ROW OCCURS 9 TIMES.
               10  WS-PREPAID-BAND-LOW     PIC 9(09)V99.
               10  WS-PREPAID-BAND-RATE    PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  SDBCLSE  SAFE DEPOSIT BOXES'.
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
       COPY SDLIMP.
       COPY SDRISKP.

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
               PERFORM 3000-SPLIT-KEY-FEE
               PERFORM 3010-CHECK-OPEN
               PERFORM 3020-EDIT-REJECT
               PERFORM 3030-SELECT-PENALTY-PCT
               PERFORM 3040-PRORATE-DRILL
               PERFORM 3050-SELECT-FEE-PCT
               PERFORM 3060-ASSESS-OVERRIDE
               PERFORM 3070-CHECK-LAST-ACT
               PERFORM 3080-LIMIT-ITEM
               PERFORM 3090-DESCRIBE-VAULT
               PERFORM 3100-ACCUM-REASON
               PERFORM 3110-CHECK-NEXT-REVIEW
               PERFORM 3120-FLAG-RENT
               PERFORM 3130-COUNT-NOTICE
               PERFORM 3140-ACCUM-CHANNEL
               PERFORM 3150-ACCRUE-FEE-PCT
               PERFORM 3160-ACCRUE-LATE-FEE
               PERFORM 3170-AGE-POST
               PERFORM 3180-CHECK-NOTICE
               PERFORM 3190-EDIT-ADDR
               PERFORM 3200-ACCRUE-PREPAID
               PERFORM 3210-ASSESS-AUTO-RENEW
               PERFORM 3220-PRORATE-DRILL
               PERFORM 3230-ASSESS-REVIEW
               PERFORM 3240-SELECT-PENALTY-PCT
               PERFORM 3250-COUNT-NOTICE
               PERFORM 3260-ACCRUE-LATE-FEE
               PERFORM 3270-AGE-POST
               PERFORM 3280-CHECK-POST
               PERFORM 3290-SPLIT-DRILL
               PERFORM 3300-ACCUM-TRAN
               PERFORM 3310-ACCUM-SOURCE
               PERFORM 3320-ACCRUE-DEPOSIT
               PERFORM 3330-SET-CYCLE
               PERFORM 3340-ASSESS-AUTO-RENEW
               PERFORM 3350-ACCRUE-KEY-FEE
               PERFORM 3360-PRORATE-RENT
               PERFORM 3370-LIMIT-DEPOSIT
               PERFORM 3380-ASSESS-PREFERRED
               PERFORM 3390-SELECT-DISCOUNT
               PERFORM 3400-CHECK-LAST-STMT
               PERFORM 3410-ACCUM-SOURCE
               PERFORM 3420-SET-VAULT
               PERFORM 3430-ASSESS-AUTO-RENEW
               PERFORM 3440-FLAG-KEY-FEE
               PERFORM 3450-ACCUM-REASON
               PERFORM 3460-ACCUM-SOURCE
               PERFORM 3470-SET-BRANCH
               PERFORM 3480-CHECK-CLOSE
               PERFORM 3490-LIMIT-DEPOSIT
               PERFORM 3500-FLAG-DRILL
               PERFORM 3510-SET-CYCLE
               PERFORM 3520-EDIT-DESC
               PERFORM 3530-PRORATE-RENT
               PERFORM 3540-SPLIT-DRILL
               PERFORM 3550-CHECK-LAST-STMT
               PERFORM 3560-ACCUM-REASON
               PERFORM 3570-CHECK-LAST-ACT
               PERFORM 3580-SET-BOX-SIZE
               PERFORM 3590-LIMIT-PREPAID
               PERFORM 3600-DESCRIBE-BRANCH
               PERFORM 3610-SELECT-DISCOUNT
               PERFORM 3620-COUNT-ITEM
               PERFORM 3630-PRORATE-DEPOSIT
               PERFORM 3640-CHECK-LAST-MAINT
               PERFORM 3650-DESCRIBE-REASON
               PERFORM 3660-PRORATE-KEY-FEE
               PERFORM 3670-SELECT-PENALTY-PCT
               PERFORM 3680-DESCRIBE-BOX-SIZE
               PERFORM 3690-DESCRIBE-CHANNEL
               PERFORM 3700-SET-TRAN
               PERFORM 3710-EDIT-REJECT
               PERFORM 3720-SELECT-PENALTY-PCT
               PERFORM 3730-PRORATE-RENT
               PERFORM 3740-SELECT-DISCOUNT
               PERFORM 3750-SELECT-PENALTY-PCT
               PERFORM 3760-AGE-LAST-MAINT
               PERFORM 3770-ACCRUE-LATE-FEE
               PERFORM 3780-SET-VAULT
               PERFORM 3790-SPLIT-RENT
               PERFORM 3800-FLAG-DEPOSIT
               PERFORM 3810-FLAG-RENT
               PERFORM 3820-EDIT-REF-NO
               PERFORM 3830-ACCRUE-DRILL
               PERFORM 3840-COUNT-ITEM
               PERFORM 3850-AGE-CLOSE
               PERFORM 3860-PRORATE-PREPAID
               PERFORM 3870-LIMIT-RENT
               PERFORM 3880-LIMIT-PREPAID
               PERFORM 3890-SET-CHANNEL
               PERFORM 3900-CHECK-OPEN
               PERFORM 3910-PRORATE-RENT
               PERFORM 3920-PRORATE-DEPOSIT
               PERFORM 3930-SELECT-PENALTY-PCT
               PERFORM 3940-COUNT-RETRY
               PERFORM 3950-AGE-CLOSE
               PERFORM 3960-ASSESS-OVERRIDE
               PERFORM 3970-CHECK-MATURITY
               PERFORM 3980-ACCRUE-LATE-FEE
               PERFORM 3990-COUNT-NOTICE
               PERFORM 4000-CHECK-NEXT-DUE
               PERFORM 4010-FLAG-DRILL
               PERFORM 4020-DESCRIBE-REASON
               PERFORM 4030-ACCRUE-LATE-FEE
               PERFORM 4040-ACCRUE-PENALTY-PCT
               PERFORM 4050-SELECT-FEE-PCT
               PERFORM 4060-AGE-NEXT-REVIEW
               PERFORM 4070-LIMIT-KEY-FEE
               PERFORM 4080-DESCRIBE-SOURCE
               PERFORM 4090-SELECT-FEE-PCT
               PERFORM 4100-FLAG-PREPAID
               PERFORM 4110-FLAG-PREPAID
               PERFORM 4120-DESCRIBE-BOX-SIZE
               PERFORM 4130-SET-TRAN
               PERFORM 4140-ACCRUE-PREPAID
               PERFORM 4150-DESCRIBE-CHANNEL
               PERFORM 4160-SPLIT-KEY-FEE
               PERFORM 4170-EDIT-MEMO
               PERFORM 4180-SPLIT-DEPOSIT
               PERFORM 4190-PRORATE-DEPOSIT
               PERFORM 4200-AGE-NEXT-DUE
               PERFORM 4210-COUNT-NOTICE
               PERFORM 4220-LIMIT-ITEM
               PERFORM 4230-EDIT-OPERATOR
               PERFORM 4240-LIMIT-RENT
               PERFORM 4250-COUNT-NOTICE
               PERFORM 4260-COUNT-SEQ
               PERFORM 4270-EDIT-OPERATOR
               PERFORM 4280-ASSESS-PREFERRED
               PERFORM 4290-FLAG-DEPOSIT
               PERFORM 4300-SPLIT-LATE-FEE
               PERFORM 4310-SET-BOX-SIZE
               PERFORM 4320-AGE-NOTICE
               PERFORM 4330-FLAG-RENT
               PERFORM 4340-AGE-NEXT-DUE
               PERFORM 4350-DESCRIBE-CYCLE
               PERFORM 4360-LIMIT-KEY-FEE
               PERFORM 4370-EDIT-DESC
               PERFORM 4380-SET-BRANCH
               PERFORM 4390-SPLIT-KEY-FEE
               PERFORM 4400-EDIT-OPERATOR
               PERFORM 4410-SET-BOX-SIZE
               PERFORM 4420-FLAG-DEPOSIT
               PERFORM 4430-DESCRIBE-TRAN
               PERFORM 4440-ACCRUE-RENT
               PERFORM 4450-FLAG-PREPAID
               PERFORM 4460-LIMIT-LATE-FEE
               PERFORM 4470-FLAG-DEPOSIT
               PERFORM 4480-CHECK-NEXT-DUE
               PERFORM 4490-COUNT-NOTICE
               PERFORM 4500-ACCUM-VAULT
               PERFORM 4510-SPLIT-RENT
               PERFORM 4520-SPLIT-KEY-FEE
               PERFORM 4530-ACCUM-CYCLE
               PERFORM 4540-FLAG-DRILL
               PERFORM 4550-EDIT-REF-NO
               PERFORM 7020-CALL-SDLIM
               PERFORM 7010-CALL-SDRISK
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
      * SPLIT KEY FEE YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3000-SPLIT-KEY-FEE.
           COMPUTE WS-WORK-AMT = SDBM-KEY-FEE-YTD / 2
           COMPUTE WS-WORK-AMT-2 = SDBM-KEY-FEE-YTD - (WS-WORK-AMT * 2)
           COMPUTE SDBT-DEPOSIT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 15 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3010-CHECK-OPEN.
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
           IF WS-DAYS > 15
               MOVE 'Y' TO SDBM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E547 REJECT CD REQUIRED
      * E986 WORK AMT NEGATIVE
      * E052 WORK AMT OVER 10000
      * E484 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3020-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDBT-REJECT-CD = SPACES
                   MOVE 'E547' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E986' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 10000.00
                   MOVE 'E052' TO WS-REJECT-CODE
               WHEN SDBT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E484' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE LATE FEE BAND TIER FOR THE PREPAID AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3030-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-LATE-FEE-BAND-RATE(1) TO SDBM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF SDBM-PREPAID-AMT NOT < WS-LATE-FEE-BAND-LOW(WS-TX)
                   MOVE WS-LATE-FEE-BAND-RATE(WS-TX) TO SDBM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE RENT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DRILL YTD.
      *----------------------------------------------------------------*
       3040-PRORATE-DRILL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDBM-DRILL-YTD ROUNDED =
                   SDBM-RENT-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SDBM-DRILL-YTD
           END-IF
           ADD SDBM-DRILL-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE PREPAID BAND TIER FOR THE RENT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3050-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PREPAID-BAND-RATE(1) TO SDBM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF SDBT-RENT-AMT NOT < WS-PREPAID-BAND-LOW(WS-TX)
                   MOVE WS-PREPAID-BAND-RATE(WS-TX) TO SDBM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE DEPOSIT AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3060-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN SDBM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDBM-REVIEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = SDBM-DEPOSIT-AMT *
                       SDBM-FEE-PCT / 200
               WHEN SDBM-BOX-SIZE-CD = 'CU'
                   MOVE 5.00 TO WS-WORK-AMT
               WHEN SDBM-BOX-SIZE-CD = 'MG'
                   MOVE 15.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = SDBM-DEPOSIT-AMT *
                       SDBM-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 25.00
               MOVE 25.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 45 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3070-CHECK-LAST-ACT.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO SDBM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3080-LIMIT-ITEM.
           IF WS-WORK-AMT < 50.00
               MOVE 50.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 5000.00
               MOVE 5000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE VAULT CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3090-DESCRIBE-VAULT.
           EVALUATE SDBM-VAULT-CD
               WHEN 'BU'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ET'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'FF'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'NG'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'NH'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE DRILL AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3100-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDBM-REASON-CD
                   ADD SDBT-DRILL-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDBM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDBT-DRILL-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 45 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3110-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF SDBM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDBM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO SDBM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RENT AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3120-FLAG-RENT.
           IF SDBT-RENT-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE SDBT-RENT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3130-COUNT-NOTICE.
           IF SDBM-NOTICE-CNT < 9999999
               ADD 1 TO SDBM-NOTICE-CNT
           END-IF
           IF SDBM-NOTICE-CNT NOT < 6
               MOVE 'Y' TO SDBM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE LATE FEE YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3140-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDBT-CHANNEL-CD
                   ADD SDBM-LATE-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDBT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDBM-LATE-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3150-ACCRUE-FEE-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * SDBM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO SDBM-KEY-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON LATE FEE AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3160-ACCRUE-LATE-FEE.
           IF SDBM-LATE-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBM-LATE-FEE-AMT * SDBM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO SDBM-PREPAID-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3170-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF SDBT-POST-DATE > 0 AND SDBT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDBT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SDBM-KEY-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD SDBM-KEY-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD SDBM-KEY-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD SDBM-KEY-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDBM-KEY-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3180-CHECK-NOTICE.
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
           IF WS-DAYS > 30
               MOVE 'Y' TO SDBT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E429 ADDR REQUIRED
      * E692 PREPAID YTD NEGATIVE
      * E080 PREPAID YTD OVER 10000
      * E309 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3190-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDBM-ADDR = SPACES
                   MOVE 'E429' TO WS-REJECT-CODE
               WHEN SDBM-PREPAID-YTD < 0
                   MOVE 'E692' TO WS-REJECT-CODE
               WHEN SDBM-PREPAID-YTD > 10000.00
                   MOVE 'E080' TO WS-REJECT-CODE
               WHEN SDBT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E309' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PREPAID AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3200-ACCRUE-PREPAID.
           IF SDBM-PREPAID-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBM-PREPAID-AMT * SDBM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO SDBM-DRILL-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RENT YTD FEE ON THE DRILL YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3210-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN SDBM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO SDBM-RENT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDBM-REVIEW-FLAG = 'Y'
                   COMPUTE SDBM-RENT-YTD ROUNDED = SDBM-DRILL-YTD *
                       SDBM-FEE-PCT / 200
               WHEN SDBM-BRANCH-CD = 'EXS'
                   MOVE 12.00 TO SDBM-RENT-YTD
               WHEN SDBM-BRANCH-CD = 'FJW'
                   MOVE 5.00 TO SDBM-RENT-YTD
               WHEN OTHER
                   COMPUTE SDBM-RENT-YTD ROUNDED = SDBM-DRILL-YTD *
                       SDBM-FEE-PCT / 100
           END-EVALUATE
           IF SDBM-RENT-YTD > 0 AND SDBM-RENT-YTD < 10.00
               MOVE 10.00 TO SDBM-RENT-YTD
           END-IF
           IF SDBM-RENT-YTD > 25.00
               MOVE 25.00 TO SDBM-RENT-YTD
           END-IF
           ADD SDBM-RENT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE RENT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO DRILL YTD.
      *----------------------------------------------------------------*
       3220-PRORATE-DRILL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDBM-DRILL-YTD ROUNDED =
                   SDBT-RENT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SDBM-DRILL-YTD
           END-IF
           ADD SDBM-DRILL-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE PREPAID YTD FEE ON THE DEPOSIT YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3230-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN SDBM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO SDBM-PREPAID-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDBM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE SDBM-PREPAID-YTD ROUNDED = SDBM-DEPOSIT-YTD *
                       SDBM-PENALTY-PCT / 200
               WHEN SDBM-REASON-CD = 'J'
                   MOVE 3.00 TO SDBM-PREPAID-YTD
               WHEN SDBM-REASON-CD = 'K'
                   MOVE 7.00 TO SDBM-PREPAID-YTD
               WHEN OTHER
                   COMPUTE SDBM-PREPAID-YTD ROUNDED = SDBM-DEPOSIT-YTD *
                       SDBM-PENALTY-PCT / 100
           END-EVALUATE
           IF SDBM-PREPAID-YTD > 0 AND SDBM-PREPAID-YTD < 10.00
               MOVE 10.00 TO SDBM-PREPAID-YTD
           END-IF
           IF SDBM-PREPAID-YTD > 50.00
               MOVE 50.00 TO SDBM-PREPAID-YTD
           END-IF
           ADD SDBM-PREPAID-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE RENT SCHED TIER FOR THE KEY FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3240-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-RENT-SCHED-RATE(1) TO SDBM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF SDBM-KEY-FEE-AMT NOT < WS-RENT-SCHED-LOW(WS-TX)
                   MOVE WS-RENT-SCHED-RATE(WS-TX) TO SDBM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3250-COUNT-NOTICE.
           IF SDBM-NOTICE-CNT < 9999999
               ADD 1 TO SDBM-NOTICE-CNT
           END-IF
           IF SDBM-NOTICE-CNT NOT < 3
               MOVE 'Y' TO SDBM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON LATE FEE AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3260-ACCRUE-LATE-FEE.
           IF SDBM-LATE-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBM-LATE-FEE-AMT * SDBM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO SDBM-PREPAID-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3270-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF SDBT-POST-DATE > 0 AND SDBT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDBT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SDBM-RENT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD SDBM-RENT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD SDBM-RENT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SDBM-RENT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDBM-RENT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 90 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3280-CHECK-POST.
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
               MOVE 'Y' TO SDBM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DRILL AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3290-SPLIT-DRILL.
           COMPUTE WS-WORK-AMT = SDBT-DRILL-AMT / 2
           COMPUTE WS-WORK-AMT-2 = SDBT-DRILL-AMT - (WS-WORK-AMT * 2)
           COMPUTE SDBM-RENT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE RENT AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3300-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDBT-TRAN-CD
                   ADD SDBT-RENT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDBT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDBT-RENT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE RENT AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3310-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDBM-SOURCE-CD
                   ADD SDBM-RENT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDBM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDBM-RENT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DEPOSIT YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3320-ACCRUE-DEPOSIT.
           IF SDBM-DEPOSIT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBM-DEPOSIT-YTD * SDBM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO SDBT-DEPOSIT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * AP -> DA WHEN LAST ACT DATE IS PAST
      * DA -> HV WHEN LAST ACT DATE IS PAST
      * HV -> SE WHEN LAST ACT DATE IS PAST
      * SE -> AP WHEN LAST ACT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3330-SET-CYCLE.
           EVALUATE SDBM-CYCLE-CD
               WHEN 'AP'
                   IF SDBM-LAST-ACT-DATE > 0 AND SDBM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DA' TO SDBM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DA'
                   IF SDBM-LAST-ACT-DATE > 0 AND SDBM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HV' TO SDBM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HV'
                   IF SDBM-LAST-ACT-DATE > 0 AND SDBM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SE' TO SDBM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SE'
                   IF SDBM-LAST-ACT-DATE > 0 AND SDBM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AP' TO SDBM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE KEY FEE FEE ON THE RENT AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3340-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN SDBM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO SDBM-KEY-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDBM-EXEMPT-FLAG = 'Y'
                   COMPUTE SDBM-KEY-FEE-AMT ROUNDED = SDBM-RENT-AMT *
                       SDBM-DISCOUNT / 200
               WHEN SDBM-BOX-SIZE-CD = 'CU'
                   MOVE 15.00 TO SDBM-KEY-FEE-AMT
               WHEN SDBM-BOX-SIZE-CD = 'MG'
                   MOVE 5.00 TO SDBM-KEY-FEE-AMT
               WHEN OTHER
                   COMPUTE SDBM-KEY-FEE-AMT ROUNDED = SDBM-RENT-AMT *
                       SDBM-DISCOUNT / 100
           END-EVALUATE
           IF SDBM-KEY-FEE-AMT > 0 AND SDBM-KEY-FEE-AMT < 10.00
               MOVE 10.00 TO SDBM-KEY-FEE-AMT
           END-IF
           IF SDBM-KEY-FEE-AMT > 150.00
               MOVE 150.00 TO SDBM-KEY-FEE-AMT
           END-IF
           ADD SDBM-KEY-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON KEY FEE AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3350-ACCRUE-KEY-FEE.
           IF SDBM-KEY-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBM-KEY-FEE-AMT * SDBM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO SDBM-LATE-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO RENT YTD.
      *----------------------------------------------------------------*
       3360-PRORATE-RENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDBM-RENT-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SDBM-RENT-YTD
           END-IF
           ADD SDBM-RENT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DEPOSIT AMT IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3370-LIMIT-DEPOSIT.
           IF SDBM-DEPOSIT-AMT < 100.00
               MOVE 100.00 TO SDBM-DEPOSIT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SDBM-DEPOSIT-AMT > 10000.00
               MOVE 10000.00 TO SDBM-DEPOSIT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PREPAID YTD FEE ON THE DRILL YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3380-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN SDBM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO SDBM-PREPAID-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDBM-REVIEW-FLAG = 'Y'
                   COMPUTE SDBM-PREPAID-YTD ROUNDED = SDBM-DRILL-YTD *
                       SDBM-PENALTY-PCT / 200
               WHEN SDBT-CHANNEL-CD = 'BRN'
                   MOVE 10.00 TO SDBM-PREPAID-YTD
               WHEN SDBT-CHANNEL-CD = 'BAT'
                   MOVE 20.00 TO SDBM-PREPAID-YTD
               WHEN OTHER
                   COMPUTE SDBM-PREPAID-YTD ROUNDED = SDBM-DRILL-YTD *
                       SDBM-PENALTY-PCT / 100
           END-EVALUATE
           IF SDBM-PREPAID-YTD > 0 AND SDBM-PREPAID-YTD < 10.00
               MOVE 10.00 TO SDBM-PREPAID-YTD
           END-IF
           IF SDBM-PREPAID-YTD > 35.00
               MOVE 35.00 TO SDBM-PREPAID-YTD
           END-IF
           ADD SDBM-PREPAID-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE RENT SCHED TIER FOR THE DRILL YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3390-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-RENT-SCHED-RATE(1) TO SDBM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF SDBM-DRILL-YTD NOT < WS-RENT-SCHED-LOW(WS-TX)
                   MOVE WS-RENT-SCHED-RATE(WS-TX) TO SDBM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 120 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3400-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF SDBM-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDBM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO SDBT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE RENT AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3410-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDBM-SOURCE-CD
                   ADD SDBT-RENT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDBM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDBT-RENT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR VAULT CD:
      * BU -> ET WHEN LAST ACT DATE IS PAST
      * ET -> FF WHEN LAST ACT DATE IS PAST
      * FF -> NG WHEN LAST ACT DATE IS PAST
      * NG -> NH WHEN LAST ACT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3420-SET-VAULT.
           EVALUATE SDBM-VAULT-CD
               WHEN 'BU'
                   IF SDBM-LAST-ACT-DATE > 0 AND SDBM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ET' TO SDBM-VAULT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ET'
                   IF SDBM-LAST-ACT-DATE > 0 AND SDBM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FF' TO SDBM-VAULT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FF'
                   IF SDBM-LAST-ACT-DATE > 0 AND SDBM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NG' TO SDBM-VAULT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NG'
                   IF SDBM-LAST-ACT-DATE > 0 AND SDBM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NH' TO SDBM-VAULT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE DEPOSIT FEE ON THE PREPAID YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3430-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN SDBM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO SDBT-DEPOSIT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDBM-EXEMPT-FLAG = 'Y'
                   COMPUTE SDBT-DEPOSIT-AMT ROUNDED = SDBM-PREPAID-YTD *
                       SDBM-FEE-PCT / 200
               WHEN SDBM-CYCLE-CD = 'AP'
                   MOVE 12.00 TO SDBT-DEPOSIT-AMT
               WHEN SDBM-CYCLE-CD = 'DA'
                   MOVE 10.00 TO SDBT-DEPOSIT-AMT
               WHEN OTHER
                   COMPUTE SDBT-DEPOSIT-AMT ROUNDED = SDBM-PREPAID-YTD *
                       SDBM-FEE-PCT / 100
           END-EVALUATE
           IF SDBT-DEPOSIT-AMT > 0 AND SDBT-DEPOSIT-AMT < 2.00
               MOVE 2.00 TO SDBT-DEPOSIT-AMT
           END-IF
           IF SDBT-DEPOSIT-AMT > 100.00
               MOVE 100.00 TO SDBT-DEPOSIT-AMT
           END-IF
           ADD SDBT-DEPOSIT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * KEY FEE AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3440-FLAG-KEY-FEE.
           IF SDBM-KEY-FEE-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE SDBM-KEY-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE RENT YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3450-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDBM-REASON-CD
                   ADD SDBM-RENT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDBM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDBM-RENT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE RENT AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3460-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDBM-SOURCE-CD
                   ADD SDBT-RENT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDBM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDBT-RENT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * EXS -> FJW WHEN LAST STMT DATE IS PAST
      * FJW -> YFB WHEN LAST STMT DATE IS PAST
      * YFB -> EXS WHEN LAST STMT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3470-SET-BRANCH.
           EVALUATE SDBM-BRANCH-CD
               WHEN 'EXS'
                   IF SDBM-LAST-STMT-DATE > 0 AND SDBM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FJW' TO SDBM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FJW'
                   IF SDBM-LAST-STMT-DATE > 0 AND SDBM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YFB' TO SDBM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YFB'
                   IF SDBM-LAST-STMT-DATE > 0 AND SDBM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EXS' TO SDBM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3480-CHECK-CLOSE.
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
               MOVE 'Y' TO SDBM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DEPOSIT YTD IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3490-LIMIT-DEPOSIT.
           IF SDBM-DEPOSIT-YTD < 100.00
               MOVE 100.00 TO SDBM-DEPOSIT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SDBM-DEPOSIT-YTD > 5000.00
               MOVE 5000.00 TO SDBM-DEPOSIT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DRILL AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3500-FLAG-DRILL.
           IF SDBT-DRILL-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE SDBT-DRILL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * AP -> DA WHEN LAST ACT DATE IS PAST
      * DA -> HV WHEN LAST ACT DATE IS PAST
      * HV -> SE WHEN LAST ACT DATE IS PAST
      * SE -> AP WHEN LAST ACT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3510-SET-CYCLE.
           EVALUATE SDBM-CYCLE-CD
               WHEN 'AP'
                   IF SDBM-LAST-ACT-DATE > 0 AND SDBM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DA' TO SDBM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DA'
                   IF SDBM-LAST-ACT-DATE > 0 AND SDBM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HV' TO SDBM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HV'
                   IF SDBM-LAST-ACT-DATE > 0 AND SDBM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SE' TO SDBM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SE'
                   IF SDBM-LAST-ACT-DATE > 0 AND SDBM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AP' TO SDBM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E354 DESC REQUIRED
      * E983 RENT AMT NEGATIVE
      * E401 RENT AMT OVER 50000
      * E634 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3520-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDBM-DESC = SPACES
                   MOVE 'E354' TO WS-REJECT-CODE
               WHEN SDBM-RENT-AMT < 0
                   MOVE 'E983' TO WS-REJECT-CODE
               WHEN SDBM-RENT-AMT > 50000.00
                   MOVE 'E401' TO WS-REJECT-CODE
               WHEN SDBM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E634' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE RENT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3530-PRORATE-RENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBM-RENT-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT DRILL AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3540-SPLIT-DRILL.
           COMPUTE WS-WORK-AMT = SDBM-DRILL-AMT / 3
           COMPUTE WS-WORK-AMT-2 = SDBM-DRILL-AMT - (WS-WORK-AMT * 3)
           COMPUTE SDBT-DRILL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 45 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3550-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF SDBM-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDBM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO SDBM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE KEY FEE AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3560-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDBM-REASON-CD
                   ADD SDBM-KEY-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDBM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDBM-KEY-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 45 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3570-CHECK-LAST-ACT.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO SDBT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BOX SIZE CD:
      * CU -> MG WHEN LAST ACT DATE IS PAST
      * MG -> VG WHEN LAST ACT DATE IS PAST
      * VG -> VN WHEN LAST ACT DATE IS PAST
      * VN -> XJ WHEN LAST ACT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3580-SET-BOX-SIZE.
           EVALUATE SDBM-BOX-SIZE-CD
               WHEN 'CU'
                   IF SDBM-LAST-ACT-DATE > 0 AND SDBM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MG' TO SDBM-BOX-SIZE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MG'
                   IF SDBM-LAST-ACT-DATE > 0 AND SDBM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VG' TO SDBM-BOX-SIZE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VG'
                   IF SDBM-LAST-ACT-DATE > 0 AND SDBM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VN' TO SDBM-BOX-SIZE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VN'
                   IF SDBM-LAST-ACT-DATE > 0 AND SDBM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XJ' TO SDBM-BOX-SIZE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PREPAID AMT IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3590-LIMIT-PREPAID.
           IF SDBM-PREPAID-AMT < 100.00
               MOVE 100.00 TO SDBM-PREPAID-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SDBM-PREPAID-AMT > 10000.00
               MOVE 10000.00 TO SDBM-PREPAID-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3600-DESCRIBE-BRANCH.
           EVALUATE SDBM-BRANCH-CD
               WHEN 'EXS'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'FJW'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'YFB'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE KEY FEE GRID TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3610-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-KEY-FEE-GRID-RATE(1) TO SDBM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF WS-WORK-AMT NOT < WS-KEY-FEE-GRID-LOW(WS-TX)
                   MOVE WS-KEY-FEE-GRID-RATE(WS-TX) TO SDBM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3620-COUNT-ITEM.
           IF SDBM-ITEM-CNT < 9999999
               ADD 1 TO SDBM-ITEM-CNT
           END-IF
           IF SDBM-ITEM-CNT NOT < 6
               MOVE 'Y' TO SDBM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE LATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO DEPOSIT AMT.
      *----------------------------------------------------------------*
       3630-PRORATE-DEPOSIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDBM-DEPOSIT-AMT ROUNDED =
                   SDBM-LATE-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SDBM-DEPOSIT-AMT
           END-IF
           ADD SDBM-DEPOSIT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 15 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3640-CHECK-LAST-MAINT.
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
               MOVE 'Y' TO SDBM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3650-DESCRIBE-REASON.
           EVALUATE SDBM-REASON-CD
               WHEN 'J'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'K'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'L'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'M'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'S'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'W'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'Y'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE KEY FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO KEY FEE AMT.
      *----------------------------------------------------------------*
       3660-PRORATE-KEY-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDBT-KEY-FEE-AMT ROUNDED =
                   SDBM-KEY-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SDBT-KEY-FEE-AMT
           END-IF
           ADD SDBT-KEY-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE PREPAID BAND TIER FOR THE PREPAID YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3670-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PREPAID-BAND-RATE(1) TO SDBM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF SDBM-PREPAID-YTD NOT < WS-PREPAID-BAND-LOW(WS-TX)
                   MOVE WS-PREPAID-BAND-RATE(WS-TX) TO SDBM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BOX SIZE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3680-DESCRIBE-BOX-SIZE.
           EVALUATE SDBM-BOX-SIZE-CD
               WHEN 'CU'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'MG'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'VG'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'VN'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'XJ'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3690-DESCRIBE-CHANNEL.
           EVALUATE SDBT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * RN -> AC WHEN NOTICE DATE IS PAST
      * AC -> DR WHEN NOTICE DATE IS PAST
      * DR -> KY WHEN NOTICE DATE IS PAST
      * KY -> CL WHEN NOTICE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3700-SET-TRAN.
           EVALUATE SDBT-TRAN-CD
               WHEN 'RN'
                   IF SDBM-NOTICE-DATE > 0 AND SDBM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AC' TO SDBT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AC'
                   IF SDBM-NOTICE-DATE > 0 AND SDBM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DR' TO SDBT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DR'
                   IF SDBM-NOTICE-DATE > 0 AND SDBM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KY' TO SDBT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KY'
                   IF SDBM-NOTICE-DATE > 0 AND SDBM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CL' TO SDBT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E822 REJECT CD REQUIRED
      * E511 RENT YTD NEGATIVE
      * E415 RENT YTD OVER 50000
      * E132 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3710-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDBT-REJECT-CD = SPACES
                   MOVE 'E822' TO WS-REJECT-CODE
               WHEN SDBM-RENT-YTD < 0
                   MOVE 'E511' TO WS-REJECT-CODE
               WHEN SDBM-RENT-YTD > 50000.00
                   MOVE 'E415' TO WS-REJECT-CODE
               WHEN SDBT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E132' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PREPAID BAND TIER FOR THE DEPOSIT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3720-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PREPAID-BAND-RATE(1) TO SDBM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF SDBM-DEPOSIT-AMT NOT < WS-PREPAID-BAND-LOW(WS-TX)
                   MOVE WS-PREPAID-BAND-RATE(WS-TX) TO SDBM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PREPAID YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO RENT AMT.
      *----------------------------------------------------------------*
       3730-PRORATE-RENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDBT-RENT-AMT ROUNDED =
                   SDBM-PREPAID-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SDBT-RENT-AMT
           END-IF
           ADD SDBT-RENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE KEY FEE GRID TIER FOR THE DRILL YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3740-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-KEY-FEE-GRID-RATE(1) TO SDBM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF SDBM-DRILL-YTD NOT < WS-KEY-FEE-GRID-LOW(WS-TX)
                   MOVE WS-KEY-FEE-GRID-RATE(WS-TX) TO SDBM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE KEY FEE GRID TIER FOR THE KEY FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3750-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-KEY-FEE-GRID-RATE(1) TO SDBM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF SDBM-KEY-FEE-YTD NOT < WS-KEY-FEE-GRID-LOW(WS-TX)
                   MOVE WS-KEY-FEE-GRID-RATE(WS-TX) TO SDBM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3760-AGE-LAST-MAINT.
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
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON LATE FEE YTD AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3770-ACCRUE-LATE-FEE.
           IF SDBM-LATE-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBM-LATE-FEE-YTD * SDBM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO SDBT-DEPOSIT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR VAULT CD:
      * BU -> ET WHEN LAST STMT DATE IS PAST
      * ET -> FF WHEN LAST STMT DATE IS PAST
      * FF -> NG WHEN LAST STMT DATE IS PAST
      * NG -> NH WHEN LAST STMT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3780-SET-VAULT.
           EVALUATE SDBM-VAULT-CD
               WHEN 'BU'
                   IF SDBM-LAST-STMT-DATE > 0 AND SDBM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ET' TO SDBM-VAULT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ET'
                   IF SDBM-LAST-STMT-DATE > 0 AND SDBM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FF' TO SDBM-VAULT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FF'
                   IF SDBM-LAST-STMT-DATE > 0 AND SDBM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NG' TO SDBM-VAULT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NG'
                   IF SDBM-LAST-STMT-DATE > 0 AND SDBM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NH' TO SDBM-VAULT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT RENT AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3790-SPLIT-RENT.
           COMPUTE WS-WORK-AMT = SDBT-RENT-AMT / 3
           COMPUTE WS-WORK-AMT-2 = SDBT-RENT-AMT - (WS-WORK-AMT * 3)
           COMPUTE SDBM-PREPAID-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DEPOSIT YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3800-FLAG-DEPOSIT.
           IF SDBM-DEPOSIT-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE SDBM-DEPOSIT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * RENT AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3810-FLAG-RENT.
           IF SDBT-RENT-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE SDBT-RENT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E455 REF NO REQUIRED
      * E511 KEY FEE AMT NEGATIVE
      * E048 KEY FEE AMT OVER 99999
      * E702 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3820-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDBM-REF-NO = SPACES
                   MOVE 'E455' TO WS-REJECT-CODE
               WHEN SDBT-KEY-FEE-AMT < 0
                   MOVE 'E511' TO WS-REJECT-CODE
               WHEN SDBT-KEY-FEE-AMT > 99999.00
                   MOVE 'E048' TO WS-REJECT-CODE
               WHEN SDBM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E702' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DRILL AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3830-ACCRUE-DRILL.
           IF SDBM-DRILL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBM-DRILL-AMT * SDBM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO SDBM-DEPOSIT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3840-COUNT-ITEM.
           IF SDBM-ITEM-CNT < 9999999
               ADD 1 TO SDBM-ITEM-CNT
           END-IF
           IF SDBM-ITEM-CNT NOT < 10
               MOVE 'Y' TO SDBM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-90, 91-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3850-AGE-CLOSE.
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
                   ADD SDBM-LATE-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD SDBM-LATE-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD SDBM-LATE-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SDBM-LATE-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDBM-LATE-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE LATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PREPAID YTD.
      *----------------------------------------------------------------*
       3860-PRORATE-PREPAID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDBM-PREPAID-YTD ROUNDED =
                   SDBM-LATE-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SDBM-PREPAID-YTD
           END-IF
           ADD SDBM-PREPAID-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * RENT AMT IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3870-LIMIT-RENT.
           IF SDBM-RENT-AMT < 50.00
               MOVE 50.00 TO SDBM-RENT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SDBM-RENT-AMT > 1000.00
               MOVE 1000.00 TO SDBM-RENT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PREPAID YTD IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3880-LIMIT-PREPAID.
           IF SDBM-PREPAID-YTD < 50.00
               MOVE 50.00 TO SDBM-PREPAID-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SDBM-PREPAID-YTD > 2500.00
               MOVE 2500.00 TO SDBM-PREPAID-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN LAST ACT DATE IS PAST
      * BAT -> MAL WHEN LAST ACT DATE IS PAST
      * MAL -> TEL WHEN LAST ACT DATE IS PAST
      * TEL -> ONL WHEN LAST ACT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3890-SET-CHANNEL.
           EVALUATE SDBT-CHANNEL-CD
               WHEN 'BRN'
                   IF SDBM-LAST-ACT-DATE > 0 AND SDBM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO SDBT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF SDBM-LAST-ACT-DATE > 0 AND SDBM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO SDBT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF SDBM-LAST-ACT-DATE > 0 AND SDBM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO SDBT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF SDBM-LAST-ACT-DATE > 0 AND SDBM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO SDBT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 120 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3900-CHECK-OPEN.
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
           IF WS-DAYS > 120
               MOVE 'Y' TO SDBM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE LATE FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO RENT AMT.
      *----------------------------------------------------------------*
       3910-PRORATE-RENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDBT-RENT-AMT ROUNDED =
                   SDBM-LATE-FEE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SDBT-RENT-AMT
           END-IF
           ADD SDBT-RENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE DEPOSIT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3920-PRORATE-DEPOSIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   SDBM-DEPOSIT-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE LATE FEE BAND TIER FOR THE KEY FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3930-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-LATE-FEE-BAND-RATE(1) TO SDBM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF SDBM-KEY-FEE-AMT NOT < WS-LATE-FEE-BAND-LOW(WS-TX)
                   MOVE WS-LATE-FEE-BAND-RATE(WS-TX) TO SDBM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3940-COUNT-RETRY.
           IF SDBM-RETRY-CNT < 9999999
               ADD 1 TO SDBM-RETRY-CNT
           END-IF
           IF SDBM-RETRY-CNT NOT < 12
               MOVE 'Y' TO SDBM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3950-AGE-CLOSE.
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
                   ADD SDBM-PREPAID-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD SDBM-PREPAID-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD SDBM-PREPAID-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD SDBM-PREPAID-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDBM-PREPAID-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE DRILL FEE ON THE PREPAID YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3960-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN SDBM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO SDBT-DRILL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDBM-EXEMPT-FLAG = 'Y'
                   COMPUTE SDBT-DRILL-AMT ROUNDED = SDBM-PREPAID-YTD *
                       SDBM-DISCOUNT / 200
               WHEN SDBT-CHANNEL-CD = 'BRN'
                   MOVE 20.00 TO SDBT-DRILL-AMT
               WHEN SDBT-CHANNEL-CD = 'BAT'
                   MOVE 3.00 TO SDBT-DRILL-AMT
               WHEN OTHER
                   COMPUTE SDBT-DRILL-AMT ROUNDED = SDBM-PREPAID-YTD *
                       SDBM-DISCOUNT / 100
           END-EVALUATE
           IF SDBT-DRILL-AMT > 0 AND SDBT-DRILL-AMT < 5.00
               MOVE 5.00 TO SDBT-DRILL-AMT
           END-IF
           IF SDBT-DRILL-AMT > 250.00
               MOVE 250.00 TO SDBT-DRILL-AMT
           END-IF
           ADD SDBT-DRILL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 365 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3970-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF SDBM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDBM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO SDBT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON LATE FEE YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3980-ACCRUE-LATE-FEE.
           IF SDBM-LATE-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBM-LATE-FEE-YTD * SDBM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO SDBM-DRILL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3990-COUNT-NOTICE.
           IF SDBM-NOTICE-CNT < 9999999
               ADD 1 TO SDBM-NOTICE-CNT
           END-IF
           IF SDBM-NOTICE-CNT NOT < 5
               MOVE 'Y' TO SDBM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4000-CHECK-NEXT-DUE.
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
      * DRILL AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4010-FLAG-DRILL.
           IF SDBM-DRILL-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE SDBM-DRILL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4020-DESCRIBE-REASON.
           EVALUATE SDBM-REASON-CD
               WHEN 'J'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'K'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'L'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'M'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'S'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'W'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'Y'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON LATE FEE YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4030-ACCRUE-LATE-FEE.
           IF SDBM-LATE-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBM-LATE-FEE-YTD * SDBM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO SDBM-KEY-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4040-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * SDBM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PREPAID BAND TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4050-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PREPAID-BAND-RATE(1) TO SDBM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF WS-WORK-AMT NOT < WS-PREPAID-BAND-LOW(WS-TX)
                   MOVE WS-PREPAID-BAND-RATE(WS-TX) TO SDBM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4060-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF SDBM-NEXT-REVIEW-DATE > 0 AND SDBM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDBM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SDBM-PREPAID-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD SDBM-PREPAID-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD SDBM-PREPAID-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD SDBM-PREPAID-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDBM-PREPAID-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * KEY FEE YTD IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4070-LIMIT-KEY-FEE.
           IF SDBM-KEY-FEE-YTD < 0.00
               MOVE 0.00 TO SDBM-KEY-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SDBM-KEY-FEE-YTD > 1000.00
               MOVE 1000.00 TO SDBM-KEY-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4080-DESCRIBE-SOURCE.
           EVALUATE SDBM-SOURCE-CD
               WHEN 'DD'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'JZ'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'PS'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'UM'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE LATE FEE BAND TIER FOR THE DEPOSIT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4090-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-LATE-FEE-BAND-RATE(1) TO SDBM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF SDBT-DEPOSIT-AMT NOT < WS-LATE-FEE-BAND-LOW(WS-TX)
                   MOVE WS-LATE-FEE-BAND-RATE(WS-TX) TO SDBM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PREPAID AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4100-FLAG-PREPAID.
           IF SDBM-PREPAID-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE SDBM-PREPAID-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PREPAID YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4110-FLAG-PREPAID.
           IF SDBM-PREPAID-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE SDBM-PREPAID-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BOX SIZE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4120-DESCRIBE-BOX-SIZE.
           EVALUATE SDBM-BOX-SIZE-CD
               WHEN 'CU'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'MG'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'VG'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'VN'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'XJ'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * RN -> AC WHEN NEXT REVIEW DATE IS PAST
      * AC -> DR WHEN NEXT REVIEW DATE IS PAST
      * DR -> KY WHEN NEXT REVIEW DATE IS PAST
      * KY -> CL WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4130-SET-TRAN.
           EVALUATE SDBT-TRAN-CD
               WHEN 'RN'
                   IF SDBM-NEXT-REVIEW-DATE > 0 AND
                       SDBM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'AC' TO SDBT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AC'
                   IF SDBM-NEXT-REVIEW-DATE > 0 AND
                       SDBM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'DR' TO SDBT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DR'
                   IF SDBM-NEXT-REVIEW-DATE > 0 AND
                       SDBM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'KY' TO SDBT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KY'
                   IF SDBM-NEXT-REVIEW-DATE > 0 AND
                       SDBM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'CL' TO SDBT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PREPAID AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4140-ACCRUE-PREPAID.
           IF SDBM-PREPAID-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBM-PREPAID-AMT * SDBM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO SDBT-DEPOSIT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4150-DESCRIBE-CHANNEL.
           EVALUATE SDBT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT KEY FEE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4160-SPLIT-KEY-FEE.
           COMPUTE WS-WORK-AMT = SDBM-KEY-FEE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = SDBM-KEY-FEE-AMT - (WS-WORK-AMT * 2)
           COMPUTE SDBT-DEPOSIT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E181 MEMO REQUIRED
      * E370 DEPOSIT AMT NEGATIVE
      * E071 DEPOSIT AMT OVER 250000
      * E437 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       4170-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDBT-MEMO = SPACES
                   MOVE 'E181' TO WS-REJECT-CODE
               WHEN SDBT-DEPOSIT-AMT < 0
                   MOVE 'E370' TO WS-REJECT-CODE
               WHEN SDBT-DEPOSIT-AMT > 250000.00
                   MOVE 'E071' TO WS-REJECT-CODE
               WHEN SDBM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E437' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DEPOSIT YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4180-SPLIT-DEPOSIT.
           COMPUTE WS-WORK-AMT = SDBM-DEPOSIT-YTD / 12
           COMPUTE WS-WORK-AMT-2 = SDBM-DEPOSIT-YTD - (WS-WORK-AMT * 12)
           COMPUTE SDBM-LATE-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DRILL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO DEPOSIT YTD.
      *----------------------------------------------------------------*
       4190-PRORATE-DEPOSIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDBM-DEPOSIT-YTD ROUNDED =
                   SDBM-DRILL-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SDBM-DEPOSIT-YTD
           END-IF
           ADD SDBM-DEPOSIT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4200-AGE-NEXT-DUE.
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
                   ADD SDBM-KEY-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD SDBM-KEY-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD SDBM-KEY-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD SDBM-KEY-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDBM-KEY-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4210-COUNT-NOTICE.
           IF SDBM-NOTICE-CNT < 9999999
               ADD 1 TO SDBM-NOTICE-CNT
           END-IF
           IF SDBM-NOTICE-CNT NOT < 5
               MOVE 'Y' TO SDBM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4220-LIMIT-ITEM.
           IF WS-WORK-AMT < 50.00
               MOVE 50.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 10000.00
               MOVE 10000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E963 OPERATOR REQUIRED
      * E639 DRILL AMT NEGATIVE
      * E361 DRILL AMT OVER 1000000
      * E636 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       4230-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDBT-OPERATOR = SPACES
                   MOVE 'E963' TO WS-REJECT-CODE
               WHEN SDBM-DRILL-AMT < 0
                   MOVE 'E639' TO WS-REJECT-CODE
               WHEN SDBM-DRILL-AMT > 1000000.00
                   MOVE 'E361' TO WS-REJECT-CODE
               WHEN SDBM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E636' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RENT AMT IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4240-LIMIT-RENT.
           IF SDBT-RENT-AMT < 0.00
               MOVE 0.00 TO SDBT-RENT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SDBT-RENT-AMT > 1000.00
               MOVE 1000.00 TO SDBT-RENT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4250-COUNT-NOTICE.
           IF SDBM-NOTICE-CNT < 9999999
               ADD 1 TO SDBM-NOTICE-CNT
           END-IF
           IF SDBM-NOTICE-CNT NOT < 5
               MOVE 'Y' TO SDBM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       4260-COUNT-SEQ.
           IF SDBT-SEQ < 9999999
               ADD 1 TO SDBT-SEQ
           END-IF
           IF SDBT-SEQ NOT < 12
               MOVE 'Y' TO SDBM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E647 OPERATOR REQUIRED
      * E816 DRILL AMT NEGATIVE
      * E460 DRILL AMT OVER 25000
      * E631 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       4270-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDBT-OPERATOR = SPACES
                   MOVE 'E647' TO WS-REJECT-CODE
               WHEN SDBM-DRILL-AMT < 0
                   MOVE 'E816' TO WS-REJECT-CODE
               WHEN SDBM-DRILL-AMT > 25000.00
                   MOVE 'E460' TO WS-REJECT-CODE
               WHEN SDBM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E631' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RENT FEE ON THE LATE FEE YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4280-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN SDBM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO SDBT-RENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDBT-REVERSAL-FLAG = 'Y'
                   COMPUTE SDBT-RENT-AMT ROUNDED = SDBM-LATE-FEE-YTD *
                       SDBM-FEE-PCT / 200
               WHEN SDBT-TRAN-CD = 'RN'
                   MOVE 3.00 TO SDBT-RENT-AMT
               WHEN SDBT-TRAN-CD = 'AC'
                   MOVE 20.00 TO SDBT-RENT-AMT
               WHEN OTHER
                   COMPUTE SDBT-RENT-AMT ROUNDED = SDBM-LATE-FEE-YTD *
                       SDBM-FEE-PCT / 100
           END-EVALUATE
           IF SDBT-RENT-AMT > 0 AND SDBT-RENT-AMT < 2.00
               MOVE 2.00 TO SDBT-RENT-AMT
           END-IF
           IF SDBT-RENT-AMT > 75.00
               MOVE 75.00 TO SDBT-RENT-AMT
           END-IF
           ADD SDBT-RENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DEPOSIT YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4290-FLAG-DEPOSIT.
           IF SDBM-DEPOSIT-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE SDBM-DEPOSIT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4300-SPLIT-LATE-FEE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 12)
           COMPUTE SDBM-LATE-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BOX SIZE CD:
      * CU -> MG WHEN NEXT REVIEW DATE IS PAST
      * MG -> VG WHEN NEXT REVIEW DATE IS PAST
      * VG -> VN WHEN NEXT REVIEW DATE IS PAST
      * VN -> XJ WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4310-SET-BOX-SIZE.
           EVALUATE SDBM-BOX-SIZE-CD
               WHEN 'CU'
                   IF SDBM-NEXT-REVIEW-DATE > 0 AND
                       SDBM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'MG' TO SDBM-BOX-SIZE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MG'
                   IF SDBM-NEXT-REVIEW-DATE > 0 AND
                       SDBM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'VG' TO SDBM-BOX-SIZE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VG'
                   IF SDBM-NEXT-REVIEW-DATE > 0 AND
                       SDBM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'VN' TO SDBM-BOX-SIZE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VN'
                   IF SDBM-NEXT-REVIEW-DATE > 0 AND
                       SDBM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'XJ' TO SDBM-BOX-SIZE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4320-AGE-NOTICE.
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
                   ADD SDBM-PREPAID-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD SDBM-PREPAID-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD SDBM-PREPAID-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD SDBM-PREPAID-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDBM-PREPAID-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RENT AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4330-FLAG-RENT.
           IF SDBM-RENT-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE SDBM-RENT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       4340-AGE-NEXT-DUE.
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
               WHEN WS-DAYS NOT > 60
                   ADD SDBM-DRILL-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD SDBM-DRILL-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDBM-DRILL-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4350-DESCRIBE-CYCLE.
           EVALUATE SDBM-CYCLE-CD
               WHEN 'AP'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'DA'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'HV'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'SE'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * KEY FEE YTD IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4360-LIMIT-KEY-FEE.
           IF SDBM-KEY-FEE-YTD < 25.00
               MOVE 25.00 TO SDBM-KEY-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SDBM-KEY-FEE-YTD > 10000.00
               MOVE 10000.00 TO SDBM-KEY-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E436 DESC REQUIRED
      * E376 RENT YTD NEGATIVE
      * E886 RENT YTD OVER 25000
      * E444 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       4370-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDBM-DESC = SPACES
                   MOVE 'E436' TO WS-REJECT-CODE
               WHEN SDBM-RENT-YTD < 0
                   MOVE 'E376' TO WS-REJECT-CODE
               WHEN SDBM-RENT-YTD > 25000.00
                   MOVE 'E886' TO WS-REJECT-CODE
               WHEN SDBM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E444' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * EXS -> FJW WHEN LAST MAINT DATE IS PAST
      * FJW -> YFB WHEN LAST MAINT DATE IS PAST
      * YFB -> EXS WHEN LAST MAINT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4380-SET-BRANCH.
           EVALUATE SDBM-BRANCH-CD
               WHEN 'EXS'
                   IF SDBM-LAST-MAINT-DATE > 0 AND SDBM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'FJW' TO SDBM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FJW'
                   IF SDBM-LAST-MAINT-DATE > 0 AND SDBM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'YFB' TO SDBM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YFB'
                   IF SDBM-LAST-MAINT-DATE > 0 AND SDBM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'EXS' TO SDBM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT KEY FEE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4390-SPLIT-KEY-FEE.
           COMPUTE WS-WORK-AMT = SDBM-KEY-FEE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = SDBM-KEY-FEE-AMT - (WS-WORK-AMT * 4)
           COMPUTE SDBT-DRILL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E960 OPERATOR REQUIRED
      * E078 PREPAID YTD NEGATIVE
      * E477 PREPAID YTD OVER 99999
      * E093 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       4400-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDBT-OPERATOR = SPACES
                   MOVE 'E960' TO WS-REJECT-CODE
               WHEN SDBM-PREPAID-YTD < 0
                   MOVE 'E078' TO WS-REJECT-CODE
               WHEN SDBM-PREPAID-YTD > 99999.00
                   MOVE 'E477' TO WS-REJECT-CODE
               WHEN SDBM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E093' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BOX SIZE CD:
      * CU -> MG WHEN NOTICE DATE IS PAST
      * MG -> VG WHEN NOTICE DATE IS PAST
      * VG -> VN WHEN NOTICE DATE IS PAST
      * VN -> XJ WHEN NOTICE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4410-SET-BOX-SIZE.
           EVALUATE SDBM-BOX-SIZE-CD
               WHEN 'CU'
                   IF SDBM-NOTICE-DATE > 0 AND SDBM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MG' TO SDBM-BOX-SIZE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MG'
                   IF SDBM-NOTICE-DATE > 0 AND SDBM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VG' TO SDBM-BOX-SIZE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VG'
                   IF SDBM-NOTICE-DATE > 0 AND SDBM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VN' TO SDBM-BOX-SIZE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VN'
                   IF SDBM-NOTICE-DATE > 0 AND SDBM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XJ' TO SDBM-BOX-SIZE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DEPOSIT AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4420-FLAG-DEPOSIT.
           IF SDBT-DEPOSIT-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE SDBT-DEPOSIT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4430-DESCRIBE-TRAN.
           EVALUATE SDBT-TRAN-CD
               WHEN 'RN'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'AC'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'DR'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'KY'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'CL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RENT AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4440-ACCRUE-RENT.
           IF SDBM-RENT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBM-RENT-AMT * SDBM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO SDBM-DEPOSIT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PREPAID YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4450-FLAG-PREPAID.
           IF SDBM-PREPAID-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE SDBM-PREPAID-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * LATE FEE YTD IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4460-LIMIT-LATE-FEE.
           IF SDBM-LATE-FEE-YTD < 0.00
               MOVE 0.00 TO SDBM-LATE-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SDBM-LATE-FEE-YTD > 2500.00
               MOVE 2500.00 TO SDBM-LATE-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DEPOSIT AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4470-FLAG-DEPOSIT.
           IF SDBT-DEPOSIT-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE SDBT-DEPOSIT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4480-CHECK-NEXT-DUE.
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
           IF WS-DAYS > 30
               MOVE 'Y' TO SDBT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       4490-COUNT-NOTICE.
           IF SDBM-NOTICE-CNT < 9999999
               ADD 1 TO SDBM-NOTICE-CNT
           END-IF
           IF SDBM-NOTICE-CNT NOT < 12
               MOVE 'Y' TO SDBM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE DEPOSIT AMT BY VAULT CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4500-ACCUM-VAULT.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDBM-VAULT-CD
                   ADD SDBT-DEPOSIT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDBM-VAULT-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDBT-DEPOSIT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT RENT YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4510-SPLIT-RENT.
           COMPUTE WS-WORK-AMT = SDBM-RENT-YTD / 4
           COMPUTE WS-WORK-AMT-2 = SDBM-RENT-YTD - (WS-WORK-AMT * 4)
           COMPUTE SDBM-DRILL-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT KEY FEE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4520-SPLIT-KEY-FEE.
           COMPUTE WS-WORK-AMT = SDBT-KEY-FEE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = SDBT-KEY-FEE-AMT - (WS-WORK-AMT * 2)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PREPAID YTD BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4530-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDBM-CYCLE-CD
                   ADD SDBM-PREPAID-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDBM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDBM-PREPAID-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DRILL AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4540-FLAG-DRILL.
           IF SDBT-DRILL-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE SDBT-DRILL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E023 REF NO REQUIRED
      * E713 RENT AMT NEGATIVE
      * E882 RENT AMT OVER 99999
      * E871 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       4550-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDBM-REF-NO = SPACES
                   MOVE 'E023' TO WS-REJECT-CODE
               WHEN SDBT-RENT-AMT < 0
                   MOVE 'E713' TO WS-REJECT-CODE
               WHEN SDBT-RENT-AMT > 99999.00
                   MOVE 'E882' TO WS-REJECT-CODE
               WHEN SDBM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E871' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

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
                   MOVE SDLIM-DEPOSIT-AMT
                     TO SDBM-DEPOSIT-AMT
                   MOVE SDLIM-DRILL-YTD
                     TO SDBM-DRILL-YTD
                   MOVE SDLIM-OVERRIDE-FLAG
                     TO SDBM-OVERRIDE-FLAG
                   MOVE SDLIM-EXEMPT-FLAG
                     TO SDBM-EXEMPT-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RLIM' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-SDLIM' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE SDB FIELDS TO SDRISK AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7010-CALL-SDRISK.
           INITIALIZE SDRISK-PARMS
           MOVE SDBM-LATE-FEE-YTD
             TO SDRISK-LATE-FEE-YTD
           MOVE SDBM-KEY-FEE-YTD
             TO SDRISK-KEY-FEE-YTD
           MOVE SDBM-DEPOSIT-AMT
             TO SDRISK-DEPOSIT-AMT
           MOVE SDBM-DEPOSIT-YTD
             TO SDRISK-DEPOSIT-YTD
           MOVE SDBM-LATE-FEE-AMT
             TO SDRISK-LATE-FEE-AMT
           MOVE SDBM-KEY-FEE-AMT
             TO SDRISK-KEY-FEE-AMT
           MOVE SDBM-NEXT-DUE-DATE
             TO SDRISK-NEXT-DUE-DATE
           MOVE SDBM-OPEN-DATE
             TO SDRISK-OPEN-DATE
           MOVE SDBM-NEXT-REVIEW-DATE
             TO SDRISK-NEXT-REVIEW-DATE
           MOVE SDBM-LAST-MAINT-DATE
             TO SDRISK-LAST-MAINT-DATE
           MOVE SDBM-REASON-CD
             TO SDRISK-REASON-CD
           MOVE SDBM-CYCLE-CD
             TO SDRISK-CYCLE-CD
           MOVE SDBM-BOX-SIZE-CD
             TO SDRISK-BOX-SIZE-CD
           MOVE SDBM-VAULT-CD
             TO SDRISK-VAULT-CD
           MOVE SDBM-AUTO-RENEW-FLAG
             TO SDRISK-AUTO-RENEW-FLAG
           MOVE SDBM-OVERRIDE-FLAG
             TO SDRISK-OVERRIDE-FLAG
           MOVE SDBM-PREFERRED-FLAG
             TO SDRISK-PREFERRED-FLAG
           MOVE SDBM-ITEM-CNT
             TO SDRISK-ITEM-CNT
           MOVE SDBM-PAYMENT-CNT
             TO SDRISK-PAYMENT-CNT
           MOVE SDBM-FEE-PCT
             TO SDRISK-FEE-PCT
           MOVE SDBM-PENALTY-PCT
             TO SDRISK-PENALTY-PCT
           MOVE SDBM-DESC
             TO SDRISK-DESC
           MOVE SDBM-CUST-ID
             TO SDRISK-CUST-ID
           CALL 'SDRISK' USING SDRISK-PARMS
           EVALUATE SDRISK-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE SDRISK-KEY-FEE-YTD
                     TO SDBM-KEY-FEE-YTD
                   MOVE SDRISK-LATE-FEE-AMT
                     TO SDBM-LATE-FEE-AMT
                   MOVE SDRISK-OVERRIDE-FLAG
                     TO SDBM-OVERRIDE-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RISK' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-SDRISK' TO ABND-PARAGRAPH
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
           DISPLAY 'SDBCLSE READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'SDBCLSE' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
