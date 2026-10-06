      *================================================================*
      * PROGRAM  : SDBMTHE                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : SAFE DEPOSIT BOXES - MONTH-END. READS THE SDB       *
      *            TRANSACTIONS, UPDATES THE SDB MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. SAFE DEPOSIT BOX RENTAL BILLING,*
      *            ACCESS LOG AND DRILLING                             *
      * INPUT    : SDBMST (KSDS), SDBTRN, BANKCTL                      *
      * OUTPUT   : SDBOUT, SDBMTRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, SDLIM                   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1993-04-19 JLM  ORIGINAL                                       *
      * 1997-06-03 DPS  DEPOSIT STATUS CODES ADDED PRB2076             *
      * 1998-02-15 LMB  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1998-01-07 SKN  NEW DEPOSIT REPORT COLUMNS REQ8636             *
      * 2004-06-20 JLM  NEW LATE-FEE REPORT COLUMNS PRB9457            *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SDBMTHE.
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
           SELECT REPORT-FILE ASSIGN TO "SDBMTRPT"
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
                                           VALUE 13.000000.
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
      * WS-LATE-FEE-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-LATE-FEE-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000173000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000407500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001100000000078500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011100000000499000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011600000000354500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014100000000853000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00024100000000482000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00024200000000413500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00024700000000861500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025200000000778500'.
       01  WS-LATE-FEE-SCHED-TBL REDEFINES WS-LATE-FEE-SCHED-VALUES.
           05  WS-LATE-FEE-SCHED-ROW OCCURS 10 TIMES.
               10  WS-LATE-FEE-SCHED-LOW   PIC 9(09)V99.
               10  WS-LATE-FEE-SCHED-RATE  PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-DEPOSIT-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-DEPOSIT-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000620000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000744500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005500000000170500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006000000000318000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007000000000427000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017000000000762000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017500000000208000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017600000000443500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020100000000629500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020600000000613000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021600000000721000'.
       01  WS-DEPOSIT-BAND-TBL REDEFINES WS-DEPOSIT-BAND-VALUES.
           05  WS-DEPOSIT-BAND-ROW OCCURS 11 TIMES.
               10  WS-DEPOSIT-BAND-LOW     PIC 9(09)V99.
               10  WS-DEPOSIT-BAND-RATE    PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  SDBMTHE  SAFE DEPOSIT BOXES'.
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
               PERFORM 3000-CHECK-EFF
               PERFORM 3010-DESCRIBE-CHANNEL
               PERFORM 3020-EDIT-MEMO
               PERFORM 3030-SET-REASON
               PERFORM 3040-SELECT-FEE-PCT
               PERFORM 3050-ACCUM-BOX-SIZE
               PERFORM 3060-SET-CYCLE
               PERFORM 3070-COUNT-RETRY
               PERFORM 3080-ASSESS-REVIEW
               PERFORM 3090-SET-BRANCH
               PERFORM 3100-CHECK-POST
               PERFORM 3110-SPLIT-LATE-FEE
               PERFORM 3120-ACCUM-VAULT
               PERFORM 3130-ACCUM-BOX-SIZE
               PERFORM 3140-SET-VAULT
               PERFORM 3150-SPLIT-DRILL
               PERFORM 3160-ASSESS-AUTO-RENEW
               PERFORM 3170-EDIT-REJECT
               PERFORM 3180-COUNT-RETRY
               PERFORM 3190-CHECK-POST
               PERFORM 3200-ACCRUE-DISCOUNT
               PERFORM 3210-SELECT-DISCOUNT
               PERFORM 3220-AGE-OPEN
               PERFORM 3230-COUNT-ITEM
               PERFORM 3240-COUNT-PAYMENT
               PERFORM 3250-AGE-OPEN
               PERFORM 3260-SPLIT-DEPOSIT
               PERFORM 3270-SET-VAULT
               PERFORM 3280-EDIT-NAME
               PERFORM 3290-FLAG-DRILL
               PERFORM 3300-SELECT-FEE-PCT
               PERFORM 3310-SET-SOURCE
               PERFORM 3320-SPLIT-DEPOSIT
               PERFORM 3330-ACCUM-CYCLE
               PERFORM 3340-EDIT-CUST-ID
               PERFORM 3350-SELECT-FEE-PCT
               PERFORM 3360-ACCUM-BRANCH
               PERFORM 3370-SET-REASON
               PERFORM 3380-EDIT-NAME
               PERFORM 3390-COUNT-NOTICE
               PERFORM 3400-SELECT-FEE-PCT
               PERFORM 3410-PRORATE-DRILL
               PERFORM 3420-AGE-NOTICE
               PERFORM 3430-COUNT-RETRY
               PERFORM 3440-CHECK-OPEN
               PERFORM 3450-AGE-LAST-ACT
               PERFORM 3460-AGE-MATURITY
               PERFORM 3470-LIMIT-LATE-FEE
               PERFORM 3480-PRORATE-KEY-FEE
               PERFORM 3490-ACCRUE-DRILL
               PERFORM 3500-CHECK-NEXT-DUE
               PERFORM 3510-LIMIT-PREPAID
               PERFORM 3520-ACCRUE-DEPOSIT
               PERFORM 3530-DESCRIBE-TRAN
               PERFORM 3540-PRORATE-PREPAID
               PERFORM 3550-AGE-POST
               PERFORM 3560-AGE-LAST-MAINT
               PERFORM 3570-PRORATE-DEPOSIT
               PERFORM 3580-SPLIT-KEY-FEE
               PERFORM 3590-SET-SOURCE
               PERFORM 3600-FLAG-RENT
               PERFORM 3610-AGE-NOTICE
               PERFORM 3620-PRORATE-DEPOSIT
               PERFORM 3630-SELECT-FEE-PCT
               PERFORM 3640-CHECK-MATURITY
               PERFORM 3650-AGE-LAST-MAINT
               PERFORM 3660-ASSESS-AUTO-RENEW
               PERFORM 3670-DESCRIBE-REASON
               PERFORM 3680-PRORATE-KEY-FEE
               PERFORM 3690-SELECT-PENALTY-PCT
               PERFORM 3700-COUNT-SEQ
               PERFORM 3710-SET-VAULT
               PERFORM 3720-ACCRUE-DRILL
               PERFORM 3730-ACCRUE-KEY-FEE
               PERFORM 3740-EDIT-CONTACT
               PERFORM 3750-AGE-POST
               PERFORM 3760-LIMIT-RENT
               PERFORM 3770-ACCUM-REASON
               PERFORM 3780-SET-SOURCE
               PERFORM 3790-ACCUM-BOX-SIZE
               PERFORM 3800-LIMIT-PREPAID
               PERFORM 3810-ACCRUE-PENALTY-PCT
               PERFORM 3820-ACCUM-BOX-SIZE
               PERFORM 3830-AGE-MATURITY
               PERFORM 3840-SET-TRAN
               PERFORM 3850-EDIT-CONTACT
               PERFORM 3860-COUNT-SEQ
               PERFORM 3870-ASSESS-REVERSAL
               PERFORM 3880-SPLIT-DRILL
               PERFORM 3890-SPLIT-KEY-FEE
               PERFORM 3900-ASSESS-AUTO-RENEW
               PERFORM 3910-PRORATE-KEY-FEE
               PERFORM 3920-AGE-LAST-ACT
               PERFORM 3930-CHECK-NEXT-DUE
               PERFORM 3940-PRORATE-LATE-FEE
               PERFORM 3950-CHECK-EFF
               PERFORM 3960-FLAG-ITEM
               PERFORM 3970-EDIT-CONTACT
               PERFORM 3980-COUNT-RETRY
               PERFORM 3990-FLAG-PREPAID
               PERFORM 4000-EDIT-CUST-ID
               PERFORM 4010-DESCRIBE-REASON
               PERFORM 4020-LIMIT-PREPAID
               PERFORM 4030-COUNT-PAYMENT
               PERFORM 4040-SELECT-DISCOUNT
               PERFORM 4050-ACCUM-REASON
               PERFORM 4060-PRORATE-KEY-FEE
               PERFORM 4070-SET-VAULT
               PERFORM 4080-ACCUM-SOURCE
               PERFORM 4090-ACCUM-BRANCH
               PERFORM 4100-FLAG-RENT
               PERFORM 4110-EDIT-MEMO
               PERFORM 4120-ASSESS-AUTO-RENEW
               PERFORM 4130-PRORATE-LATE-FEE
               PERFORM 4140-ACCRUE-RENT
               PERFORM 4150-SELECT-FEE-PCT
               PERFORM 4160-SET-BRANCH
               PERFORM 4170-ACCRUE-PREPAID
               PERFORM 4180-SET-BRANCH
               PERFORM 4190-DESCRIBE-BRANCH
               PERFORM 4200-AGE-LAST-STMT
               PERFORM 4210-SET-TRAN
               PERFORM 4220-LIMIT-DRILL
               PERFORM 4230-ASSESS-EXEMPT
               PERFORM 4240-ACCRUE-RENT
               PERFORM 4250-DESCRIBE-VAULT
               PERFORM 4260-ACCUM-TRAN
               PERFORM 4270-LIMIT-ITEM
               PERFORM 4280-ASSESS-REVERSAL
               PERFORM 4290-AGE-NOTICE
               PERFORM 4300-AGE-NEXT-REVIEW
               PERFORM 4310-ACCRUE-RENT
               PERFORM 4320-DESCRIBE-CYCLE
               PERFORM 4330-ACCUM-VAULT
               PERFORM 4340-CHECK-CLOSE
               PERFORM 4350-ACCRUE-RENT
               PERFORM 4360-DESCRIBE-BRANCH
               PERFORM 4370-EDIT-DESC
               PERFORM 4380-EDIT-NAME
               PERFORM 4390-ASSESS-AUTO-RENEW
               PERFORM 4400-SPLIT-KEY-FEE
               PERFORM 4410-ACCRUE-PREPAID
               PERFORM 4420-COUNT-NOTICE
               PERFORM 4430-SET-TRAN
               PERFORM 4440-DESCRIBE-REASON
               PERFORM 4450-ASSESS-AUTO-RENEW
               PERFORM 4460-CHECK-OPEN
               PERFORM 4470-PRORATE-KEY-FEE
               PERFORM 4480-FLAG-DRILL
               PERFORM 4490-AGE-CLOSE
               PERFORM 4500-FLAG-DRILL
               PERFORM 4510-CHECK-EFF
               PERFORM 4520-AGE-CLOSE
               PERFORM 4530-FLAG-LATE-FEE
               PERFORM 4540-SET-CYCLE
               PERFORM 4550-SET-CYCLE
               PERFORM 4560-SELECT-DISCOUNT
               PERFORM 4570-ACCRUE-RENT
               PERFORM 4580-SET-VAULT
               PERFORM 4590-EDIT-REF-NO
               PERFORM 4600-AGE-NEXT-REVIEW
               PERFORM 4610-FLAG-ITEM
               PERFORM 4620-ACCUM-BOX-SIZE
               PERFORM 4630-ACCUM-REASON
               PERFORM 4640-SPLIT-DEPOSIT
               PERFORM 4650-PRORATE-KEY-FEE
               PERFORM 4660-CHECK-MATURITY
               PERFORM 4670-DESCRIBE-BRANCH
               PERFORM 4680-SELECT-FEE-PCT
               PERFORM 4690-CHECK-LAST-STMT
               PERFORM 4700-COUNT-NOTICE
               PERFORM 4710-CHECK-LAST-ACT
               PERFORM 4720-COUNT-PAYMENT
               PERFORM 4730-DESCRIBE-VAULT
               PERFORM 4740-ACCUM-TRAN
               PERFORM 4750-SET-SOURCE
               PERFORM 4760-PRORATE-RENT
               PERFORM 4770-ACCUM-BOX-SIZE
               PERFORM 4780-CHECK-EFF
               PERFORM 4790-SPLIT-PREPAID
               PERFORM 4800-SPLIT-DRILL
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
      * DAYS SINCE EFF DATE. MORE THAN 30 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3000-CHECK-EFF.
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
           IF WS-DAYS > 30
               MOVE 'Y' TO SDBM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3010-DESCRIBE-CHANNEL.
           EVALUATE SDBT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E403 MEMO REQUIRED
      * E631 RENT YTD NEGATIVE
      * E609 RENT YTD OVER 99999
      * E770 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3020-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDBT-MEMO = SPACES
                   MOVE 'E403' TO WS-REJECT-CODE
               WHEN SDBM-RENT-YTD < 0
                   MOVE 'E631' TO WS-REJECT-CODE
               WHEN SDBM-RENT-YTD > 99999.00
                   MOVE 'E609' TO WS-REJECT-CODE
               WHEN SDBM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E770' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * J -> K WHEN OPEN DATE IS PAST
      * K -> L WHEN OPEN DATE IS PAST
      * L -> M WHEN OPEN DATE IS PAST
      * M -> S WHEN OPEN DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3030-SET-REASON.
           EVALUATE SDBM-REASON-CD
               WHEN 'J'
                   IF SDBM-OPEN-DATE > 0 AND SDBM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'K' TO SDBM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF SDBM-OPEN-DATE > 0 AND SDBM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO SDBM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF SDBM-OPEN-DATE > 0 AND SDBM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO SDBM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF SDBM-OPEN-DATE > 0 AND SDBM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO SDBM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE LATE FEE SCHED TIER FOR THE DRILL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3040-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-LATE-FEE-SCHED-RATE(1) TO SDBM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF SDBM-DRILL-AMT NOT < WS-LATE-FEE-SCHED-LOW(WS-TX)
                   MOVE WS-LATE-FEE-SCHED-RATE(WS-TX) TO SDBM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE RENT AMT BY BOX SIZE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3050-ACCUM-BOX-SIZE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDBM-BOX-SIZE-CD
                   ADD SDBT-RENT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDBM-BOX-SIZE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDBT-RENT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * AP -> DA WHEN NOTICE DATE IS PAST
      * DA -> HV WHEN NOTICE DATE IS PAST
      * HV -> SE WHEN NOTICE DATE IS PAST
      * SE -> AP WHEN NOTICE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3060-SET-CYCLE.
           EVALUATE SDBM-CYCLE-CD
               WHEN 'AP'
                   IF SDBM-NOTICE-DATE > 0 AND SDBM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DA' TO SDBM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DA'
                   IF SDBM-NOTICE-DATE > 0 AND SDBM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HV' TO SDBM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HV'
                   IF SDBM-NOTICE-DATE > 0 AND SDBM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SE' TO SDBM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SE'
                   IF SDBM-NOTICE-DATE > 0 AND SDBM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AP' TO SDBM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3070-COUNT-RETRY.
           IF SDBM-RETRY-CNT < 9999999
               ADD 1 TO SDBM-RETRY-CNT
           END-IF
           IF SDBM-RETRY-CNT NOT < 10
               MOVE 'Y' TO SDBM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE KEY FEE FEE ON THE WORK AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3080-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN SDBM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO SDBM-KEY-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDBM-OVERRIDE-FLAG = 'Y'
                   COMPUTE SDBM-KEY-FEE-AMT ROUNDED = WS-WORK-AMT *
                       SDBM-DISCOUNT / 200
               WHEN SDBM-BOX-SIZE-CD = 'CU'
                   MOVE 12.00 TO SDBM-KEY-FEE-AMT
               WHEN SDBM-BOX-SIZE-CD = 'MG'
                   MOVE 10.00 TO SDBM-KEY-FEE-AMT
               WHEN OTHER
                   COMPUTE SDBM-KEY-FEE-AMT ROUNDED = WS-WORK-AMT *
                       SDBM-DISCOUNT / 100
           END-EVALUATE
           IF SDBM-KEY-FEE-AMT > 0 AND SDBM-KEY-FEE-AMT < 2.00
               MOVE 2.00 TO SDBM-KEY-FEE-AMT
           END-IF
           IF SDBM-KEY-FEE-AMT > 150.00
               MOVE 150.00 TO SDBM-KEY-FEE-AMT
           END-IF
           ADD SDBM-KEY-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * EXS -> FJW WHEN LAST ACT DATE IS PAST
      * FJW -> YFB WHEN LAST ACT DATE IS PAST
      * YFB -> EXS WHEN LAST ACT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3090-SET-BRANCH.
           EVALUATE SDBM-BRANCH-CD
               WHEN 'EXS'
                   IF SDBM-LAST-ACT-DATE > 0 AND SDBM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FJW' TO SDBM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FJW'
                   IF SDBM-LAST-ACT-DATE > 0 AND SDBM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YFB' TO SDBM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YFB'
                   IF SDBM-LAST-ACT-DATE > 0 AND SDBM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EXS' TO SDBM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 15 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3100-CHECK-POST.
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
               MOVE 'Y' TO SDBM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT LATE FEE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3110-SPLIT-LATE-FEE.
           COMPUTE WS-WORK-AMT = SDBM-LATE-FEE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = SDBM-LATE-FEE-AMT - (WS-WORK-AMT * 3)
           COMPUTE SDBM-KEY-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY VAULT CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3120-ACCUM-VAULT.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDBM-VAULT-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDBM-VAULT-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY BOX SIZE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3130-ACCUM-BOX-SIZE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDBM-BOX-SIZE-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDBM-BOX-SIZE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR VAULT CD:
      * BU -> ET WHEN NEXT DUE DATE IS PAST
      * ET -> FF WHEN NEXT DUE DATE IS PAST
      * FF -> NG WHEN NEXT DUE DATE IS PAST
      * NG -> NH WHEN NEXT DUE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3140-SET-VAULT.
           EVALUATE SDBM-VAULT-CD
               WHEN 'BU'
                   IF SDBM-NEXT-DUE-DATE > 0 AND SDBM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ET' TO SDBM-VAULT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ET'
                   IF SDBM-NEXT-DUE-DATE > 0 AND SDBM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FF' TO SDBM-VAULT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FF'
                   IF SDBM-NEXT-DUE-DATE > 0 AND SDBM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NG' TO SDBM-VAULT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NG'
                   IF SDBM-NEXT-DUE-DATE > 0 AND SDBM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NH' TO SDBM-VAULT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT DRILL YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3150-SPLIT-DRILL.
           COMPUTE WS-WORK-AMT = SDBM-DRILL-YTD / 4
           COMPUTE WS-WORK-AMT-2 = SDBM-DRILL-YTD - (WS-WORK-AMT * 4)
           COMPUTE SDBM-LATE-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RENT FEE ON THE DRILL YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3160-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN SDBM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO SDBM-RENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDBM-SUSPEND-FLAG = 'Y'
                   COMPUTE SDBM-RENT-AMT ROUNDED = SDBM-DRILL-YTD *
                       SDBM-DISCOUNT / 200
               WHEN SDBM-BOX-SIZE-CD = 'CU'
                   MOVE 10.00 TO SDBM-RENT-AMT
               WHEN SDBM-BOX-SIZE-CD = 'MG'
                   MOVE 3.00 TO SDBM-RENT-AMT
               WHEN OTHER
                   COMPUTE SDBM-RENT-AMT ROUNDED = SDBM-DRILL-YTD *
                       SDBM-DISCOUNT / 100
           END-EVALUATE
           IF SDBM-RENT-AMT > 0 AND SDBM-RENT-AMT < 5.00
               MOVE 5.00 TO SDBM-RENT-AMT
           END-IF
           IF SDBM-RENT-AMT > 25.00
               MOVE 25.00 TO SDBM-RENT-AMT
           END-IF
           ADD SDBM-RENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E924 REJECT CD REQUIRED
      * E472 DEPOSIT AMT NEGATIVE
      * E952 DEPOSIT AMT OVER 50000
      * E235 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3170-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDBT-REJECT-CD = SPACES
                   MOVE 'E924' TO WS-REJECT-CODE
               WHEN SDBM-DEPOSIT-AMT < 0
                   MOVE 'E472' TO WS-REJECT-CODE
               WHEN SDBM-DEPOSIT-AMT > 50000.00
                   MOVE 'E952' TO WS-REJECT-CODE
               WHEN SDBM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E235' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3180-COUNT-RETRY.
           IF SDBM-RETRY-CNT < 9999999
               ADD 1 TO SDBM-RETRY-CNT
           END-IF
           IF SDBM-RETRY-CNT NOT < 12
               MOVE 'Y' TO SDBM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 60 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3190-CHECK-POST.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO SDBM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3200-ACCRUE-DISCOUNT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * SDBM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO SDBM-DEPOSIT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DEPOSIT BAND TIER FOR THE PREPAID AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3210-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-DEPOSIT-BAND-RATE(1) TO SDBM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF SDBM-PREPAID-AMT NOT < WS-DEPOSIT-BAND-LOW(WS-TX)
                   MOVE WS-DEPOSIT-BAND-RATE(WS-TX) TO SDBM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3220-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF SDBM-OPEN-DATE > 0 AND SDBM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDBM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SDBM-RENT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD SDBM-RENT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD SDBM-RENT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SDBM-RENT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDBM-RENT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3230-COUNT-ITEM.
           IF SDBM-ITEM-CNT < 9999999
               ADD 1 TO SDBM-ITEM-CNT
           END-IF
           IF SDBM-ITEM-CNT NOT < 5
               MOVE 'Y' TO SDBM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3240-COUNT-PAYMENT.
           IF SDBM-PAYMENT-CNT < 9999999
               ADD 1 TO SDBM-PAYMENT-CNT
           END-IF
           IF SDBM-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO SDBM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-90, 91-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3250-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF SDBM-OPEN-DATE > 0 AND SDBM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDBM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SDBT-RENT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD SDBT-RENT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD SDBT-RENT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SDBT-RENT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDBT-RENT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT DEPOSIT AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3260-SPLIT-DEPOSIT.
           COMPUTE WS-WORK-AMT = SDBM-DEPOSIT-AMT / 4
           COMPUTE WS-WORK-AMT-2 = SDBM-DEPOSIT-AMT - (WS-WORK-AMT * 4)
           COMPUTE SDBM-DRILL-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR VAULT CD:
      * BU -> ET WHEN NEXT DUE DATE IS PAST
      * ET -> FF WHEN NEXT DUE DATE IS PAST
      * FF -> NG WHEN NEXT DUE DATE IS PAST
      * NG -> NH WHEN NEXT DUE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3270-SET-VAULT.
           EVALUATE SDBM-VAULT-CD
               WHEN 'BU'
                   IF SDBM-NEXT-DUE-DATE > 0 AND SDBM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ET' TO SDBM-VAULT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ET'
                   IF SDBM-NEXT-DUE-DATE > 0 AND SDBM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FF' TO SDBM-VAULT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FF'
                   IF SDBM-NEXT-DUE-DATE > 0 AND SDBM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NG' TO SDBM-VAULT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NG'
                   IF SDBM-NEXT-DUE-DATE > 0 AND SDBM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NH' TO SDBM-VAULT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E929 NAME REQUIRED
      * E313 RENT AMT NEGATIVE
      * E962 RENT AMT OVER 10000
      * E883 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3280-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDBM-NAME = SPACES
                   MOVE 'E929' TO WS-REJECT-CODE
               WHEN SDBM-RENT-AMT < 0
                   MOVE 'E313' TO WS-REJECT-CODE
               WHEN SDBM-RENT-AMT > 10000.00
                   MOVE 'E962' TO WS-REJECT-CODE
               WHEN SDBM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E883' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DRILL AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3290-FLAG-DRILL.
           IF SDBM-DRILL-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE SDBM-DRILL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DEPOSIT BAND TIER FOR THE RENT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3300-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DEPOSIT-BAND-RATE(1) TO SDBM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF SDBM-RENT-YTD NOT < WS-DEPOSIT-BAND-LOW(WS-TX)
                   MOVE WS-DEPOSIT-BAND-RATE(WS-TX) TO SDBM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * DD -> JZ WHEN MATURITY DATE IS PAST
      * JZ -> PS WHEN MATURITY DATE IS PAST
      * PS -> UM WHEN MATURITY DATE IS PAST
      * UM -> DD WHEN MATURITY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3310-SET-SOURCE.
           EVALUATE SDBM-SOURCE-CD
               WHEN 'DD'
                   IF SDBM-MATURITY-DATE > 0 AND SDBM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JZ' TO SDBM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JZ'
                   IF SDBM-MATURITY-DATE > 0 AND SDBM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PS' TO SDBM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PS'
                   IF SDBM-MATURITY-DATE > 0 AND SDBM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UM' TO SDBM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UM'
                   IF SDBM-MATURITY-DATE > 0 AND SDBM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DD' TO SDBM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT DEPOSIT AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3320-SPLIT-DEPOSIT.
           COMPUTE WS-WORK-AMT = SDBT-DEPOSIT-AMT / 2
           COMPUTE WS-WORK-AMT-2 = SDBT-DEPOSIT-AMT - (WS-WORK-AMT * 2)
           COMPUTE SDBM-RENT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE DEPOSIT AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3330-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDBM-CYCLE-CD
                   ADD SDBM-DEPOSIT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDBM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDBM-DEPOSIT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E670 CUST ID REQUIRED
      * E946 KEY FEE YTD NEGATIVE
      * E604 KEY FEE YTD OVER 5000
      * E936 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3340-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDBM-CUST-ID = SPACES
                   MOVE 'E670' TO WS-REJECT-CODE
               WHEN SDBM-KEY-FEE-YTD < 0
                   MOVE 'E946' TO WS-REJECT-CODE
               WHEN SDBM-KEY-FEE-YTD > 5000.00
                   MOVE 'E604' TO WS-REJECT-CODE
               WHEN SDBM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E936' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE LATE FEE SCHED TIER FOR THE KEY FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3350-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-LATE-FEE-SCHED-RATE(1) TO SDBM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF SDBM-KEY-FEE-YTD NOT < WS-LATE-FEE-SCHED-LOW(WS-TX)
                   MOVE WS-LATE-FEE-SCHED-RATE(WS-TX) TO SDBM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE KEY FEE YTD BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3360-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDBM-BRANCH-CD
                   ADD SDBM-KEY-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDBM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDBM-KEY-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * J -> K WHEN OPEN DATE IS PAST
      * K -> L WHEN OPEN DATE IS PAST
      * L -> M WHEN OPEN DATE IS PAST
      * M -> S WHEN OPEN DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3370-SET-REASON.
           EVALUATE SDBM-REASON-CD
               WHEN 'J'
                   IF SDBM-OPEN-DATE > 0 AND SDBM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'K' TO SDBM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF SDBM-OPEN-DATE > 0 AND SDBM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO SDBM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF SDBM-OPEN-DATE > 0 AND SDBM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO SDBM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF SDBM-OPEN-DATE > 0 AND SDBM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO SDBM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E778 NAME REQUIRED
      * E554 RENT AMT NEGATIVE
      * E558 RENT AMT OVER 1000000
      * E803 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3380-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDBM-NAME = SPACES
                   MOVE 'E778' TO WS-REJECT-CODE
               WHEN SDBM-RENT-AMT < 0
                   MOVE 'E554' TO WS-REJECT-CODE
               WHEN SDBM-RENT-AMT > 1000000.00
                   MOVE 'E558' TO WS-REJECT-CODE
               WHEN SDBM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E803' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3390-COUNT-NOTICE.
           IF SDBM-NOTICE-CNT < 9999999
               ADD 1 TO SDBM-NOTICE-CNT
           END-IF
           IF SDBM-NOTICE-CNT NOT < 3
               MOVE 'Y' TO SDBT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE LATE FEE SCHED TIER FOR THE LATE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3400-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-LATE-FEE-SCHED-RATE(1) TO SDBM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF SDBM-LATE-FEE-AMT NOT < WS-LATE-FEE-SCHED-LOW(WS-TX)
                   MOVE WS-LATE-FEE-SCHED-RATE(WS-TX) TO SDBM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE RENT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DRILL YTD.
      *----------------------------------------------------------------*
       3410-PRORATE-DRILL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDBM-DRILL-YTD ROUNDED =
                   SDBM-RENT-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SDBM-DRILL-YTD
           END-IF
           ADD SDBM-DRILL-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3420-AGE-NOTICE.
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
                   ADD SDBM-RENT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD SDBM-RENT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD SDBM-RENT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD SDBM-RENT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDBM-RENT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3430-COUNT-RETRY.
           IF SDBM-RETRY-CNT < 9999999
               ADD 1 TO SDBM-RETRY-CNT
           END-IF
           IF SDBM-RETRY-CNT NOT < 3
               MOVE 'Y' TO SDBM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 45 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3440-CHECK-OPEN.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO SDBM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3450-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF SDBM-LAST-ACT-DATE > 0 AND SDBM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDBM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SDBM-KEY-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD SDBM-KEY-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD SDBM-KEY-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SDBM-KEY-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDBM-KEY-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3460-AGE-MATURITY.
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
                   ADD SDBM-KEY-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD SDBM-KEY-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD SDBM-KEY-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD SDBM-KEY-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDBM-KEY-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * LATE FEE AMT IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3470-LIMIT-LATE-FEE.
           IF SDBM-LATE-FEE-AMT < 50.00
               MOVE 50.00 TO SDBM-LATE-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SDBM-LATE-FEE-AMT > 2500.00
               MOVE 2500.00 TO SDBM-LATE-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DEPOSIT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO KEY FEE AMT.
      *----------------------------------------------------------------*
       3480-PRORATE-KEY-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDBM-KEY-FEE-AMT ROUNDED =
                   SDBM-DEPOSIT-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SDBM-KEY-FEE-AMT
           END-IF
           ADD SDBM-KEY-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DRILL AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3490-ACCRUE-DRILL.
           IF SDBT-DRILL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBT-DRILL-AMT * SDBM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO SDBM-KEY-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3500-CHECK-NEXT-DUE.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO SDBT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PREPAID YTD IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3510-LIMIT-PREPAID.
           IF SDBM-PREPAID-YTD < 0.00
               MOVE 0.00 TO SDBM-PREPAID-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SDBM-PREPAID-YTD > 2500.00
               MOVE 2500.00 TO SDBM-PREPAID-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DEPOSIT YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3520-ACCRUE-DEPOSIT.
           IF SDBM-DEPOSIT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBM-DEPOSIT-YTD * SDBM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3530-DESCRIBE-TRAN.
           EVALUATE SDBT-TRAN-CD
               WHEN 'RN'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'AC'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'DR'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'KY'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'CL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE DEPOSIT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PREPAID AMT.
      *----------------------------------------------------------------*
       3540-PRORATE-PREPAID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDBM-PREPAID-AMT ROUNDED =
                   SDBT-DEPOSIT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SDBM-PREPAID-AMT
           END-IF
           ADD SDBM-PREPAID-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3550-AGE-POST.
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
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3560-AGE-LAST-MAINT.
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
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE KEY FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO DEPOSIT AMT.
      *----------------------------------------------------------------*
       3570-PRORATE-DEPOSIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDBT-DEPOSIT-AMT ROUNDED =
                   SDBM-KEY-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SDBT-DEPOSIT-AMT
           END-IF
           ADD SDBT-DEPOSIT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT KEY FEE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3580-SPLIT-KEY-FEE.
           COMPUTE WS-WORK-AMT = SDBM-KEY-FEE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = SDBM-KEY-FEE-AMT - (WS-WORK-AMT * 3)
           COMPUTE SDBM-PREPAID-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * DD -> JZ WHEN OPEN DATE IS PAST
      * JZ -> PS WHEN OPEN DATE IS PAST
      * PS -> UM WHEN OPEN DATE IS PAST
      * UM -> DD WHEN OPEN DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3590-SET-SOURCE.
           EVALUATE SDBM-SOURCE-CD
               WHEN 'DD'
                   IF SDBM-OPEN-DATE > 0 AND SDBM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JZ' TO SDBM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JZ'
                   IF SDBM-OPEN-DATE > 0 AND SDBM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PS' TO SDBM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PS'
                   IF SDBM-OPEN-DATE > 0 AND SDBM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UM' TO SDBM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UM'
                   IF SDBM-OPEN-DATE > 0 AND SDBM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DD' TO SDBM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RENT AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3600-FLAG-RENT.
           IF SDBM-RENT-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE SDBM-RENT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3610-AGE-NOTICE.
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
                   ADD SDBM-RENT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD SDBM-RENT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD SDBM-RENT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SDBM-RENT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDBM-RENT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE DEPOSIT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3620-PRORATE-DEPOSIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   SDBT-DEPOSIT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE LATE FEE SCHED TIER FOR THE KEY FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3630-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-LATE-FEE-SCHED-RATE(1) TO SDBM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF SDBM-KEY-FEE-YTD NOT < WS-LATE-FEE-SCHED-LOW(WS-TX)
                   MOVE WS-LATE-FEE-SCHED-RATE(WS-TX) TO SDBM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3640-CHECK-MATURITY.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO SDBM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3650-AGE-LAST-MAINT.
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
                   ADD SDBM-PREPAID-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD SDBM-PREPAID-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD SDBM-PREPAID-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD SDBM-PREPAID-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDBM-PREPAID-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE RENT FEE ON THE DEPOSIT YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3660-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN SDBM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO SDBT-RENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDBM-SUSPEND-FLAG = 'Y'
                   COMPUTE SDBT-RENT-AMT ROUNDED = SDBM-DEPOSIT-YTD *
                       SDBM-FEE-PCT / 200
               WHEN SDBT-CHANNEL-CD = 'BRN'
                   MOVE 15.00 TO SDBT-RENT-AMT
               WHEN SDBT-CHANNEL-CD = 'BAT'
                   MOVE 15.00 TO SDBT-RENT-AMT
               WHEN OTHER
                   COMPUTE SDBT-RENT-AMT ROUNDED = SDBM-DEPOSIT-YTD *
                       SDBM-FEE-PCT / 100
           END-EVALUATE
           IF SDBT-RENT-AMT > 0 AND SDBT-RENT-AMT < 5.00
               MOVE 5.00 TO SDBT-RENT-AMT
           END-IF
           IF SDBT-RENT-AMT > 25.00
               MOVE 25.00 TO SDBT-RENT-AMT
           END-IF
           ADD SDBT-RENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3670-DESCRIBE-REASON.
           EVALUATE SDBM-REASON-CD
               WHEN 'J'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'K'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'L'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'M'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'S'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'W'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'Y'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE DEPOSIT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO KEY FEE AMT.
      *----------------------------------------------------------------*
       3680-PRORATE-KEY-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDBT-KEY-FEE-AMT ROUNDED =
                   SDBM-DEPOSIT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SDBT-KEY-FEE-AMT
           END-IF
           ADD SDBT-KEY-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE LATE FEE SCHED TIER FOR THE LATE FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3690-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-LATE-FEE-SCHED-RATE(1) TO SDBM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF SDBM-LATE-FEE-YTD NOT < WS-LATE-FEE-SCHED-LOW(WS-TX)
                   MOVE WS-LATE-FEE-SCHED-RATE(WS-TX) TO
                       SDBM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3700-COUNT-SEQ.
           IF SDBT-SEQ < 9999999
               ADD 1 TO SDBT-SEQ
           END-IF
           IF SDBT-SEQ NOT < 6
               MOVE 'Y' TO SDBM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR VAULT CD:
      * BU -> ET WHEN LAST MAINT DATE IS PAST
      * ET -> FF WHEN LAST MAINT DATE IS PAST
      * FF -> NG WHEN LAST MAINT DATE IS PAST
      * NG -> NH WHEN LAST MAINT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3710-SET-VAULT.
           EVALUATE SDBM-VAULT-CD
               WHEN 'BU'
                   IF SDBM-LAST-MAINT-DATE > 0 AND SDBM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'ET' TO SDBM-VAULT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ET'
                   IF SDBM-LAST-MAINT-DATE > 0 AND SDBM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'FF' TO SDBM-VAULT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FF'
                   IF SDBM-LAST-MAINT-DATE > 0 AND SDBM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'NG' TO SDBM-VAULT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NG'
                   IF SDBM-LAST-MAINT-DATE > 0 AND SDBM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'NH' TO SDBM-VAULT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DRILL YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3720-ACCRUE-DRILL.
           IF SDBM-DRILL-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBM-DRILL-YTD * SDBM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO SDBM-RENT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON KEY FEE AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3730-ACCRUE-KEY-FEE.
           IF SDBM-KEY-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBM-KEY-FEE-AMT * SDBM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO SDBM-DEPOSIT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E592 CONTACT REQUIRED
      * E645 DEPOSIT AMT NEGATIVE
      * E424 DEPOSIT AMT OVER 5000
      * E867 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3740-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDBM-CONTACT = SPACES
                   MOVE 'E592' TO WS-REJECT-CODE
               WHEN SDBM-DEPOSIT-AMT < 0
                   MOVE 'E645' TO WS-REJECT-CODE
               WHEN SDBM-DEPOSIT-AMT > 5000.00
                   MOVE 'E424' TO WS-REJECT-CODE
               WHEN SDBM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E867' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3750-AGE-POST.
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
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RENT AMT IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3760-LIMIT-RENT.
           IF SDBT-RENT-AMT < 0.00
               MOVE 0.00 TO SDBT-RENT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SDBT-RENT-AMT > 1000.00
               MOVE 1000.00 TO SDBT-RENT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE DEPOSIT AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3770-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDBM-REASON-CD
                   ADD SDBT-DEPOSIT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDBM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDBT-DEPOSIT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * DD -> JZ WHEN LAST STMT DATE IS PAST
      * JZ -> PS WHEN LAST STMT DATE IS PAST
      * PS -> UM WHEN LAST STMT DATE IS PAST
      * UM -> DD WHEN LAST STMT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3780-SET-SOURCE.
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
      * ACCUMULATE KEY FEE AMT BY BOX SIZE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3790-ACCUM-BOX-SIZE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDBM-BOX-SIZE-CD
                   ADD SDBT-KEY-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDBM-BOX-SIZE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDBT-KEY-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PREPAID AMT IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3800-LIMIT-PREPAID.
           IF SDBM-PREPAID-AMT < 0.00
               MOVE 0.00 TO SDBM-PREPAID-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SDBM-PREPAID-AMT > 2500.00
               MOVE 2500.00 TO SDBM-PREPAID-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3810-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * SDBM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO SDBM-LATE-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE KEY FEE YTD BY BOX SIZE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3820-ACCUM-BOX-SIZE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDBM-BOX-SIZE-CD
                   ADD SDBM-KEY-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDBM-BOX-SIZE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDBM-KEY-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3830-AGE-MATURITY.
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
                   ADD SDBM-DEPOSIT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD SDBM-DEPOSIT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD SDBM-DEPOSIT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD SDBM-DEPOSIT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDBM-DEPOSIT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * RN -> AC WHEN LAST ACT DATE IS PAST
      * AC -> DR WHEN LAST ACT DATE IS PAST
      * DR -> KY WHEN LAST ACT DATE IS PAST
      * KY -> CL WHEN LAST ACT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3840-SET-TRAN.
           EVALUATE SDBT-TRAN-CD
               WHEN 'RN'
                   IF SDBM-LAST-ACT-DATE > 0 AND SDBM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AC' TO SDBT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AC'
                   IF SDBM-LAST-ACT-DATE > 0 AND SDBM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DR' TO SDBT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DR'
                   IF SDBM-LAST-ACT-DATE > 0 AND SDBM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KY' TO SDBT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KY'
                   IF SDBM-LAST-ACT-DATE > 0 AND SDBM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CL' TO SDBT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E782 CONTACT REQUIRED
      * E339 DEPOSIT YTD NEGATIVE
      * E765 DEPOSIT YTD OVER 1000000
      * E142 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3850-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDBM-CONTACT = SPACES
                   MOVE 'E782' TO WS-REJECT-CODE
               WHEN SDBM-DEPOSIT-YTD < 0
                   MOVE 'E339' TO WS-REJECT-CODE
               WHEN SDBM-DEPOSIT-YTD > 1000000.00
                   MOVE 'E765' TO WS-REJECT-CODE
               WHEN SDBM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E142' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3860-COUNT-SEQ.
           IF SDBT-SEQ < 9999999
               ADD 1 TO SDBT-SEQ
           END-IF
           IF SDBT-SEQ NOT < 6
               MOVE 'Y' TO SDBM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE LATE FEE YTD FEE ON THE KEY FEE AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3870-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN SDBT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO SDBM-LATE-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDBM-EXEMPT-FLAG = 'Y'
                   COMPUTE SDBM-LATE-FEE-YTD ROUNDED = SDBM-KEY-FEE-AMT
                       * SDBM-PENALTY-PCT / 200
               WHEN SDBM-VAULT-CD = 'BU'
                   MOVE 15.00 TO SDBM-LATE-FEE-YTD
               WHEN SDBM-VAULT-CD = 'ET'
                   MOVE 3.00 TO SDBM-LATE-FEE-YTD
               WHEN OTHER
                   COMPUTE SDBM-LATE-FEE-YTD ROUNDED = SDBM-KEY-FEE-AMT
                       * SDBM-PENALTY-PCT / 100
           END-EVALUATE
           IF SDBM-LATE-FEE-YTD > 0 AND SDBM-LATE-FEE-YTD < 2.00
               MOVE 2.00 TO SDBM-LATE-FEE-YTD
           END-IF
           IF SDBM-LATE-FEE-YTD > 25.00
               MOVE 25.00 TO SDBM-LATE-FEE-YTD
           END-IF
           ADD SDBM-LATE-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT DRILL AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3880-SPLIT-DRILL.
           COMPUTE WS-WORK-AMT = SDBM-DRILL-AMT / 3
           COMPUTE WS-WORK-AMT-2 = SDBM-DRILL-AMT - (WS-WORK-AMT * 3)
           COMPUTE SDBM-LATE-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT KEY FEE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3890-SPLIT-KEY-FEE.
           COMPUTE WS-WORK-AMT = SDBM-KEY-FEE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = SDBM-KEY-FEE-AMT - (WS-WORK-AMT * 12)
           COMPUTE SDBT-DRILL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE KEY FEE YTD FEE ON THE RENT AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3900-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN SDBM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO SDBM-KEY-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDBM-SUSPEND-FLAG = 'Y'
                   COMPUTE SDBM-KEY-FEE-YTD ROUNDED = SDBM-RENT-AMT *
                       SDBM-FEE-PCT / 200
               WHEN SDBM-VAULT-CD = 'BU'
                   MOVE 7.00 TO SDBM-KEY-FEE-YTD
               WHEN SDBM-VAULT-CD = 'ET'
                   MOVE 7.00 TO SDBM-KEY-FEE-YTD
               WHEN OTHER
                   COMPUTE SDBM-KEY-FEE-YTD ROUNDED = SDBM-RENT-AMT *
                       SDBM-FEE-PCT / 100
           END-EVALUATE
           IF SDBM-KEY-FEE-YTD > 0 AND SDBM-KEY-FEE-YTD < 1.00
               MOVE 1.00 TO SDBM-KEY-FEE-YTD
           END-IF
           IF SDBM-KEY-FEE-YTD > 50.00
               MOVE 50.00 TO SDBM-KEY-FEE-YTD
           END-IF
           ADD SDBM-KEY-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO KEY FEE AMT.
      *----------------------------------------------------------------*
       3910-PRORATE-KEY-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDBT-KEY-FEE-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SDBT-KEY-FEE-AMT
           END-IF
           ADD SDBT-KEY-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3920-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF SDBM-LAST-ACT-DATE > 0 AND SDBM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE SDBM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD SDBM-PREPAID-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD SDBM-PREPAID-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD SDBM-PREPAID-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SDBM-PREPAID-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDBM-PREPAID-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3930-CHECK-NEXT-DUE.
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
               MOVE 'Y' TO SDBM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DRILL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO LATE FEE YTD.
      *----------------------------------------------------------------*
       3940-PRORATE-LATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDBM-LATE-FEE-YTD ROUNDED =
                   SDBM-DRILL-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SDBM-LATE-FEE-YTD
           END-IF
           ADD SDBM-LATE-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 15 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3950-CHECK-EFF.
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
           IF WS-DAYS > 15
               MOVE 'Y' TO SDBM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3960-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E902 CONTACT REQUIRED
      * E290 LATE FEE YTD NEGATIVE
      * E721 LATE FEE YTD OVER 1000000
      * E770 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3970-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDBM-CONTACT = SPACES
                   MOVE 'E902' TO WS-REJECT-CODE
               WHEN SDBM-LATE-FEE-YTD < 0
                   MOVE 'E290' TO WS-REJECT-CODE
               WHEN SDBM-LATE-FEE-YTD > 1000000.00
                   MOVE 'E721' TO WS-REJECT-CODE
               WHEN SDBM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E770' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3980-COUNT-RETRY.
           IF SDBM-RETRY-CNT < 9999999
               ADD 1 TO SDBM-RETRY-CNT
           END-IF
           IF SDBM-RETRY-CNT NOT < 10
               MOVE 'Y' TO SDBM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PREPAID YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3990-FLAG-PREPAID.
           IF SDBM-PREPAID-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE SDBM-PREPAID-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E744 CUST ID REQUIRED
      * E741 WORK AMT 2 NEGATIVE
      * E166 WORK AMT 2 OVER 99999
      * E949 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       4000-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDBM-CUST-ID = SPACES
                   MOVE 'E744' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E741' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 99999.00
                   MOVE 'E166' TO WS-REJECT-CODE
               WHEN SDBM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E949' TO WS-REJECT-CODE
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
       4010-DESCRIBE-REASON.
           EVALUATE SDBM-REASON-CD
               WHEN 'J'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'K'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'L'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'M'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'S'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'W'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'Y'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PREPAID YTD IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4020-LIMIT-PREPAID.
           IF SDBM-PREPAID-YTD < 0.00
               MOVE 0.00 TO SDBM-PREPAID-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SDBM-PREPAID-YTD > 1000.00
               MOVE 1000.00 TO SDBM-PREPAID-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       4030-COUNT-PAYMENT.
           IF SDBM-PAYMENT-CNT < 9999999
               ADD 1 TO SDBM-PAYMENT-CNT
           END-IF
           IF SDBM-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO SDBM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DEPOSIT BAND TIER FOR THE LATE FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4040-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-DEPOSIT-BAND-RATE(1) TO SDBM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF SDBM-LATE-FEE-YTD NOT < WS-DEPOSIT-BAND-LOW(WS-TX)
                   MOVE WS-DEPOSIT-BAND-RATE(WS-TX) TO SDBM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE RENT YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4050-ACCUM-REASON.
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
      * PRORATE RENT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO KEY FEE AMT.
      *----------------------------------------------------------------*
       4060-PRORATE-KEY-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDBM-KEY-FEE-AMT ROUNDED =
                   SDBM-RENT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SDBM-KEY-FEE-AMT
           END-IF
           ADD SDBM-KEY-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR VAULT CD:
      * BU -> ET WHEN LAST STMT DATE IS PAST
      * ET -> FF WHEN LAST STMT DATE IS PAST
      * FF -> NG WHEN LAST STMT DATE IS PAST
      * NG -> NH WHEN LAST STMT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4070-SET-VAULT.
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
      * ACCUMULATE PREPAID AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4080-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDBM-SOURCE-CD
                   ADD SDBM-PREPAID-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDBM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDBM-PREPAID-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE LATE FEE AMT BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4090-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDBM-BRANCH-CD
                   ADD SDBM-LATE-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDBM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDBM-LATE-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * RENT AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4100-FLAG-RENT.
           IF SDBT-RENT-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE SDBT-RENT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E554 MEMO REQUIRED
      * E907 DRILL AMT NEGATIVE
      * E005 DRILL AMT OVER 5000
      * E113 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       4110-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDBT-MEMO = SPACES
                   MOVE 'E554' TO WS-REJECT-CODE
               WHEN SDBT-DRILL-AMT < 0
                   MOVE 'E907' TO WS-REJECT-CODE
               WHEN SDBT-DRILL-AMT > 5000.00
                   MOVE 'E005' TO WS-REJECT-CODE
               WHEN SDBM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E113' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RENT YTD FEE ON THE PREPAID AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4120-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN SDBM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO SDBM-RENT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDBM-REVIEW-FLAG = 'Y'
                   COMPUTE SDBM-RENT-YTD ROUNDED = SDBM-PREPAID-AMT *
                       SDBM-PENALTY-PCT / 200
               WHEN SDBT-CHANNEL-CD = 'BRN'
                   MOVE 15.00 TO SDBM-RENT-YTD
               WHEN SDBT-CHANNEL-CD = 'BAT'
                   MOVE 5.00 TO SDBM-RENT-YTD
               WHEN OTHER
                   COMPUTE SDBM-RENT-YTD ROUNDED = SDBM-PREPAID-AMT *
                       SDBM-PENALTY-PCT / 100
           END-EVALUATE
           IF SDBM-RENT-YTD > 0 AND SDBM-RENT-YTD < 2.00
               MOVE 2.00 TO SDBM-RENT-YTD
           END-IF
           IF SDBM-RENT-YTD > 35.00
               MOVE 35.00 TO SDBM-RENT-YTD
           END-IF
           ADD SDBM-RENT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO LATE FEE YTD.
      *----------------------------------------------------------------*
       4130-PRORATE-LATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDBM-LATE-FEE-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SDBM-LATE-FEE-YTD
           END-IF
           ADD SDBM-LATE-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RENT YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4140-ACCRUE-RENT.
           IF SDBM-RENT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBM-RENT-YTD * SDBM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE LATE FEE SCHED TIER FOR THE KEY FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4150-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-LATE-FEE-SCHED-RATE(1) TO SDBM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF SDBM-KEY-FEE-YTD NOT < WS-LATE-FEE-SCHED-LOW(WS-TX)
                   MOVE WS-LATE-FEE-SCHED-RATE(WS-TX) TO SDBM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * EXS -> FJW WHEN LAST MAINT DATE IS PAST
      * FJW -> YFB WHEN LAST MAINT DATE IS PAST
      * YFB -> EXS WHEN LAST MAINT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4160-SET-BRANCH.
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
      * DAILY INTEREST ON PREPAID AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4170-ACCRUE-PREPAID.
           IF SDBM-PREPAID-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBM-PREPAID-AMT * SDBM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO SDBT-RENT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * EXS -> FJW WHEN MATURITY DATE IS PAST
      * FJW -> YFB WHEN MATURITY DATE IS PAST
      * YFB -> EXS WHEN MATURITY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4180-SET-BRANCH.
           EVALUATE SDBM-BRANCH-CD
               WHEN 'EXS'
                   IF SDBM-MATURITY-DATE > 0 AND SDBM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FJW' TO SDBM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FJW'
                   IF SDBM-MATURITY-DATE > 0 AND SDBM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YFB' TO SDBM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YFB'
                   IF SDBM-MATURITY-DATE > 0 AND SDBM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EXS' TO SDBM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4190-DESCRIBE-BRANCH.
           EVALUATE SDBM-BRANCH-CD
               WHEN 'EXS'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'FJW'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'YFB'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       4200-AGE-LAST-STMT.
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
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * RN -> AC WHEN MATURITY DATE IS PAST
      * AC -> DR WHEN MATURITY DATE IS PAST
      * DR -> KY WHEN MATURITY DATE IS PAST
      * KY -> CL WHEN MATURITY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4210-SET-TRAN.
           EVALUATE SDBT-TRAN-CD
               WHEN 'RN'
                   IF SDBM-MATURITY-DATE > 0 AND SDBM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AC' TO SDBT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AC'
                   IF SDBM-MATURITY-DATE > 0 AND SDBM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DR' TO SDBT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DR'
                   IF SDBM-MATURITY-DATE > 0 AND SDBM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KY' TO SDBT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KY'
                   IF SDBM-MATURITY-DATE > 0 AND SDBM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CL' TO SDBT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DRILL YTD IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4220-LIMIT-DRILL.
           IF SDBM-DRILL-YTD < 25.00
               MOVE 25.00 TO SDBM-DRILL-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SDBM-DRILL-YTD > 10000.00
               MOVE 10000.00 TO SDBM-DRILL-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE RENT AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4230-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN SDBM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDBM-OVERRIDE-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = SDBT-RENT-AMT *
                       SDBM-DISCOUNT / 200
               WHEN SDBM-VAULT-CD = 'BU'
                   MOVE 3.00 TO WS-WORK-AMT-2
               WHEN SDBM-VAULT-CD = 'ET'
                   MOVE 15.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = SDBT-RENT-AMT *
                       SDBM-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 5.00
               MOVE 5.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 25.00
               MOVE 25.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RENT AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4240-ACCRUE-RENT.
           IF SDBT-RENT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBT-RENT-AMT * SDBM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO SDBM-DEPOSIT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE VAULT CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4250-DESCRIBE-VAULT.
           EVALUATE SDBM-VAULT-CD
               WHEN 'BU'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'ET'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'FF'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'NG'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'NH'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE PREPAID AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4260-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDBT-TRAN-CD
                   ADD SDBM-PREPAID-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDBT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDBM-PREPAID-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4270-LIMIT-ITEM.
           IF WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 50000.00
               MOVE 50000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DEPOSIT YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4280-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN SDBT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO SDBM-DEPOSIT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDBM-SUSPEND-FLAG = 'Y'
                   COMPUTE SDBM-DEPOSIT-YTD ROUNDED = WS-WORK-AMT-2 *
                       SDBM-FEE-PCT / 200
               WHEN SDBM-REASON-CD = 'J'
                   MOVE 7.00 TO SDBM-DEPOSIT-YTD
               WHEN SDBM-REASON-CD = 'K'
                   MOVE 10.00 TO SDBM-DEPOSIT-YTD
               WHEN OTHER
                   COMPUTE SDBM-DEPOSIT-YTD ROUNDED = WS-WORK-AMT-2 *
                       SDBM-FEE-PCT / 100
           END-EVALUATE
           IF SDBM-DEPOSIT-YTD > 0 AND SDBM-DEPOSIT-YTD < 5.00
               MOVE 5.00 TO SDBM-DEPOSIT-YTD
           END-IF
           IF SDBM-DEPOSIT-YTD > 100.00
               MOVE 100.00 TO SDBM-DEPOSIT-YTD
           END-IF
           ADD SDBM-DEPOSIT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4290-AGE-NOTICE.
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
                   ADD SDBM-DRILL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD SDBM-DRILL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD SDBM-DRILL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SDBM-DRILL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDBM-DRILL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4300-AGE-NEXT-REVIEW.
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
      * DAILY INTEREST ON RENT AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4310-ACCRUE-RENT.
           IF SDBM-RENT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBM-RENT-AMT * SDBM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4320-DESCRIBE-CYCLE.
           EVALUATE SDBM-CYCLE-CD
               WHEN 'AP'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'DA'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'HV'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'SE'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE DEPOSIT AMT BY VAULT CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4330-ACCUM-VAULT.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDBM-VAULT-CD
                   ADD SDBM-DEPOSIT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDBM-VAULT-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDBM-DEPOSIT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4340-CHECK-CLOSE.
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
           IF WS-DAYS > 30
               MOVE 'Y' TO SDBT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RENT AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4350-ACCRUE-RENT.
           IF SDBT-RENT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBT-RENT-AMT * SDBM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO SDBM-LATE-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4360-DESCRIBE-BRANCH.
           EVALUATE SDBM-BRANCH-CD
               WHEN 'EXS'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'FJW'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'YFB'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E842 DESC REQUIRED
      * E664 RENT YTD NEGATIVE
      * E219 RENT YTD OVER 10000
      * E056 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       4370-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDBM-DESC = SPACES
                   MOVE 'E842' TO WS-REJECT-CODE
               WHEN SDBM-RENT-YTD < 0
                   MOVE 'E664' TO WS-REJECT-CODE
               WHEN SDBM-RENT-YTD > 10000.00
                   MOVE 'E219' TO WS-REJECT-CODE
               WHEN SDBT-EFF-DATE > CTL-NEXT-BUS-DATE
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
      * E747 NAME REQUIRED
      * E996 DEPOSIT YTD NEGATIVE
      * E881 DEPOSIT YTD OVER 10000
      * E146 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       4380-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDBM-NAME = SPACES
                   MOVE 'E747' TO WS-REJECT-CODE
               WHEN SDBM-DEPOSIT-YTD < 0
                   MOVE 'E996' TO WS-REJECT-CODE
               WHEN SDBM-DEPOSIT-YTD > 10000.00
                   MOVE 'E881' TO WS-REJECT-CODE
               WHEN SDBM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E146' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE LATE FEE FEE ON THE LATE FEE YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       4390-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN SDBM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO SDBM-LATE-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDBM-OVERRIDE-FLAG = 'Y'
                   COMPUTE SDBM-LATE-FEE-AMT ROUNDED = SDBM-LATE-FEE-YTD
                       * SDBM-FEE-PCT / 200
               WHEN SDBM-BRANCH-CD = 'EXS'
                   MOVE 12.00 TO SDBM-LATE-FEE-AMT
               WHEN SDBM-BRANCH-CD = 'FJW'
                   MOVE 12.00 TO SDBM-LATE-FEE-AMT
               WHEN OTHER
                   COMPUTE SDBM-LATE-FEE-AMT ROUNDED = SDBM-LATE-FEE-YTD
                       * SDBM-FEE-PCT / 100
           END-EVALUATE
           IF SDBM-LATE-FEE-AMT > 0 AND SDBM-LATE-FEE-AMT < 1.00
               MOVE 1.00 TO SDBM-LATE-FEE-AMT
           END-IF
           IF SDBM-LATE-FEE-AMT > 250.00
               MOVE 250.00 TO SDBM-LATE-FEE-AMT
           END-IF
           ADD SDBM-LATE-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT KEY FEE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4400-SPLIT-KEY-FEE.
           COMPUTE WS-WORK-AMT = SDBT-KEY-FEE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = SDBT-KEY-FEE-AMT - (WS-WORK-AMT * 3)
           COMPUTE SDBM-LATE-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PREPAID YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4410-ACCRUE-PREPAID.
           IF SDBM-PREPAID-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBM-PREPAID-YTD * SDBM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO SDBM-RENT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4420-COUNT-NOTICE.
           IF SDBM-NOTICE-CNT < 9999999
               ADD 1 TO SDBM-NOTICE-CNT
           END-IF
           IF SDBM-NOTICE-CNT NOT < 12
               MOVE 'Y' TO SDBM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * RN -> AC WHEN LAST ACT DATE IS PAST
      * AC -> DR WHEN LAST ACT DATE IS PAST
      * DR -> KY WHEN LAST ACT DATE IS PAST
      * KY -> CL WHEN LAST ACT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4430-SET-TRAN.
           EVALUATE SDBT-TRAN-CD
               WHEN 'RN'
                   IF SDBM-LAST-ACT-DATE > 0 AND SDBM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AC' TO SDBT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AC'
                   IF SDBM-LAST-ACT-DATE > 0 AND SDBM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DR' TO SDBT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DR'
                   IF SDBM-LAST-ACT-DATE > 0 AND SDBM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KY' TO SDBT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KY'
                   IF SDBM-LAST-ACT-DATE > 0 AND SDBM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CL' TO SDBT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4440-DESCRIBE-REASON.
           EVALUATE SDBM-REASON-CD
               WHEN 'J'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'K'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'L'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'M'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'S'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'W'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'Y'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE DRILL YTD FEE ON THE RENT AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4450-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN SDBM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO SDBM-DRILL-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDBM-REVIEW-FLAG = 'Y'
                   COMPUTE SDBM-DRILL-YTD ROUNDED = SDBT-RENT-AMT *
                       SDBM-DISCOUNT / 200
               WHEN SDBM-VAULT-CD = 'BU'
                   MOVE 15.00 TO SDBM-DRILL-YTD
               WHEN SDBM-VAULT-CD = 'ET'
                   MOVE 7.00 TO SDBM-DRILL-YTD
               WHEN OTHER
                   COMPUTE SDBM-DRILL-YTD ROUNDED = SDBT-RENT-AMT *
                       SDBM-DISCOUNT / 100
           END-EVALUATE
           IF SDBM-DRILL-YTD > 0 AND SDBM-DRILL-YTD < 5.00
               MOVE 5.00 TO SDBM-DRILL-YTD
           END-IF
           IF SDBM-DRILL-YTD > 25.00
               MOVE 25.00 TO SDBM-DRILL-YTD
           END-IF
           ADD SDBM-DRILL-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 10 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4460-CHECK-OPEN.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO SDBM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DRILL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO KEY FEE YTD.
      *----------------------------------------------------------------*
       4470-PRORATE-KEY-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDBM-KEY-FEE-YTD ROUNDED =
                   SDBT-DRILL-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SDBM-KEY-FEE-YTD
           END-IF
           ADD SDBM-KEY-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DRILL YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4480-FLAG-DRILL.
           IF SDBM-DRILL-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE SDBM-DRILL-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4490-AGE-CLOSE.
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
                   ADD SDBT-DRILL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD SDBT-DRILL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD SDBT-DRILL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD SDBT-DRILL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDBT-DRILL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DRILL AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4500-FLAG-DRILL.
           IF SDBM-DRILL-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE SDBM-DRILL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 45 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4510-CHECK-EFF.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO SDBM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       4520-AGE-CLOSE.
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
                   ADD SDBT-DEPOSIT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD SDBT-DEPOSIT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD SDBT-DEPOSIT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD SDBT-DEPOSIT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDBT-DEPOSIT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * LATE FEE YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4530-FLAG-LATE-FEE.
           IF SDBM-LATE-FEE-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE SDBM-LATE-FEE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * AP -> DA WHEN NEXT DUE DATE IS PAST
      * DA -> HV WHEN NEXT DUE DATE IS PAST
      * HV -> SE WHEN NEXT DUE DATE IS PAST
      * SE -> AP WHEN NEXT DUE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4540-SET-CYCLE.
           EVALUATE SDBM-CYCLE-CD
               WHEN 'AP'
                   IF SDBM-NEXT-DUE-DATE > 0 AND SDBM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DA' TO SDBM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DA'
                   IF SDBM-NEXT-DUE-DATE > 0 AND SDBM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HV' TO SDBM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HV'
                   IF SDBM-NEXT-DUE-DATE > 0 AND SDBM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SE' TO SDBM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SE'
                   IF SDBM-NEXT-DUE-DATE > 0 AND SDBM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AP' TO SDBM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * AP -> DA WHEN LAST MAINT DATE IS PAST
      * DA -> HV WHEN LAST MAINT DATE IS PAST
      * HV -> SE WHEN LAST MAINT DATE IS PAST
      * SE -> AP WHEN LAST MAINT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4550-SET-CYCLE.
           EVALUATE SDBM-CYCLE-CD
               WHEN 'AP'
                   IF SDBM-LAST-MAINT-DATE > 0 AND SDBM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'DA' TO SDBM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DA'
                   IF SDBM-LAST-MAINT-DATE > 0 AND SDBM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'HV' TO SDBM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HV'
                   IF SDBM-LAST-MAINT-DATE > 0 AND SDBM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'SE' TO SDBM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SE'
                   IF SDBM-LAST-MAINT-DATE > 0 AND SDBM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'AP' TO SDBM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE DEPOSIT BAND TIER FOR THE DRILL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4560-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-DEPOSIT-BAND-RATE(1) TO SDBM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF SDBT-DRILL-AMT NOT < WS-DEPOSIT-BAND-LOW(WS-TX)
                   MOVE WS-DEPOSIT-BAND-RATE(WS-TX) TO SDBM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RENT YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4570-ACCRUE-RENT.
           IF SDBM-RENT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBM-RENT-YTD * SDBM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO SDBM-DEPOSIT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR VAULT CD:
      * BU -> ET WHEN LAST ACT DATE IS PAST
      * ET -> FF WHEN LAST ACT DATE IS PAST
      * FF -> NG WHEN LAST ACT DATE IS PAST
      * NG -> NH WHEN LAST ACT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4580-SET-VAULT.
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
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E443 REF NO REQUIRED
      * E387 KEY FEE YTD NEGATIVE
      * E287 KEY FEE YTD OVER 25000
      * E823 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       4590-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDBM-REF-NO = SPACES
                   MOVE 'E443' TO WS-REJECT-CODE
               WHEN SDBM-KEY-FEE-YTD < 0
                   MOVE 'E387' TO WS-REJECT-CODE
               WHEN SDBM-KEY-FEE-YTD > 25000.00
                   MOVE 'E287' TO WS-REJECT-CODE
               WHEN SDBM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E823' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4600-AGE-NEXT-REVIEW.
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
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4610-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE KEY FEE YTD BY BOX SIZE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4620-ACCUM-BOX-SIZE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDBM-BOX-SIZE-CD
                   ADD SDBM-KEY-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDBM-BOX-SIZE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDBM-KEY-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PREPAID YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4630-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDBM-REASON-CD
                   ADD SDBM-PREPAID-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDBM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDBM-PREPAID-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DEPOSIT YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4640-SPLIT-DEPOSIT.
           COMPUTE WS-WORK-AMT = SDBM-DEPOSIT-YTD / 12
           COMPUTE WS-WORK-AMT-2 = SDBM-DEPOSIT-YTD - (WS-WORK-AMT * 12)
           COMPUTE SDBT-DRILL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE KEY FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       4650-PRORATE-KEY-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   SDBM-KEY-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 10 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4660-CHECK-MATURITY.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO SDBM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4670-DESCRIBE-BRANCH.
           EVALUATE SDBM-BRANCH-CD
               WHEN 'EXS'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'FJW'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'YFB'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE DEPOSIT BAND TIER FOR THE DRILL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4680-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DEPOSIT-BAND-RATE(1) TO SDBM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF SDBT-DRILL-AMT NOT < WS-DEPOSIT-BAND-LOW(WS-TX)
                   MOVE WS-DEPOSIT-BAND-RATE(WS-TX) TO SDBM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 10 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4690-CHECK-LAST-STMT.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO SDBM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       4700-COUNT-NOTICE.
           IF SDBM-NOTICE-CNT < 9999999
               ADD 1 TO SDBM-NOTICE-CNT
           END-IF
           IF SDBM-NOTICE-CNT NOT < 6
               MOVE 'Y' TO SDBM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 90 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4710-CHECK-LAST-ACT.
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
           IF WS-DAYS > 90
               MOVE 'Y' TO SDBT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4720-COUNT-PAYMENT.
           IF SDBM-PAYMENT-CNT < 9999999
               ADD 1 TO SDBM-PAYMENT-CNT
           END-IF
           IF SDBM-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO SDBM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE VAULT CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4730-DESCRIBE-VAULT.
           EVALUATE SDBM-VAULT-CD
               WHEN 'BU'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'ET'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'FF'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'NG'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'NH'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE DRILL AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4740-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDBT-TRAN-CD
                   ADD SDBT-DRILL-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDBT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDBT-DRILL-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * DD -> JZ WHEN LAST ACT DATE IS PAST
      * JZ -> PS WHEN LAST ACT DATE IS PAST
      * PS -> UM WHEN LAST ACT DATE IS PAST
      * UM -> DD WHEN LAST ACT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4750-SET-SOURCE.
           EVALUATE SDBM-SOURCE-CD
               WHEN 'DD'
                   IF SDBM-LAST-ACT-DATE > 0 AND SDBM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JZ' TO SDBM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JZ'
                   IF SDBM-LAST-ACT-DATE > 0 AND SDBM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PS' TO SDBM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PS'
                   IF SDBM-LAST-ACT-DATE > 0 AND SDBM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UM' TO SDBM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UM'
                   IF SDBM-LAST-ACT-DATE > 0 AND SDBM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DD' TO SDBM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE DEPOSIT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO RENT AMT.
      *----------------------------------------------------------------*
       4760-PRORATE-RENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDBT-RENT-AMT ROUNDED =
                   SDBM-DEPOSIT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SDBT-RENT-AMT
           END-IF
           ADD SDBT-RENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE DEPOSIT AMT BY BOX SIZE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4770-ACCUM-BOX-SIZE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDBM-BOX-SIZE-CD
                   ADD SDBM-DEPOSIT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDBM-BOX-SIZE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDBM-DEPOSIT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 10 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4780-CHECK-EFF.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO SDBM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4790-SPLIT-PREPAID.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 2)
           COMPUTE SDBM-PREPAID-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4800-SPLIT-DRILL.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 3)
           COMPUTE SDBT-DRILL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
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
                   MOVE SDLIM-RENT-YTD
                     TO SDBM-RENT-YTD
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
           DISPLAY 'SDBMTHE READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'SDBMTHE' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
