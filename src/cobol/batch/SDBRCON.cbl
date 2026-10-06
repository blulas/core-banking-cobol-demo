      *================================================================*
      * PROGRAM  : SDBRCON                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : SAFE DEPOSIT BOXES - RECONCILIATION. READS THE SDB  *
      *            TRANSACTIONS, UPDATES THE SDB MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. SAFE DEPOSIT BOX RENTAL BILLING,*
      *            ACCESS LOG AND DRILLING                             *
      * INPUT    : SDBMST (KSDS), SDBTRN, BANKCTL                      *
      * OUTPUT   : SDBOUT, SDBRCRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, SDVAL, SDFEE, SDLIM     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1993-08-11 LMB  ORIGINAL                                       *
      * 2001-02-10 SKN  DEPOSIT TABLE EXPANDED CHG0734                 *
      * 2002-01-23 TWH  DEPOSIT EXCEPTIONS TO OPS QUEUE REQ8162        *
      * 2002-07-21 CRP  DRILL NOTICE TEXT PER LEGAL CHG3543            *
      * 2003-02-20 GAF  AUDIT FINDING - PREPAID CONTROL TOTALS INC2453 *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SDBRCON.
       AUTHOR. M K LOWE.

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
           SELECT REPORT-FILE ASSIGN TO "SDBRCRPT"
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
                                           VALUE 22.000000.
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
      * WS-KEY-FEE-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-KEY-FEE-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000192500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000850500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002750000000062500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003250000000059500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013250000000403500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018250000000855000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018500000000406500'.
       01  WS-KEY-FEE-GRID-TBL REDEFINES WS-KEY-FEE-GRID-VALUES.
           05  WS-KEY-FEE-GRID-ROW OCCURS 7 TIMES.
               10  WS-KEY-FEE-GRID-LOW     PIC 9(09)V99.
               10  WS-KEY-FEE-GRID-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-DEPOSIT-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-DEPOSIT-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000068500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000239000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015000000000834500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015050000000656500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015100000000837000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015600000000550500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015650000000238500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020650000000527000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020750000000337000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021000000000537500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023500000000843000'.
       01  WS-DEPOSIT-SCHED-TBL REDEFINES WS-DEPOSIT-SCHED-VALUES.
           05  WS-DEPOSIT-SCHED-ROW OCCURS 11 TIMES.
               10  WS-DEPOSIT-SCHED-LOW    PIC 9(09)V99.
               10  WS-DEPOSIT-SCHED-RATE   PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  SDBRCON  SAFE DEPOSIT BOXES'.
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
       COPY SDVALP.
       COPY SDFEEP.
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
               PERFORM 3000-DESCRIBE-BOX-SIZE
               PERFORM 3010-SET-VAULT
               PERFORM 3020-PRORATE-PREPAID
               PERFORM 3030-SPLIT-DRILL
               PERFORM 3040-SPLIT-PREPAID
               PERFORM 3050-DESCRIBE-BOX-SIZE
               PERFORM 3060-FLAG-RENT
               PERFORM 3070-ACCRUE-RENT
               PERFORM 3080-SELECT-FEE-PCT
               PERFORM 3090-ACCRUE-PENALTY-PCT
               PERFORM 3100-SELECT-DISCOUNT
               PERFORM 3110-ASSESS-REVIEW
               PERFORM 3120-SET-REASON
               PERFORM 3130-AGE-MATURITY
               PERFORM 3140-SELECT-DISCOUNT
               PERFORM 3150-DESCRIBE-VAULT
               PERFORM 3160-DESCRIBE-BOX-SIZE
               PERFORM 3170-LIMIT-RENT
               PERFORM 3180-FLAG-KEY-FEE
               PERFORM 3190-AGE-LAST-MAINT
               PERFORM 3200-EDIT-ADDR
               PERFORM 3210-ACCRUE-KEY-FEE
               PERFORM 3220-EDIT-REF-NO
               PERFORM 3230-SPLIT-RENT
               PERFORM 3240-PRORATE-KEY-FEE
               PERFORM 3250-CHECK-LAST-MAINT
               PERFORM 3260-PRORATE-LATE-FEE
               PERFORM 3270-FLAG-PREPAID
               PERFORM 3280-AGE-POST
               PERFORM 3290-DESCRIBE-TRAN
               PERFORM 3300-COUNT-SEQ
               PERFORM 3310-SELECT-FEE-PCT
               PERFORM 3320-AGE-POST
               PERFORM 3330-LIMIT-PREPAID
               PERFORM 3340-COUNT-RETRY
               PERFORM 3350-ACCUM-SOURCE
               PERFORM 3360-ASSESS-EXEMPT
               PERFORM 3370-ACCUM-TRAN
               PERFORM 3380-CHECK-OPEN
               PERFORM 3390-ACCUM-BOX-SIZE
               PERFORM 3400-CHECK-LAST-STMT
               PERFORM 3410-AGE-NEXT-DUE
               PERFORM 3420-SELECT-PENALTY-PCT
               PERFORM 3430-ACCUM-CHANNEL
               PERFORM 3440-COUNT-RETRY
               PERFORM 3450-PRORATE-RENT
               PERFORM 3460-SET-TRAN
               PERFORM 3470-COUNT-RETRY
               PERFORM 3480-LIMIT-DEPOSIT
               PERFORM 3490-AGE-EFF
               PERFORM 3500-LIMIT-RENT
               PERFORM 3510-COUNT-ITEM
               PERFORM 3520-COUNT-ITEM
               PERFORM 3530-SELECT-PENALTY-PCT
               PERFORM 3540-DESCRIBE-VAULT
               PERFORM 3550-EDIT-MEMO
               PERFORM 3560-DESCRIBE-TRAN
               PERFORM 3570-ACCRUE-KEY-FEE
               PERFORM 3580-DESCRIBE-BOX-SIZE
               PERFORM 3590-SPLIT-DRILL
               PERFORM 3600-AGE-NEXT-REVIEW
               PERFORM 3610-ASSESS-AUTO-RENEW
               PERFORM 3620-FLAG-ITEM
               PERFORM 3630-CHECK-MATURITY
               PERFORM 3640-LIMIT-KEY-FEE
               PERFORM 3650-PRORATE-KEY-FEE
               PERFORM 3660-SET-BRANCH
               PERFORM 3670-ACCRUE-DISCOUNT
               PERFORM 3680-SELECT-PENALTY-PCT
               PERFORM 3690-DESCRIBE-CHANNEL
               PERFORM 3700-EDIT-NAME
               PERFORM 3710-SPLIT-KEY-FEE
               PERFORM 3720-SPLIT-LATE-FEE
               PERFORM 3730-AGE-LAST-STMT
               PERFORM 3740-AGE-CLOSE
               PERFORM 3750-SPLIT-KEY-FEE
               PERFORM 3760-DESCRIBE-REASON
               PERFORM 3770-AGE-NEXT-DUE
               PERFORM 3780-ACCUM-VAULT
               PERFORM 3790-PRORATE-DEPOSIT
               PERFORM 3800-COUNT-ITEM
               PERFORM 3810-SET-REASON
               PERFORM 3820-CHECK-LAST-MAINT
               PERFORM 3830-EDIT-OPERATOR
               PERFORM 3840-SPLIT-DEPOSIT
               PERFORM 3850-LIMIT-DEPOSIT
               PERFORM 3860-ACCUM-SOURCE
               PERFORM 3870-LIMIT-LATE-FEE
               PERFORM 3880-FLAG-RENT
               PERFORM 3890-PRORATE-DRILL
               PERFORM 3900-CHECK-POST
               PERFORM 3910-FLAG-RENT
               PERFORM 3920-LIMIT-RENT
               PERFORM 3930-SPLIT-LATE-FEE
               PERFORM 3940-EDIT-NAME
               PERFORM 3950-ACCUM-CYCLE
               PERFORM 3960-ACCRUE-KEY-FEE
               PERFORM 3970-ACCRUE-DRILL
               PERFORM 3980-SELECT-FEE-PCT
               PERFORM 3990-SPLIT-ITEM
               PERFORM 4000-SELECT-DISCOUNT
               PERFORM 4010-SELECT-DISCOUNT
               PERFORM 4020-SPLIT-RENT
               PERFORM 4030-AGE-EFF
               PERFORM 4040-AGE-LAST-STMT
               PERFORM 4050-SET-REASON
               PERFORM 4060-ACCRUE-DISCOUNT
               PERFORM 4070-DESCRIBE-REASON
               PERFORM 4080-SPLIT-RENT
               PERFORM 4090-ACCUM-TRAN
               PERFORM 4100-ASSESS-EXEMPT
               PERFORM 4110-LIMIT-ITEM
               PERFORM 4120-COUNT-RETRY
               PERFORM 4130-LIMIT-DRILL
               PERFORM 4140-AGE-LAST-MAINT
               PERFORM 4150-PRORATE-RENT
               PERFORM 4160-FLAG-PREPAID
               PERFORM 4170-ACCRUE-PREPAID
               PERFORM 4180-COUNT-RETRY
               PERFORM 4190-AGE-LAST-STMT
               PERFORM 4200-SPLIT-PREPAID
               PERFORM 4210-LIMIT-DEPOSIT
               PERFORM 4220-DESCRIBE-BRANCH
               PERFORM 4230-ACCRUE-DRILL
               PERFORM 4240-CHECK-EFF
               PERFORM 4250-COUNT-RETRY
               PERFORM 4260-DESCRIBE-BRANCH
               PERFORM 4270-SPLIT-PREPAID
               PERFORM 4280-FLAG-DRILL
               PERFORM 4290-PRORATE-KEY-FEE
               PERFORM 4300-LIMIT-DEPOSIT
               PERFORM 4310-ASSESS-SUSPEND
               PERFORM 4320-DESCRIBE-CYCLE
               PERFORM 4330-EDIT-CONTACT
               PERFORM 4340-ACCUM-TRAN
               PERFORM 4350-SPLIT-LATE-FEE
               PERFORM 4360-ACCRUE-KEY-FEE
               PERFORM 4370-EDIT-CONTACT
               PERFORM 4380-PRORATE-DRILL
               PERFORM 4390-ASSESS-OVERRIDE
               PERFORM 4400-FLAG-RENT
               PERFORM 7000-CALL-SDVAL
               PERFORM 7030-CALL-SDFEE
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
      * TRANSLATE BOX SIZE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3000-DESCRIBE-BOX-SIZE.
           EVALUATE SDBM-BOX-SIZE-CD
               WHEN 'CU'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'MG'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'VG'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'VN'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'XJ'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR VAULT CD:
      * BU -> ET WHEN POST DATE IS PAST
      * ET -> FF WHEN POST DATE IS PAST
      * FF -> NG WHEN POST DATE IS PAST
      * NG -> NH WHEN POST DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3010-SET-VAULT.
           EVALUATE SDBM-VAULT-CD
               WHEN 'BU'
                   IF SDBT-POST-DATE > 0 AND SDBT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ET' TO SDBM-VAULT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ET'
                   IF SDBT-POST-DATE > 0 AND SDBT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FF' TO SDBM-VAULT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FF'
                   IF SDBT-POST-DATE > 0 AND SDBT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NG' TO SDBM-VAULT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NG'
                   IF SDBT-POST-DATE > 0 AND SDBT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NH' TO SDBM-VAULT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PREPAID YTD.
      *----------------------------------------------------------------*
       3020-PRORATE-PREPAID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDBM-PREPAID-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SDBM-PREPAID-YTD
           END-IF
           ADD SDBM-PREPAID-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT DRILL AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3030-SPLIT-DRILL.
           COMPUTE WS-WORK-AMT = SDBM-DRILL-AMT / 3
           COMPUTE WS-WORK-AMT-2 = SDBM-DRILL-AMT - (WS-WORK-AMT * 3)
           COMPUTE SDBM-RENT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PREPAID AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3040-SPLIT-PREPAID.
           COMPUTE WS-WORK-AMT = SDBM-PREPAID-AMT / 4
           COMPUTE WS-WORK-AMT-2 = SDBM-PREPAID-AMT - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BOX SIZE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3050-DESCRIBE-BOX-SIZE.
           EVALUATE SDBM-BOX-SIZE-CD
               WHEN 'CU'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'MG'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'VG'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'VN'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'XJ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RENT YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3060-FLAG-RENT.
           IF SDBM-RENT-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE SDBM-RENT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RENT AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3070-ACCRUE-RENT.
           IF SDBT-RENT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBT-RENT-AMT * SDBM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO SDBM-RENT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE KEY FEE GRID TIER FOR THE DEPOSIT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3080-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-KEY-FEE-GRID-RATE(1) TO SDBM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF SDBT-DEPOSIT-AMT NOT < WS-KEY-FEE-GRID-LOW(WS-TX)
                   MOVE WS-KEY-FEE-GRID-RATE(WS-TX) TO SDBM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3090-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * SDBM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO SDBM-RENT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE KEY FEE GRID TIER FOR THE LATE FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3100-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-KEY-FEE-GRID-RATE(1) TO SDBM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF SDBM-LATE-FEE-YTD NOT < WS-KEY-FEE-GRID-LOW(WS-TX)
                   MOVE WS-KEY-FEE-GRID-RATE(WS-TX) TO SDBM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE LATE FEE YTD FEE ON THE RENT AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3110-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN SDBM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO SDBM-LATE-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDBM-EXEMPT-FLAG = 'Y'
                   COMPUTE SDBM-LATE-FEE-YTD ROUNDED = SDBM-RENT-AMT *
                       SDBM-PENALTY-PCT / 200
               WHEN SDBM-BRANCH-CD = 'EXS'
                   MOVE 20.00 TO SDBM-LATE-FEE-YTD
               WHEN SDBM-BRANCH-CD = 'FJW'
                   MOVE 7.00 TO SDBM-LATE-FEE-YTD
               WHEN OTHER
                   COMPUTE SDBM-LATE-FEE-YTD ROUNDED = SDBM-RENT-AMT *
                       SDBM-PENALTY-PCT / 100
           END-EVALUATE
           IF SDBM-LATE-FEE-YTD > 0 AND SDBM-LATE-FEE-YTD < 1.00
               MOVE 1.00 TO SDBM-LATE-FEE-YTD
           END-IF
           IF SDBM-LATE-FEE-YTD > 100.00
               MOVE 100.00 TO SDBM-LATE-FEE-YTD
           END-IF
           ADD SDBM-LATE-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * J -> K WHEN CLOSE DATE IS PAST
      * K -> L WHEN CLOSE DATE IS PAST
      * L -> M WHEN CLOSE DATE IS PAST
      * M -> S WHEN CLOSE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3120-SET-REASON.
           EVALUATE SDBM-REASON-CD
               WHEN 'J'
                   IF SDBM-CLOSE-DATE > 0 AND SDBM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'K' TO SDBM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF SDBM-CLOSE-DATE > 0 AND SDBM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO SDBM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF SDBM-CLOSE-DATE > 0 AND SDBM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO SDBM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF SDBM-CLOSE-DATE > 0 AND SDBM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO SDBM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3130-AGE-MATURITY.
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
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE DEPOSIT SCHED TIER FOR THE LATE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3140-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-DEPOSIT-SCHED-RATE(1) TO SDBM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF SDBM-LATE-FEE-AMT NOT < WS-DEPOSIT-SCHED-LOW(WS-TX)
                   MOVE WS-DEPOSIT-SCHED-RATE(WS-TX) TO SDBM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE VAULT CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3150-DESCRIBE-VAULT.
           EVALUATE SDBM-VAULT-CD
               WHEN 'BU'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ET'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'FF'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'NG'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'NH'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE BOX SIZE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3160-DESCRIBE-BOX-SIZE.
           EVALUATE SDBM-BOX-SIZE-CD
               WHEN 'CU'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'MG'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'VG'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'VN'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'XJ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RENT YTD IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3170-LIMIT-RENT.
           IF SDBM-RENT-YTD < 10.00
               MOVE 10.00 TO SDBM-RENT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SDBM-RENT-YTD > 50000.00
               MOVE 50000.00 TO SDBM-RENT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * KEY FEE YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3180-FLAG-KEY-FEE.
           IF SDBM-KEY-FEE-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE SDBM-KEY-FEE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3190-AGE-LAST-MAINT.
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
               WHEN WS-DAYS NOT > 60
                   ADD SDBM-KEY-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD SDBM-KEY-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD SDBM-KEY-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDBM-KEY-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E197 ADDR REQUIRED
      * E819 DEPOSIT AMT NEGATIVE
      * E240 DEPOSIT AMT OVER 10000
      * E550 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3200-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDBM-ADDR = SPACES
                   MOVE 'E197' TO WS-REJECT-CODE
               WHEN SDBT-DEPOSIT-AMT < 0
                   MOVE 'E819' TO WS-REJECT-CODE
               WHEN SDBT-DEPOSIT-AMT > 10000.00
                   MOVE 'E240' TO WS-REJECT-CODE
               WHEN SDBM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E550' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON KEY FEE YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3210-ACCRUE-KEY-FEE.
           IF SDBM-KEY-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBM-KEY-FEE-YTD * SDBM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO SDBM-DRILL-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E761 REF NO REQUIRED
      * E065 PREPAID YTD NEGATIVE
      * E724 PREPAID YTD OVER 99999
      * E335 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3220-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDBM-REF-NO = SPACES
                   MOVE 'E761' TO WS-REJECT-CODE
               WHEN SDBM-PREPAID-YTD < 0
                   MOVE 'E065' TO WS-REJECT-CODE
               WHEN SDBM-PREPAID-YTD > 99999.00
                   MOVE 'E724' TO WS-REJECT-CODE
               WHEN SDBM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E335' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT RENT AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3230-SPLIT-RENT.
           COMPUTE WS-WORK-AMT = SDBM-RENT-AMT / 12
           COMPUTE WS-WORK-AMT-2 = SDBM-RENT-AMT - (WS-WORK-AMT * 12)
           COMPUTE SDBT-DRILL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DRILL YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO KEY FEE AMT.
      *----------------------------------------------------------------*
       3240-PRORATE-KEY-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDBM-KEY-FEE-AMT ROUNDED =
                   SDBM-DRILL-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SDBM-KEY-FEE-AMT
           END-IF
           ADD SDBM-KEY-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 60 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3250-CHECK-LAST-MAINT.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO SDBM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE LATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3260-PRORATE-LATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   SDBM-LATE-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PREPAID YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3270-FLAG-PREPAID.
           IF SDBM-PREPAID-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE SDBM-PREPAID-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3280-AGE-POST.
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
                   ADD SDBM-LATE-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD SDBM-LATE-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD SDBM-LATE-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD SDBM-LATE-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDBM-LATE-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3290-DESCRIBE-TRAN.
           EVALUATE SDBT-TRAN-CD
               WHEN 'RN'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'AC'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'DR'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'KY'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'CL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3300-COUNT-SEQ.
           IF SDBT-SEQ < 9999999
               ADD 1 TO SDBT-SEQ
           END-IF
           IF SDBT-SEQ NOT < 5
               MOVE 'Y' TO SDBM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DEPOSIT SCHED TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3310-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DEPOSIT-SCHED-RATE(1) TO SDBM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF WS-WORK-AMT NOT < WS-DEPOSIT-SCHED-LOW(WS-TX)
                   MOVE WS-DEPOSIT-SCHED-RATE(WS-TX) TO SDBM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-45, 46-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3320-AGE-POST.
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
                   ADD SDBM-PREPAID-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD SDBM-PREPAID-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD SDBM-PREPAID-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SDBM-PREPAID-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDBM-PREPAID-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PREPAID AMT IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3330-LIMIT-PREPAID.
           IF SDBM-PREPAID-AMT < 50.00
               MOVE 50.00 TO SDBM-PREPAID-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SDBM-PREPAID-AMT > 10000.00
               MOVE 10000.00 TO SDBM-PREPAID-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3340-COUNT-RETRY.
           IF SDBM-RETRY-CNT < 9999999
               ADD 1 TO SDBM-RETRY-CNT
           END-IF
           IF SDBM-RETRY-CNT NOT < 5
               MOVE 'Y' TO SDBT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PREPAID AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3350-ACCUM-SOURCE.
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
      * ASSESS THE DRILL FEE ON THE DRILL YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3360-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN SDBM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO SDBT-DRILL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDBM-REVIEW-FLAG = 'Y'
                   COMPUTE SDBT-DRILL-AMT ROUNDED = SDBM-DRILL-YTD *
                       SDBM-PENALTY-PCT / 200
               WHEN SDBM-REASON-CD = 'J'
                   MOVE 12.00 TO SDBT-DRILL-AMT
               WHEN SDBM-REASON-CD = 'K'
                   MOVE 5.00 TO SDBT-DRILL-AMT
               WHEN OTHER
                   COMPUTE SDBT-DRILL-AMT ROUNDED = SDBM-DRILL-YTD *
                       SDBM-PENALTY-PCT / 100
           END-EVALUATE
           IF SDBT-DRILL-AMT > 0 AND SDBT-DRILL-AMT < 10.00
               MOVE 10.00 TO SDBT-DRILL-AMT
           END-IF
           IF SDBT-DRILL-AMT > 35.00
               MOVE 35.00 TO SDBT-DRILL-AMT
           END-IF
           ADD SDBT-DRILL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE DRILL AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3370-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDBT-TRAN-CD
                   ADD SDBM-DRILL-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDBT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDBM-DRILL-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 365 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3380-CHECK-OPEN.
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
           IF WS-DAYS > 365
               MOVE 'Y' TO SDBT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PREPAID YTD BY BOX SIZE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3390-ACCUM-BOX-SIZE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDBM-BOX-SIZE-CD
                   ADD SDBM-PREPAID-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDBM-BOX-SIZE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDBM-PREPAID-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 45 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO SDBM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3410-AGE-NEXT-DUE.
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
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE DEPOSIT SCHED TIER FOR THE KEY FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3420-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DEPOSIT-SCHED-RATE(1) TO SDBM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF SDBT-KEY-FEE-AMT NOT < WS-DEPOSIT-SCHED-LOW(WS-TX)
                   MOVE WS-DEPOSIT-SCHED-RATE(WS-TX) TO SDBM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE DRILL AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3430-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDBT-CHANNEL-CD
                   ADD SDBM-DRILL-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDBT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDBM-DRILL-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3440-COUNT-RETRY.
           IF SDBM-RETRY-CNT < 9999999
               ADD 1 TO SDBM-RETRY-CNT
           END-IF
           IF SDBM-RETRY-CNT NOT < 10
               MOVE 'Y' TO SDBM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE RENT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO RENT AMT.
      *----------------------------------------------------------------*
       3450-PRORATE-RENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDBT-RENT-AMT ROUNDED =
                   SDBM-RENT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SDBT-RENT-AMT
           END-IF
           ADD SDBT-RENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * RN -> AC WHEN OPEN DATE IS PAST
      * AC -> DR WHEN OPEN DATE IS PAST
      * DR -> KY WHEN OPEN DATE IS PAST
      * KY -> CL WHEN OPEN DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3460-SET-TRAN.
           EVALUATE SDBT-TRAN-CD
               WHEN 'RN'
                   IF SDBM-OPEN-DATE > 0 AND SDBM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AC' TO SDBT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AC'
                   IF SDBM-OPEN-DATE > 0 AND SDBM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DR' TO SDBT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DR'
                   IF SDBM-OPEN-DATE > 0 AND SDBM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KY' TO SDBT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KY'
                   IF SDBM-OPEN-DATE > 0 AND SDBM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CL' TO SDBT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3470-COUNT-RETRY.
           IF SDBM-RETRY-CNT < 9999999
               ADD 1 TO SDBM-RETRY-CNT
           END-IF
           IF SDBM-RETRY-CNT NOT < 3
               MOVE 'Y' TO SDBM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DEPOSIT AMT IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3480-LIMIT-DEPOSIT.
           IF SDBM-DEPOSIT-AMT < 25.00
               MOVE 25.00 TO SDBM-DEPOSIT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SDBM-DEPOSIT-AMT > 10000.00
               MOVE 10000.00 TO SDBM-DEPOSIT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3490-AGE-EFF.
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
                   ADD SDBM-DRILL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD SDBM-DRILL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD SDBM-DRILL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD SDBM-DRILL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDBM-DRILL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RENT AMT IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3500-LIMIT-RENT.
           IF SDBM-RENT-AMT < 100.00
               MOVE 100.00 TO SDBM-RENT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SDBM-RENT-AMT > 1000.00
               MOVE 1000.00 TO SDBM-RENT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3510-COUNT-ITEM.
           IF SDBM-ITEM-CNT < 9999999
               ADD 1 TO SDBM-ITEM-CNT
           END-IF
           IF SDBM-ITEM-CNT NOT < 3
               MOVE 'Y' TO SDBM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3520-COUNT-ITEM.
           IF SDBM-ITEM-CNT < 9999999
               ADD 1 TO SDBM-ITEM-CNT
           END-IF
           IF SDBM-ITEM-CNT NOT < 6
               MOVE 'Y' TO SDBM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE KEY FEE GRID TIER FOR THE KEY FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3530-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-KEY-FEE-GRID-RATE(1) TO SDBM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF SDBM-KEY-FEE-AMT NOT < WS-KEY-FEE-GRID-LOW(WS-TX)
                   MOVE WS-KEY-FEE-GRID-RATE(WS-TX) TO SDBM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE VAULT CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3540-DESCRIBE-VAULT.
           EVALUATE SDBM-VAULT-CD
               WHEN 'BU'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'ET'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'FF'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'NG'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'NH'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E318 MEMO REQUIRED
      * E497 DRILL AMT NEGATIVE
      * E280 DRILL AMT OVER 25000
      * E070 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3550-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDBT-MEMO = SPACES
                   MOVE 'E318' TO WS-REJECT-CODE
               WHEN SDBT-DRILL-AMT < 0
                   MOVE 'E497' TO WS-REJECT-CODE
               WHEN SDBT-DRILL-AMT > 25000.00
                   MOVE 'E280' TO WS-REJECT-CODE
               WHEN SDBM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E070' TO WS-REJECT-CODE
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
       3560-DESCRIBE-TRAN.
           EVALUATE SDBT-TRAN-CD
               WHEN 'RN'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'AC'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'DR'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'KY'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'CL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON KEY FEE YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3570-ACCRUE-KEY-FEE.
           IF SDBM-KEY-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBM-KEY-FEE-YTD * SDBM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BOX SIZE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3580-DESCRIBE-BOX-SIZE.
           EVALUATE SDBM-BOX-SIZE-CD
               WHEN 'CU'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'MG'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'VG'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'VN'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'XJ'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT DRILL AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3590-SPLIT-DRILL.
           COMPUTE WS-WORK-AMT = SDBT-DRILL-AMT / 4
           COMPUTE WS-WORK-AMT-2 = SDBT-DRILL-AMT - (WS-WORK-AMT * 4)
           COMPUTE SDBM-LATE-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3600-AGE-NEXT-REVIEW.
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
               WHEN WS-DAYS NOT > 45
                   ADD SDBM-PREPAID-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD SDBM-PREPAID-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDBM-PREPAID-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE RENT FEE ON THE RENT AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3610-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN SDBM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO SDBM-RENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDBM-PREFERRED-FLAG = 'Y'
                   COMPUTE SDBM-RENT-AMT ROUNDED = SDBT-RENT-AMT *
                       SDBM-DISCOUNT / 200
               WHEN SDBM-VAULT-CD = 'BU'
                   MOVE 12.00 TO SDBM-RENT-AMT
               WHEN SDBM-VAULT-CD = 'ET'
                   MOVE 3.00 TO SDBM-RENT-AMT
               WHEN OTHER
                   COMPUTE SDBM-RENT-AMT ROUNDED = SDBT-RENT-AMT *
                       SDBM-DISCOUNT / 100
           END-EVALUATE
           IF SDBM-RENT-AMT > 0 AND SDBM-RENT-AMT < 2.00
               MOVE 2.00 TO SDBM-RENT-AMT
           END-IF
           IF SDBM-RENT-AMT > 100.00
               MOVE 100.00 TO SDBM-RENT-AMT
           END-IF
           ADD SDBM-RENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * WORK AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3620-FLAG-ITEM.
           IF WS-WORK-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 90 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3630-CHECK-MATURITY.
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
           IF WS-DAYS > 90
               MOVE 'Y' TO SDBM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * KEY FEE AMT IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3640-LIMIT-KEY-FEE.
           IF SDBM-KEY-FEE-AMT < 25.00
               MOVE 25.00 TO SDBM-KEY-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SDBM-KEY-FEE-AMT > 500.00
               MOVE 500.00 TO SDBM-KEY-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE KEY FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3650-PRORATE-KEY-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBM-KEY-FEE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * EXS -> FJW WHEN OPEN DATE IS PAST
      * FJW -> YFB WHEN OPEN DATE IS PAST
      * YFB -> EXS WHEN OPEN DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3660-SET-BRANCH.
           EVALUATE SDBM-BRANCH-CD
               WHEN 'EXS'
                   IF SDBM-OPEN-DATE > 0 AND SDBM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FJW' TO SDBM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FJW'
                   IF SDBM-OPEN-DATE > 0 AND SDBM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YFB' TO SDBM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YFB'
                   IF SDBM-OPEN-DATE > 0 AND SDBM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EXS' TO SDBM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3670-ACCRUE-DISCOUNT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * SDBM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO SDBM-LATE-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DEPOSIT SCHED TIER FOR THE DEPOSIT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3680-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DEPOSIT-SCHED-RATE(1) TO SDBM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF SDBM-DEPOSIT-AMT NOT < WS-DEPOSIT-SCHED-LOW(WS-TX)
                   MOVE WS-DEPOSIT-SCHED-RATE(WS-TX) TO SDBM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3690-DESCRIBE-CHANNEL.
           EVALUATE SDBT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E650 NAME REQUIRED
      * E481 KEY FEE AMT NEGATIVE
      * E849 KEY FEE AMT OVER 250000
      * E003 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3700-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDBM-NAME = SPACES
                   MOVE 'E650' TO WS-REJECT-CODE
               WHEN SDBM-KEY-FEE-AMT < 0
                   MOVE 'E481' TO WS-REJECT-CODE
               WHEN SDBM-KEY-FEE-AMT > 250000.00
                   MOVE 'E849' TO WS-REJECT-CODE
               WHEN SDBM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E003' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT KEY FEE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3710-SPLIT-KEY-FEE.
           COMPUTE WS-WORK-AMT = SDBT-KEY-FEE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = SDBT-KEY-FEE-AMT - (WS-WORK-AMT * 4)
           COMPUTE SDBM-PREPAID-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT LATE FEE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3720-SPLIT-LATE-FEE.
           COMPUTE WS-WORK-AMT = SDBM-LATE-FEE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = SDBM-LATE-FEE-AMT - (WS-WORK-AMT * 2)
           COMPUTE SDBM-RENT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3730-AGE-LAST-STMT.
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
                   ADD SDBM-KEY-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD SDBM-KEY-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD SDBM-KEY-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD SDBM-KEY-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDBM-KEY-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3740-AGE-CLOSE.
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
                   ADD SDBM-PREPAID-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD SDBM-PREPAID-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD SDBM-PREPAID-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD SDBM-PREPAID-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDBM-PREPAID-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT KEY FEE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3750-SPLIT-KEY-FEE.
           COMPUTE WS-WORK-AMT = SDBM-KEY-FEE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = SDBM-KEY-FEE-AMT - (WS-WORK-AMT * 3)
           COMPUTE SDBM-LATE-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3760-DESCRIBE-REASON.
           EVALUATE SDBM-REASON-CD
               WHEN 'J'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'K'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'L'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'M'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'S'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'W'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'Y'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3770-AGE-NEXT-DUE.
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
                   ADD SDBM-DRILL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD SDBM-DRILL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD SDBM-DRILL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD SDBM-DRILL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDBM-DRILL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE KEY FEE AMT BY VAULT CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3780-ACCUM-VAULT.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDBM-VAULT-CD
                   ADD SDBT-KEY-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDBM-VAULT-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDBT-KEY-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PREPAID AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DEPOSIT AMT.
      *----------------------------------------------------------------*
       3790-PRORATE-DEPOSIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDBM-DEPOSIT-AMT ROUNDED =
                   SDBM-PREPAID-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SDBM-DEPOSIT-AMT
           END-IF
           ADD SDBM-DEPOSIT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3800-COUNT-ITEM.
           IF SDBM-ITEM-CNT < 9999999
               ADD 1 TO SDBM-ITEM-CNT
           END-IF
           IF SDBM-ITEM-CNT NOT < 6
               MOVE 'Y' TO SDBT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * J -> K WHEN OPEN DATE IS PAST
      * K -> L WHEN OPEN DATE IS PAST
      * L -> M WHEN OPEN DATE IS PAST
      * M -> S WHEN OPEN DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3810-SET-REASON.
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
      * DAYS SINCE LAST MAINT DATE. MORE THAN 15 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3820-CHECK-LAST-MAINT.
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
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E051 OPERATOR REQUIRED
      * E804 RENT YTD NEGATIVE
      * E080 RENT YTD OVER 10000
      * E601 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3830-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDBT-OPERATOR = SPACES
                   MOVE 'E051' TO WS-REJECT-CODE
               WHEN SDBM-RENT-YTD < 0
                   MOVE 'E804' TO WS-REJECT-CODE
               WHEN SDBM-RENT-YTD > 10000.00
                   MOVE 'E080' TO WS-REJECT-CODE
               WHEN SDBM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E601' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DEPOSIT AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3840-SPLIT-DEPOSIT.
           COMPUTE WS-WORK-AMT = SDBM-DEPOSIT-AMT / 12
           COMPUTE WS-WORK-AMT-2 = SDBM-DEPOSIT-AMT - (WS-WORK-AMT * 12)
           COMPUTE SDBM-RENT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DEPOSIT YTD IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3850-LIMIT-DEPOSIT.
           IF SDBM-DEPOSIT-YTD < 0.00
               MOVE 0.00 TO SDBM-DEPOSIT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SDBM-DEPOSIT-YTD > 10000.00
               MOVE 10000.00 TO SDBM-DEPOSIT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE RENT AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3860-ACCUM-SOURCE.
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
      * LATE FEE YTD IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3870-LIMIT-LATE-FEE.
           IF SDBM-LATE-FEE-YTD < 0.00
               MOVE 0.00 TO SDBM-LATE-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SDBM-LATE-FEE-YTD > 1000.00
               MOVE 1000.00 TO SDBM-LATE-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RENT AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3880-FLAG-RENT.
           IF SDBM-RENT-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE SDBM-RENT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE LATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO DRILL AMT.
      *----------------------------------------------------------------*
       3890-PRORATE-DRILL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDBM-DRILL-AMT ROUNDED =
                   SDBM-LATE-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SDBM-DRILL-AMT
           END-IF
           ADD SDBM-DRILL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 180 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3900-CHECK-POST.
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
           IF WS-DAYS > 180
               MOVE 'Y' TO SDBM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RENT AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3910-FLAG-RENT.
           IF SDBM-RENT-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE SDBM-RENT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * RENT YTD IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3920-LIMIT-RENT.
           IF SDBM-RENT-YTD < 50.00
               MOVE 50.00 TO SDBM-RENT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SDBM-RENT-YTD > 5000.00
               MOVE 5000.00 TO SDBM-RENT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT LATE FEE YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3930-SPLIT-LATE-FEE.
           COMPUTE WS-WORK-AMT = SDBM-LATE-FEE-YTD / 12
           COMPUTE WS-WORK-AMT-2 = SDBM-LATE-FEE-YTD - (WS-WORK-AMT *
               12)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E996 NAME REQUIRED
      * E771 KEY FEE AMT NEGATIVE
      * E333 KEY FEE AMT OVER 5000
      * E644 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3940-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDBM-NAME = SPACES
                   MOVE 'E996' TO WS-REJECT-CODE
               WHEN SDBT-KEY-FEE-AMT < 0
                   MOVE 'E771' TO WS-REJECT-CODE
               WHEN SDBT-KEY-FEE-AMT > 5000.00
                   MOVE 'E333' TO WS-REJECT-CODE
               WHEN SDBM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E644' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PREPAID YTD BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3950-ACCUM-CYCLE.
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
      * DAILY INTEREST ON KEY FEE YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3960-ACCRUE-KEY-FEE.
           IF SDBM-KEY-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBM-KEY-FEE-YTD * SDBM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO SDBM-DRILL-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DRILL AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3970-ACCRUE-DRILL.
           IF SDBM-DRILL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBM-DRILL-AMT * SDBM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO SDBT-DRILL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DEPOSIT SCHED TIER FOR THE RENT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3980-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DEPOSIT-SCHED-RATE(1) TO SDBM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF SDBM-RENT-YTD NOT < WS-DEPOSIT-SCHED-LOW(WS-TX)
                   MOVE WS-DEPOSIT-SCHED-RATE(WS-TX) TO SDBM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3990-SPLIT-ITEM.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 12)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DEPOSIT SCHED TIER FOR THE DEPOSIT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4000-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-DEPOSIT-SCHED-RATE(1) TO SDBM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF SDBM-DEPOSIT-AMT NOT < WS-DEPOSIT-SCHED-LOW(WS-TX)
                   MOVE WS-DEPOSIT-SCHED-RATE(WS-TX) TO SDBM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DEPOSIT SCHED TIER FOR THE DRILL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4010-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-DEPOSIT-SCHED-RATE(1) TO SDBM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF SDBT-DRILL-AMT NOT < WS-DEPOSIT-SCHED-LOW(WS-TX)
                   MOVE WS-DEPOSIT-SCHED-RATE(WS-TX) TO SDBM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF SDBM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO SDBM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT RENT YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4020-SPLIT-RENT.
           COMPUTE WS-WORK-AMT = SDBM-RENT-YTD / 2
           COMPUTE WS-WORK-AMT-2 = SDBM-RENT-YTD - (WS-WORK-AMT * 2)
           COMPUTE SDBM-PREPAID-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4030-AGE-EFF.
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
                   ADD SDBM-DEPOSIT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD SDBM-DEPOSIT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD SDBM-DEPOSIT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD SDBM-DEPOSIT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDBM-DEPOSIT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4040-AGE-LAST-STMT.
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
                   ADD SDBM-DEPOSIT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD SDBM-DEPOSIT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD SDBM-DEPOSIT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD SDBM-DEPOSIT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDBM-DEPOSIT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * J -> K WHEN NEXT DUE DATE IS PAST
      * K -> L WHEN NEXT DUE DATE IS PAST
      * L -> M WHEN NEXT DUE DATE IS PAST
      * M -> S WHEN NEXT DUE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4050-SET-REASON.
           EVALUATE SDBM-REASON-CD
               WHEN 'J'
                   IF SDBM-NEXT-DUE-DATE > 0 AND SDBM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'K' TO SDBM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF SDBM-NEXT-DUE-DATE > 0 AND SDBM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO SDBM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF SDBM-NEXT-DUE-DATE > 0 AND SDBM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO SDBM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF SDBM-NEXT-DUE-DATE > 0 AND SDBM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO SDBM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4060-ACCRUE-DISCOUNT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * SDBM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO SDBM-LATE-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4070-DESCRIBE-REASON.
           EVALUATE SDBM-REASON-CD
               WHEN 'J'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'K'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'L'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'M'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'S'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'W'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'Y'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT RENT AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4080-SPLIT-RENT.
           COMPUTE WS-WORK-AMT = SDBT-RENT-AMT / 2
           COMPUTE WS-WORK-AMT-2 = SDBT-RENT-AMT - (WS-WORK-AMT * 2)
           COMPUTE SDBM-DEPOSIT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE KEY FEE AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4090-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDBT-TRAN-CD
                   ADD SDBT-KEY-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDBT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDBT-KEY-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DEPOSIT FEE ON THE RENT YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       4100-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN SDBM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO SDBM-DEPOSIT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDBM-PREFERRED-FLAG = 'Y'
                   COMPUTE SDBM-DEPOSIT-AMT ROUNDED = SDBM-RENT-YTD *
                       SDBM-DISCOUNT / 200
               WHEN SDBM-BOX-SIZE-CD = 'CU'
                   MOVE 7.00 TO SDBM-DEPOSIT-AMT
               WHEN SDBM-BOX-SIZE-CD = 'MG'
                   MOVE 15.00 TO SDBM-DEPOSIT-AMT
               WHEN OTHER
                   COMPUTE SDBM-DEPOSIT-AMT ROUNDED = SDBM-RENT-YTD *
                       SDBM-DISCOUNT / 100
           END-EVALUATE
           IF SDBM-DEPOSIT-AMT > 0 AND SDBM-DEPOSIT-AMT < 1.00
               MOVE 1.00 TO SDBM-DEPOSIT-AMT
           END-IF
           IF SDBM-DEPOSIT-AMT > 150.00
               MOVE 150.00 TO SDBM-DEPOSIT-AMT
           END-IF
           ADD SDBM-DEPOSIT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4110-LIMIT-ITEM.
           IF WS-WORK-AMT < 25.00
               MOVE 25.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 1000.00
               MOVE 1000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4120-COUNT-RETRY.
           IF SDBM-RETRY-CNT < 9999999
               ADD 1 TO SDBM-RETRY-CNT
           END-IF
           IF SDBM-RETRY-CNT NOT < 6
               MOVE 'Y' TO SDBM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DRILL AMT IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4130-LIMIT-DRILL.
           IF SDBM-DRILL-AMT < 0.00
               MOVE 0.00 TO SDBM-DRILL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SDBM-DRILL-AMT > 10000.00
               MOVE 10000.00 TO SDBM-DRILL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       4140-AGE-LAST-MAINT.
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
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE DRILL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO RENT AMT.
      *----------------------------------------------------------------*
       4150-PRORATE-RENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDBM-RENT-AMT ROUNDED =
                   SDBT-DRILL-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO SDBM-RENT-AMT
           END-IF
           ADD SDBM-RENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PREPAID YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4160-FLAG-PREPAID.
           IF SDBM-PREPAID-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE SDBM-PREPAID-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PREPAID AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4170-ACCRUE-PREPAID.
           IF SDBM-PREPAID-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBM-PREPAID-AMT * SDBM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO SDBM-DEPOSIT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       4180-COUNT-RETRY.
           IF SDBM-RETRY-CNT < 9999999
               ADD 1 TO SDBM-RETRY-CNT
           END-IF
           IF SDBM-RETRY-CNT NOT < 6
               MOVE 'Y' TO SDBM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4190-AGE-LAST-STMT.
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
                   ADD SDBM-LATE-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD SDBM-LATE-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD SDBM-LATE-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD SDBM-LATE-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD SDBM-LATE-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT PREPAID AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4200-SPLIT-PREPAID.
           COMPUTE WS-WORK-AMT = SDBM-PREPAID-AMT / 3
           COMPUTE WS-WORK-AMT-2 = SDBM-PREPAID-AMT - (WS-WORK-AMT * 3)
           COMPUTE SDBT-DEPOSIT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DEPOSIT AMT IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4210-LIMIT-DEPOSIT.
           IF SDBT-DEPOSIT-AMT < 10.00
               MOVE 10.00 TO SDBT-DEPOSIT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SDBT-DEPOSIT-AMT > 1000.00
               MOVE 1000.00 TO SDBT-DEPOSIT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4220-DESCRIBE-BRANCH.
           EVALUATE SDBM-BRANCH-CD
               WHEN 'EXS'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'FJW'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'YFB'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DRILL AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4230-ACCRUE-DRILL.
           IF SDBT-DRILL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBT-DRILL-AMT * SDBM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO SDBM-DEPOSIT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 45 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4240-CHECK-EFF.
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
               MOVE 'Y' TO SDBM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4250-COUNT-RETRY.
           IF SDBM-RETRY-CNT < 9999999
               ADD 1 TO SDBM-RETRY-CNT
           END-IF
           IF SDBM-RETRY-CNT NOT < 10
               MOVE 'Y' TO SDBM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4260-DESCRIBE-BRANCH.
           EVALUATE SDBM-BRANCH-CD
               WHEN 'EXS'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'FJW'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'YFB'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT PREPAID AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4270-SPLIT-PREPAID.
           COMPUTE WS-WORK-AMT = SDBM-PREPAID-AMT / 12
           COMPUTE WS-WORK-AMT-2 = SDBM-PREPAID-AMT - (WS-WORK-AMT * 12)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DRILL AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4280-FLAG-DRILL.
           IF SDBM-DRILL-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE SDBM-DRILL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE KEY FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO KEY FEE AMT.
      *----------------------------------------------------------------*
       4290-PRORATE-KEY-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDBM-KEY-FEE-AMT ROUNDED =
                   SDBT-KEY-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SDBM-KEY-FEE-AMT
           END-IF
           ADD SDBM-KEY-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DEPOSIT YTD IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4300-LIMIT-DEPOSIT.
           IF SDBM-DEPOSIT-YTD < 25.00
               MOVE 25.00 TO SDBM-DEPOSIT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF SDBM-DEPOSIT-YTD > 10000.00
               MOVE 10000.00 TO SDBM-DEPOSIT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DRILL FEE ON THE WORK AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4310-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN SDBM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO SDBM-DRILL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDBM-PREFERRED-FLAG = 'Y'
                   COMPUTE SDBM-DRILL-AMT ROUNDED = WS-WORK-AMT *
                       SDBM-DISCOUNT / 200
               WHEN SDBM-VAULT-CD = 'BU'
                   MOVE 7.00 TO SDBM-DRILL-AMT
               WHEN SDBM-VAULT-CD = 'ET'
                   MOVE 12.00 TO SDBM-DRILL-AMT
               WHEN OTHER
                   COMPUTE SDBM-DRILL-AMT ROUNDED = WS-WORK-AMT *
                       SDBM-DISCOUNT / 100
           END-EVALUATE
           IF SDBM-DRILL-AMT > 0 AND SDBM-DRILL-AMT < 5.00
               MOVE 5.00 TO SDBM-DRILL-AMT
           END-IF
           IF SDBM-DRILL-AMT > 35.00
               MOVE 35.00 TO SDBM-DRILL-AMT
           END-IF
           ADD SDBM-DRILL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4320-DESCRIBE-CYCLE.
           EVALUATE SDBM-CYCLE-CD
               WHEN 'AP'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'DA'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'HV'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'SE'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E114 CONTACT REQUIRED
      * E367 PREPAID AMT NEGATIVE
      * E411 PREPAID AMT OVER 10000
      * E241 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       4330-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDBM-CONTACT = SPACES
                   MOVE 'E114' TO WS-REJECT-CODE
               WHEN SDBM-PREPAID-AMT < 0
                   MOVE 'E367' TO WS-REJECT-CODE
               WHEN SDBM-PREPAID-AMT > 10000.00
                   MOVE 'E411' TO WS-REJECT-CODE
               WHEN SDBM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E241' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE LATE FEE YTD BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4340-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = SDBT-TRAN-CD
                   ADD SDBM-LATE-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE SDBT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE SDBM-LATE-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT LATE FEE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4350-SPLIT-LATE-FEE.
           COMPUTE WS-WORK-AMT = SDBM-LATE-FEE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = SDBM-LATE-FEE-AMT - (WS-WORK-AMT * 3)
           COMPUTE SDBM-RENT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON KEY FEE AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4360-ACCRUE-KEY-FEE.
           IF SDBM-KEY-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   SDBM-KEY-FEE-AMT * SDBM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO SDBM-RENT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E588 CONTACT REQUIRED
      * E260 DEPOSIT AMT NEGATIVE
      * E912 DEPOSIT AMT OVER 10000
      * E458 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       4370-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN SDBM-CONTACT = SPACES
                   MOVE 'E588' TO WS-REJECT-CODE
               WHEN SDBT-DEPOSIT-AMT < 0
                   MOVE 'E260' TO WS-REJECT-CODE
               WHEN SDBT-DEPOSIT-AMT > 10000.00
                   MOVE 'E912' TO WS-REJECT-CODE
               WHEN SDBM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E458' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE RENT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO DRILL AMT.
      *----------------------------------------------------------------*
       4380-PRORATE-DRILL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE SDBT-DRILL-AMT ROUNDED =
                   SDBM-RENT-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO SDBT-DRILL-AMT
           END-IF
           ADD SDBT-DRILL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE RENT FEE ON THE WORK AMT 2.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       4390-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN SDBM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO SDBT-RENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN SDBM-REVIEW-FLAG = 'Y'
                   COMPUTE SDBT-RENT-AMT ROUNDED = WS-WORK-AMT-2 *
                       SDBM-FEE-PCT / 200
               WHEN SDBM-CYCLE-CD = 'AP'
                   MOVE 7.00 TO SDBT-RENT-AMT
               WHEN SDBM-CYCLE-CD = 'DA'
                   MOVE 12.00 TO SDBT-RENT-AMT
               WHEN OTHER
                   COMPUTE SDBT-RENT-AMT ROUNDED = WS-WORK-AMT-2 *
                       SDBM-FEE-PCT / 100
           END-EVALUATE
           IF SDBT-RENT-AMT > 0 AND SDBT-RENT-AMT < 2.00
               MOVE 2.00 TO SDBT-RENT-AMT
           END-IF
           IF SDBT-RENT-AMT > 250.00
               MOVE 250.00 TO SDBT-RENT-AMT
           END-IF
           ADD SDBT-RENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * RENT AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4400-FLAG-RENT.
           IF SDBT-RENT-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE SDBT-RENT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE SDB FIELDS TO SDVAL AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7000-CALL-SDVAL.
           INITIALIZE SDVAL-PARMS
           MOVE SDBM-DRILL-YTD
             TO SDVAL-DRILL-YTD
           MOVE SDBM-KEY-FEE-YTD
             TO SDVAL-KEY-FEE-YTD
           MOVE SDBM-DEPOSIT-AMT
             TO SDVAL-DEPOSIT-AMT
           MOVE SDBM-RENT-AMT
             TO SDVAL-RENT-AMT
           MOVE SDBM-RENT-YTD
             TO SDVAL-RENT-YTD
           MOVE SDBM-DRILL-AMT
             TO SDVAL-DRILL-AMT
           MOVE SDBM-NEXT-DUE-DATE
             TO SDVAL-NEXT-DUE-DATE
           MOVE SDBM-LAST-MAINT-DATE
             TO SDVAL-LAST-MAINT-DATE
           MOVE SDBM-OPEN-DATE
             TO SDVAL-OPEN-DATE
           MOVE SDBM-LAST-STMT-DATE
             TO SDVAL-LAST-STMT-DATE
           MOVE SDBM-CYCLE-CD
             TO SDVAL-CYCLE-CD
           MOVE SDBM-BOX-SIZE-CD
             TO SDVAL-BOX-SIZE-CD
           MOVE SDBM-REASON-CD
             TO SDVAL-REASON-CD
           MOVE SDBM-BRANCH-CD
             TO SDVAL-BRANCH-CD
           MOVE SDBM-OVERRIDE-FLAG
             TO SDVAL-OVERRIDE-FLAG
           MOVE SDBM-AUTO-RENEW-FLAG
             TO SDVAL-AUTO-RENEW-FLAG
           MOVE SDBM-REVIEW-FLAG
             TO SDVAL-REVIEW-FLAG
           MOVE SDBM-NOTICE-CNT
             TO SDVAL-NOTICE-CNT
           MOVE SDBM-RETRY-CNT
             TO SDVAL-RETRY-CNT
           MOVE SDBM-PENALTY-PCT
             TO SDVAL-PENALTY-PCT
           MOVE SDBM-DISCOUNT
             TO SDVAL-DISCOUNT
           MOVE SDBM-NAME
             TO SDVAL-NAME
           MOVE SDBM-CONTACT
             TO SDVAL-CONTACT
           CALL 'SDVAL' USING SDVAL-PARMS
           EVALUATE SDVAL-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE SDVAL-RENT-AMT
                     TO SDBM-RENT-AMT
                   MOVE SDVAL-OVERRIDE-FLAG
                     TO SDBM-OVERRIDE-FLAG
                   MOVE SDVAL-REVIEW-FLAG
                     TO SDBM-REVIEW-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RVAL' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-SDVAL' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE SDB FIELDS TO SDFEE AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7030-CALL-SDFEE.
           INITIALIZE SDFEE-PARMS
           MOVE SDBM-PREPAID-AMT
             TO SDFEE-PREPAID-AMT
           MOVE SDBM-PREPAID-YTD
             TO SDFEE-PREPAID-YTD
           MOVE SDBM-KEY-FEE-AMT
             TO SDFEE-KEY-FEE-AMT
           MOVE SDBM-DEPOSIT-YTD
             TO SDFEE-DEPOSIT-YTD
           MOVE SDBM-RENT-YTD
             TO SDFEE-RENT-YTD
           MOVE SDBM-DRILL-YTD
             TO SDFEE-DRILL-YTD
           MOVE SDBM-LAST-MAINT-DATE
             TO SDFEE-LAST-MAINT-DATE
           MOVE SDBM-LAST-ACT-DATE
             TO SDFEE-LAST-ACT-DATE
           MOVE SDBM-MATURITY-DATE
             TO SDFEE-MATURITY-DATE
           MOVE SDBM-OPEN-DATE
             TO SDFEE-OPEN-DATE
           MOVE SDBM-SOURCE-CD
             TO SDFEE-SOURCE-CD
           MOVE SDBM-BRANCH-CD
             TO SDFEE-BRANCH-CD
           MOVE SDBM-BOX-SIZE-CD
             TO SDFEE-BOX-SIZE-CD
           MOVE SDBM-VAULT-CD
             TO SDFEE-VAULT-CD
           MOVE SDBM-SUSPEND-FLAG
             TO SDFEE-SUSPEND-FLAG
           MOVE SDBM-AUTO-RENEW-FLAG
             TO SDFEE-AUTO-RENEW-FLAG
           MOVE SDBM-EXEMPT-FLAG
             TO SDFEE-EXEMPT-FLAG
           MOVE SDBM-RETRY-CNT
             TO SDFEE-RETRY-CNT
           MOVE SDBM-ITEM-CNT
             TO SDFEE-ITEM-CNT
           MOVE SDBM-DISCOUNT
             TO SDFEE-DISCOUNT
           MOVE SDBM-PENALTY-PCT
             TO SDFEE-PENALTY-PCT
           MOVE SDBM-CONTACT
             TO SDFEE-CONTACT
           MOVE SDBM-CUST-ID
             TO SDFEE-CUST-ID
           CALL 'SDFEE' USING SDFEE-PARMS
           EVALUATE SDFEE-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE SDFEE-PREPAID-AMT
                     TO SDBM-PREPAID-AMT
                   MOVE SDFEE-KEY-FEE-AMT
                     TO SDBM-KEY-FEE-AMT
                   MOVE SDFEE-DEPOSIT-YTD
                     TO SDBM-DEPOSIT-YTD
                   CONTINUE
               WHEN 08
                   MOVE 'RFEE' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7030-CALL-SDFEE' TO ABND-PARAGRAPH
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
                   MOVE SDLIM-RENT-YTD
                     TO SDBM-RENT-YTD
                   MOVE SDLIM-LATE-FEE-YTD
                     TO SDBM-LATE-FEE-YTD
                   MOVE SDLIM-LATE-FEE-AMT
                     TO SDBM-LATE-FEE-AMT
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
           DISPLAY 'SDBRCON READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'SDBRCON' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
