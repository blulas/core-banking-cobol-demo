      *================================================================*
      * PROGRAM  : CLLAGE                                              *
      * TYPE     : BATCH                                               *
      * PURPOSE  : COLLECTIONS QUEUE - AGING. READS THE COL            *
      *            TRANSACTIONS, UPDATES THE COL MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. DELINQUENT ACCOUNT COLLECTION   *
      *            QUEUES, PROMISES AND LETTERS                        *
      * INPUT    : COLMST (KSDS), COLTRN, BANKCTL                      *
      * OUTPUT   : COLOUT, CLLAGRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, CLLIM                   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-10-26 PQR  ORIGINAL                                       *
      * 1996-10-19 LMB  COLLECTED FEE SCHEDULE UPDATE CHG5533          *
      * 1997-03-18 DPS  WRITEOFF CUTOFF MOVED CHG6849                  *
      * 1997-01-07 CRP  WRITEOFF NOTICE TEXT PER LEGAL REQ4583         *
      * 1997-09-18 GAF  PERFORMANCE - COLLECTED READ AHEAD REQ3582     *
      * 1998-09-13 DPS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-05-05 GAF  PERFORMANCE - PROMISED READ AHEAD PRB7001      *
      * 2001-04-23 GAF  AUDIT FINDING - PROMISED CONTROL TOTALS REQ9978*
      * 2002-10-21 JLM  WRITEOFF EXCEPTIONS TO OPS QUEUE REQ7211       *
      * 2003-07-04 DPS  AUDIT FINDING - WRITEOFF CONTROL TOTALS PRB5900*
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLLAGE.
       AUTHOR. R J TRAVERS.

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
           SELECT REPORT-FILE ASSIGN TO "CLLAGRPT"
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
      * WS-PAST-DUE-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PAST-DUE-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000101500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000745500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011000000000650000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021000000000479000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021100000000683000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00031100000000602000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00031200000000791500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00031300000000249500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00033800000000780500'.
       01  WS-PAST-DUE-TIER-TBL REDEFINES WS-PAST-DUE-TIER-VALUES.
           05  WS-PAST-DUE-TIER-ROW OCCURS 9 TIMES.
               10  WS-PAST-DUE-TIER-LOW    PIC 9(09)V99.
               10  WS-PAST-DUE-TIER-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-FEE-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FEE-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000181000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000099000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000600000000642000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005600000000458500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008100000000114000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00009100000000434000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014100000000181500'.
       01  WS-FEE-BAND-TBL REDEFINES WS-FEE-BAND-VALUES.
           05  WS-FEE-BAND-ROW OCCURS 7 TIMES.
               10  WS-FEE-BAND-LOW         PIC 9(09)V99.
               10  WS-FEE-BAND-RATE        PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-FEE-BANDX - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FEE-BANDX-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000749500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000244000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005500000000352000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008000000000198500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008100000000301000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010600000000764000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015600000000077500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016600000000669500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016850000000481500'.
       01  WS-FEE-BANDX-TBL REDEFINES WS-FEE-BANDX-VALUES.
           05  WS-FEE-BANDX-ROW OCCURS 9 TIMES.
               10  WS-FEE-BANDX-LOW        PIC 9(09)V99.
               10  WS-FEE-BANDX-RATE       PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  CLLAGE  COLLECTIONS QUEUE'.
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
               PERFORM 3000-EDIT-REF-NO
               PERFORM 3010-FLAG-COLLECTED
               PERFORM 3020-PRORATE-PAST-DUE
               PERFORM 3030-CHECK-LAST-PAY
               PERFORM 3040-ACCRUE-WRITEOFF
               PERFORM 3050-SET-ACTION
               PERFORM 3060-ACCRUE-WRITEOFF
               PERFORM 3070-DESCRIBE-CHANNEL
               PERFORM 3080-ACCRUE-COLLECTED
               PERFORM 3090-SET-CHANNEL
               PERFORM 3100-SET-ACTION
               PERFORM 3110-COUNT-RETRY
               PERFORM 3120-SET-REASON
               PERFORM 3130-SET-REASON
               PERFORM 3140-ACCRUE-PAST-DUE
               PERFORM 3150-CHECK-EFFECTIVE
               PERFORM 3160-ACCRUE-WRITEOFF
               PERFORM 3170-ACCUM-ACTION
               PERFORM 3180-ASSESS-NOTICE-SENT
               PERFORM 3190-DESCRIBE-REASON
               PERFORM 3200-SPLIT-WRITEOFF
               PERFORM 3210-LIMIT-ITEM
               PERFORM 3220-AGE-EXPIRY
               PERFORM 3230-COUNT-SEQ
               PERFORM 3240-COUNT-NOTICE
               PERFORM 3250-CHECK-LAST-MAINT
               PERFORM 3260-DESCRIBE-CHANNEL
               PERFORM 3270-ACCRUE-PROMISED
               PERFORM 3280-COUNT-NOTICE
               PERFORM 3290-COUNT-SEQ
               PERFORM 3300-FLAG-WRITEOFF
               PERFORM 3310-DESCRIBE-QUEUE
               PERFORM 3320-SELECT-MARGIN
               PERFORM 3330-ACCRUE-PAST-DUE
               PERFORM 3340-ASSESS-OVERRIDE
               PERFORM 3350-ACCUM-REASON
               PERFORM 3360-SELECT-MARGIN
               PERFORM 3370-LIMIT-COLLECTED
               PERFORM 3380-SET-TRAN
               PERFORM 3390-ACCRUE-COLLECTED
               PERFORM 3400-ACCRUE-MARGIN
               PERFORM 3410-ACCRUE-WRITEOFF
               PERFORM 3420-COUNT-RETRY
               PERFORM 3430-SET-CYCLE
               PERFORM 3440-PRORATE-PAST-DUE
               PERFORM 3450-SET-ACTION
               PERFORM 3460-ACCUM-QUEUE
               PERFORM 3470-ACCUM-TRAN
               PERFORM 3480-LIMIT-PROMISED
               PERFORM 3490-CHECK-NEXT-DUE
               PERFORM 3500-ACCRUE-COLLECTED
               PERFORM 3510-LIMIT-ITEM
               PERFORM 3520-SELECT-MARGIN
               PERFORM 3530-DESCRIBE-QUEUE
               PERFORM 3540-SELECT-FEE-PCT
               PERFORM 3550-PRORATE-COLLECTED
               PERFORM 3560-LIMIT-PAST-DUE
               PERFORM 3570-COUNT-CYCLE
               PERFORM 3580-ACCRUE-FEE
               PERFORM 3590-ACCRUE-PAST-DUE
               PERFORM 3600-SPLIT-WRITEOFF
               PERFORM 3610-AGE-EFFECTIVE
               PERFORM 3620-SET-CYCLE
               PERFORM 3630-EDIT-DESC
               PERFORM 3640-AGE-EFFECTIVE
               PERFORM 3650-FLAG-COLLECTED
               PERFORM 3660-PRORATE-FEE
               PERFORM 3670-CHECK-EFF
               PERFORM 3680-ACCRUE-FEE-PCT
               PERFORM 3690-AGE-LAST-MAINT
               PERFORM 3700-COUNT-RETRY
               PERFORM 3710-FLAG-WRITEOFF
               PERFORM 3720-DESCRIBE-STATUS
               PERFORM 3730-SELECT-PENALTY-PCT
               PERFORM 3740-SET-STATUS
               PERFORM 3750-EDIT-CUST-ID
               PERFORM 3760-ACCRUE-WRITEOFF
               PERFORM 3770-CHECK-EXPIRY
               PERFORM 3780-EDIT-NAME
               PERFORM 3790-LIMIT-PAST-DUE
               PERFORM 3800-ASSESS-PREFERRED
               PERFORM 3810-ACCRUE-COLLECTED
               PERFORM 3820-COUNT-CYCLE
               PERFORM 3830-PRORATE-FEE
               PERFORM 3840-AGE-LAST-PAY
               PERFORM 3850-EDIT-CONTACT
               PERFORM 3860-ASSESS-REVIEW
               PERFORM 3870-ACCUM-REGION
               PERFORM 3880-DESCRIBE-REGION
               PERFORM 3890-AGE-NEXT-REVIEW
               PERFORM 3900-PRORATE-COLLECTED
               PERFORM 3910-SELECT-PENALTY-PCT
               PERFORM 3920-SELECT-PENALTY-PCT
               PERFORM 3930-FLAG-COLLECTED
               PERFORM 3940-FLAG-COLLECTED
               PERFORM 3950-PRORATE-WRITEOFF
               PERFORM 3960-CHECK-LAST-MAINT
               PERFORM 3970-SET-CHANNEL
               PERFORM 3980-ASSESS-REVERSAL
               PERFORM 3990-CHECK-LAST-PAY
               PERFORM 4000-FLAG-FEE
               PERFORM 4010-ACCUM-STATUS
               PERFORM 4020-FLAG-FEE
               PERFORM 4030-AGE-NOTICE
               PERFORM 4040-CHECK-NEXT-REVIEW
               PERFORM 4050-PRORATE-FEE
               PERFORM 4060-ACCRUE-PAST-DUE
               PERFORM 4070-SET-QUEUE
               PERFORM 4080-PRORATE-PROMISED
               PERFORM 4090-ACCUM-QUEUE
               PERFORM 4100-SELECT-FEE-PCT
               PERFORM 4110-LIMIT-PROMISED
               PERFORM 4120-SPLIT-FEE
               PERFORM 4130-SPLIT-WRITEOFF
               PERFORM 4140-DESCRIBE-QUEUE
               PERFORM 4150-ACCRUE-FEE
               PERFORM 4160-EDIT-OPERATOR
               PERFORM 4170-SPLIT-PAST-DUE
               PERFORM 4180-CHECK-EXPIRY
               PERFORM 4190-FLAG-FEE
               PERFORM 4200-SET-CYCLE
               PERFORM 4210-COUNT-NOTICE
               PERFORM 4220-LIMIT-PAST-DUE
               PERFORM 4230-SELECT-MARGIN
               PERFORM 4240-SELECT-FEE-PCT
               PERFORM 4250-EDIT-NAME
               PERFORM 4260-EDIT-ADDR
               PERFORM 4270-SET-TRAN
               PERFORM 4280-ASSESS-REVIEW
               PERFORM 4290-ACCRUE-WRITEOFF
               PERFORM 4300-COUNT-RETRY
               PERFORM 4310-CHECK-EXPIRY
               PERFORM 4320-ACCRUE-PAST-DUE
               PERFORM 4330-CHECK-NEXT-REVIEW
               PERFORM 4340-EDIT-OPERATOR
               PERFORM 4350-AGE-NEXT-DUE
               PERFORM 4360-DESCRIBE-REASON
               PERFORM 4370-AGE-EXPIRY
               PERFORM 4380-SPLIT-WRITEOFF
               PERFORM 4390-AGE-OPEN
               PERFORM 4400-EDIT-REJECT
               PERFORM 4410-SPLIT-WRITEOFF
               PERFORM 4420-SET-CHANNEL
               PERFORM 4430-CHECK-EXPIRY
               PERFORM 4440-AGE-EFFECTIVE
               PERFORM 4450-SET-ACTION
               PERFORM 4460-PRORATE-PROMISED
               PERFORM 4470-EDIT-ADDR
               PERFORM 4480-CHECK-LAST-PAY
               PERFORM 4490-ASSESS-REVERSAL
               PERFORM 4500-AGE-NEXT-REVIEW
               PERFORM 4510-ACCUM-CHANNEL
               PERFORM 4520-SET-QUEUE
               PERFORM 4530-SET-STATUS
               PERFORM 4540-FLAG-PAST-DUE
               PERFORM 4550-SELECT-MARGIN
               PERFORM 4560-FLAG-COLLECTED
               PERFORM 4570-SELECT-PENALTY-PCT
               PERFORM 4580-SPLIT-WRITEOFF
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
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E518 REF NO REQUIRED
      * E459 COLLECTED AMT NEGATIVE
      * E374 COLLECTED AMT OVER 25000
      * E022 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3000-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN COLM-REF-NO = SPACES
                   MOVE 'E518' TO WS-REJECT-CODE
               WHEN COLM-COLLECTED-AMT < 0
                   MOVE 'E459' TO WS-REJECT-CODE
               WHEN COLM-COLLECTED-AMT > 25000.00
                   MOVE 'E374' TO WS-REJECT-CODE
               WHEN COLT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E022' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * COLLECTED YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3010-FLAG-COLLECTED.
           IF COLM-COLLECTED-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE COLM-COLLECTED-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAST DUE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PAST DUE YTD.
      *----------------------------------------------------------------*
       3020-PRORATE-PAST-DUE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE COLM-PAST-DUE-YTD ROUNDED =
                   COLT-PAST-DUE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO COLM-PAST-DUE-YTD
           END-IF
           ADD COLM-PAST-DUE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 365 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3030-CHECK-LAST-PAY.
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
           IF WS-DAYS > 365
               MOVE 'Y' TO COLM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WRITEOFF YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3040-ACCRUE-WRITEOFF.
           IF COLM-WRITEOFF-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLM-WRITEOFF-YTD * COLM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO COLT-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ACTION CD:
      * KJ -> TM WHEN OPEN DATE IS PAST
      * TM -> VL WHEN OPEN DATE IS PAST
      * VL -> ZS WHEN OPEN DATE IS PAST
      * ZS -> KJ WHEN OPEN DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3050-SET-ACTION.
           EVALUATE COLM-ACTION-CD
               WHEN 'KJ'
                   IF COLM-OPEN-DATE > 0 AND COLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TM' TO COLM-ACTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TM'
                   IF COLM-OPEN-DATE > 0 AND COLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VL' TO COLM-ACTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VL'
                   IF COLM-OPEN-DATE > 0 AND COLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZS' TO COLM-ACTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZS'
                   IF COLM-OPEN-DATE > 0 AND COLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KJ' TO COLM-ACTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WRITEOFF AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3060-ACCRUE-WRITEOFF.
           IF COLT-WRITEOFF-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLT-WRITEOFF-AMT * COLM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO COLM-PAST-DUE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3070-DESCRIBE-CHANNEL.
           EVALUATE COLT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON COLLECTED AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3080-ACCRUE-COLLECTED.
           IF COLM-COLLECTED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLM-COLLECTED-AMT * COLM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO COLM-PAST-DUE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN EXPIRY DATE IS PAST
      * BAT -> MAL WHEN EXPIRY DATE IS PAST
      * MAL -> TEL WHEN EXPIRY DATE IS PAST
      * TEL -> ONL WHEN EXPIRY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3090-SET-CHANNEL.
           EVALUATE COLT-CHANNEL-CD
               WHEN 'BRN'
                   IF COLM-EXPIRY-DATE > 0 AND COLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO COLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF COLM-EXPIRY-DATE > 0 AND COLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO COLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF COLM-EXPIRY-DATE > 0 AND COLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO COLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF COLM-EXPIRY-DATE > 0 AND COLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO COLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ACTION CD:
      * KJ -> TM WHEN LAST MAINT DATE IS PAST
      * TM -> VL WHEN LAST MAINT DATE IS PAST
      * VL -> ZS WHEN LAST MAINT DATE IS PAST
      * ZS -> KJ WHEN LAST MAINT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3100-SET-ACTION.
           EVALUATE COLM-ACTION-CD
               WHEN 'KJ'
                   IF COLM-LAST-MAINT-DATE > 0 AND COLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'TM' TO COLM-ACTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TM'
                   IF COLM-LAST-MAINT-DATE > 0 AND COLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'VL' TO COLM-ACTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VL'
                   IF COLM-LAST-MAINT-DATE > 0 AND COLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'ZS' TO COLM-ACTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZS'
                   IF COLM-LAST-MAINT-DATE > 0 AND COLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'KJ' TO COLM-ACTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3110-COUNT-RETRY.
           IF COLM-RETRY-CNT < 9999999
               ADD 1 TO COLM-RETRY-CNT
           END-IF
           IF COLM-RETRY-CNT NOT < 12
               MOVE 'Y' TO COLM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * AT -> BJ WHEN EFFECTIVE DATE IS PAST
      * BJ -> GG WHEN EFFECTIVE DATE IS PAST
      * GG -> ZR WHEN EFFECTIVE DATE IS PAST
      * ZR -> AT WHEN EFFECTIVE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3120-SET-REASON.
           EVALUATE COLM-REASON-CD
               WHEN 'AT'
                   IF COLM-EFFECTIVE-DATE > 0 AND COLM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BJ' TO COLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BJ'
                   IF COLM-EFFECTIVE-DATE > 0 AND COLM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GG' TO COLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GG'
                   IF COLM-EFFECTIVE-DATE > 0 AND COLM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZR' TO COLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZR'
                   IF COLM-EFFECTIVE-DATE > 0 AND COLM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AT' TO COLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * AT -> BJ WHEN NEXT DUE DATE IS PAST
      * BJ -> GG WHEN NEXT DUE DATE IS PAST
      * GG -> ZR WHEN NEXT DUE DATE IS PAST
      * ZR -> AT WHEN NEXT DUE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3130-SET-REASON.
           EVALUATE COLM-REASON-CD
               WHEN 'AT'
                   IF COLM-NEXT-DUE-DATE > 0 AND COLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BJ' TO COLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BJ'
                   IF COLM-NEXT-DUE-DATE > 0 AND COLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GG' TO COLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GG'
                   IF COLM-NEXT-DUE-DATE > 0 AND COLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZR' TO COLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZR'
                   IF COLM-NEXT-DUE-DATE > 0 AND COLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AT' TO COLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAST DUE YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3140-ACCRUE-PAST-DUE.
           IF COLM-PAST-DUE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLM-PAST-DUE-YTD * COLM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO COLM-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3150-CHECK-EFFECTIVE.
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
               MOVE 'Y' TO COLT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WRITEOFF AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3160-ACCRUE-WRITEOFF.
           IF COLT-WRITEOFF-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLT-WRITEOFF-AMT * COLM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO COLM-PROMISED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE YTD BY ACTION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3170-ACCUM-ACTION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = COLM-ACTION-CD
                   ADD COLM-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE COLM-ACTION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE COLM-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAST DUE FEE ON THE WORK AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3180-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN COLM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO COLM-PAST-DUE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLM-PREFERRED-FLAG = 'Y'
                   COMPUTE COLM-PAST-DUE-AMT ROUNDED = WS-WORK-AMT *
                       COLM-FEE-PCT / 200
               WHEN COLM-CYCLE-CD = 'BE'
                   MOVE 12.00 TO COLM-PAST-DUE-AMT
               WHEN COLM-CYCLE-CD = 'GV'
                   MOVE 20.00 TO COLM-PAST-DUE-AMT
               WHEN OTHER
                   COMPUTE COLM-PAST-DUE-AMT ROUNDED = WS-WORK-AMT *
                       COLM-FEE-PCT / 100
           END-EVALUATE
           IF COLM-PAST-DUE-AMT > 0 AND COLM-PAST-DUE-AMT < 5.00
               MOVE 5.00 TO COLM-PAST-DUE-AMT
           END-IF
           IF COLM-PAST-DUE-AMT > 250.00
               MOVE 250.00 TO COLM-PAST-DUE-AMT
           END-IF
           ADD COLM-PAST-DUE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3190-DESCRIBE-REASON.
           EVALUATE COLM-REASON-CD
               WHEN 'AT'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'BJ'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'GG'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ZR'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WRITEOFF YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3200-SPLIT-WRITEOFF.
           COMPUTE WS-WORK-AMT = COLM-WRITEOFF-YTD / 2
           COMPUTE WS-WORK-AMT-2 = COLM-WRITEOFF-YTD - (WS-WORK-AMT * 2)
           COMPUTE COLM-PAST-DUE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3210-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 25.00
               MOVE 25.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 5000.00
               MOVE 5000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3220-AGE-EXPIRY.
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
                   ADD COLM-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD COLM-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD COLM-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD COLM-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLM-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3230-COUNT-SEQ.
           IF COLT-SEQ < 9999999
               ADD 1 TO COLT-SEQ
           END-IF
           IF COLT-SEQ NOT < 3
               MOVE 'Y' TO COLM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3240-COUNT-NOTICE.
           IF COLM-NOTICE-CNT < 9999999
               ADD 1 TO COLM-NOTICE-CNT
           END-IF
           IF COLM-NOTICE-CNT NOT < 3
               MOVE 'Y' TO COLM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 60 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3250-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF COLM-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO COLM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3260-DESCRIBE-CHANNEL.
           EVALUATE COLT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PROMISED YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3270-ACCRUE-PROMISED.
           IF COLM-PROMISED-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLM-PROMISED-YTD * COLM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO COLT-COLLECTED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3280-COUNT-NOTICE.
           IF COLM-NOTICE-CNT < 9999999
               ADD 1 TO COLM-NOTICE-CNT
           END-IF
           IF COLM-NOTICE-CNT NOT < 3
               MOVE 'Y' TO COLT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3290-COUNT-SEQ.
           IF COLT-SEQ < 9999999
               ADD 1 TO COLT-SEQ
           END-IF
           IF COLT-SEQ NOT < 10
               MOVE 'Y' TO COLM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WRITEOFF AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3300-FLAG-WRITEOFF.
           IF COLT-WRITEOFF-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE COLT-WRITEOFF-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE QUEUE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3310-DESCRIBE-QUEUE.
           EVALUATE COLM-QUEUE-CD
               WHEN 'DL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'ER'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'HA'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'HW'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'ME'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'PX'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'ZY'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE FEE BANDX TIER FOR THE PAST DUE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3320-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-BANDX-RATE(1) TO COLM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF COLM-PAST-DUE-AMT NOT < WS-FEE-BANDX-LOW(WS-TX)
                   MOVE WS-FEE-BANDX-RATE(WS-TX) TO COLM-MARGIN
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
       3330-ACCRUE-PAST-DUE.
           IF COLM-PAST-DUE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLM-PAST-DUE-AMT * COLM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WRITEOFF YTD FEE ON THE FEE AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3340-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN COLM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO COLM-WRITEOFF-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE COLM-WRITEOFF-YTD ROUNDED = COLT-FEE-AMT *
                       COLM-MARGIN / 200
               WHEN COLM-ACTION-CD = 'KJ'
                   MOVE 7.00 TO COLM-WRITEOFF-YTD
               WHEN COLM-ACTION-CD = 'TM'
                   MOVE 20.00 TO COLM-WRITEOFF-YTD
               WHEN OTHER
                   COMPUTE COLM-WRITEOFF-YTD ROUNDED = COLT-FEE-AMT *
                       COLM-MARGIN / 100
           END-EVALUATE
           IF COLM-WRITEOFF-YTD > 0 AND COLM-WRITEOFF-YTD < 5.00
               MOVE 5.00 TO COLM-WRITEOFF-YTD
           END-IF
           IF COLM-WRITEOFF-YTD > 250.00
               MOVE 250.00 TO COLM-WRITEOFF-YTD
           END-IF
           ADD COLM-WRITEOFF-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE PROMISED YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3350-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = COLM-REASON-CD
                   ADD COLM-PROMISED-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE COLM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE COLM-PROMISED-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAST DUE TIER TIER FOR THE PAST DUE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3360-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PAST-DUE-TIER-RATE(1) TO COLM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF COLT-PAST-DUE-AMT NOT < WS-PAST-DUE-TIER-LOW(WS-TX)
                   MOVE WS-PAST-DUE-TIER-RATE(WS-TX) TO COLM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF COLM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO COLM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * COLLECTED AMT IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3370-LIMIT-COLLECTED.
           IF COLT-COLLECTED-AMT < 10.00
               MOVE 10.00 TO COLT-COLLECTED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF COLT-COLLECTED-AMT > 2500.00
               MOVE 2500.00 TO COLT-COLLECTED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * PR -> CL WHEN POST DATE IS PAST
      * CL -> LT WHEN POST DATE IS PAST
      * LT -> WO WHEN POST DATE IS PAST
      * WO -> RF WHEN POST DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3380-SET-TRAN.
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
      * DAILY INTEREST ON COLLECTED YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3390-ACCRUE-COLLECTED.
           IF COLM-COLLECTED-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLM-COLLECTED-YTD * COLM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3400-ACCRUE-MARGIN.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * COLM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO COLT-COLLECTED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WRITEOFF YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3410-ACCRUE-WRITEOFF.
           IF COLM-WRITEOFF-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLM-WRITEOFF-YTD * COLM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO COLM-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3420-COUNT-RETRY.
           IF COLM-RETRY-CNT < 9999999
               ADD 1 TO COLM-RETRY-CNT
           END-IF
           IF COLM-RETRY-CNT NOT < 6
               MOVE 'Y' TO COLT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * BE -> GV WHEN LAST MAINT DATE IS PAST
      * GV -> JM WHEN LAST MAINT DATE IS PAST
      * JM -> LC WHEN LAST MAINT DATE IS PAST
      * LC -> MK WHEN LAST MAINT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3430-SET-CYCLE.
           EVALUATE COLM-CYCLE-CD
               WHEN 'BE'
                   IF COLM-LAST-MAINT-DATE > 0 AND COLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'GV' TO COLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GV'
                   IF COLM-LAST-MAINT-DATE > 0 AND COLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'JM' TO COLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JM'
                   IF COLM-LAST-MAINT-DATE > 0 AND COLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'LC' TO COLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LC'
                   IF COLM-LAST-MAINT-DATE > 0 AND COLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'MK' TO COLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE COLLECTED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PAST DUE AMT.
      *----------------------------------------------------------------*
       3440-PRORATE-PAST-DUE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE COLT-PAST-DUE-AMT ROUNDED =
                   COLM-COLLECTED-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO COLT-PAST-DUE-AMT
           END-IF
           ADD COLT-PAST-DUE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ACTION CD:
      * KJ -> TM WHEN NOTICE DATE IS PAST
      * TM -> VL WHEN NOTICE DATE IS PAST
      * VL -> ZS WHEN NOTICE DATE IS PAST
      * ZS -> KJ WHEN NOTICE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3450-SET-ACTION.
           EVALUATE COLM-ACTION-CD
               WHEN 'KJ'
                   IF COLM-NOTICE-DATE > 0 AND COLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TM' TO COLM-ACTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TM'
                   IF COLM-NOTICE-DATE > 0 AND COLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VL' TO COLM-ACTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VL'
                   IF COLM-NOTICE-DATE > 0 AND COLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZS' TO COLM-ACTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZS'
                   IF COLM-NOTICE-DATE > 0 AND COLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KJ' TO COLM-ACTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WRITEOFF AMT BY QUEUE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3460-ACCUM-QUEUE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = COLM-QUEUE-CD
                   ADD COLM-WRITEOFF-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE COLM-QUEUE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE COLM-WRITEOFF-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WRITEOFF YTD BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3470-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = COLT-TRAN-CD
                   ADD COLM-WRITEOFF-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE COLT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE COLM-WRITEOFF-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PROMISED YTD IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3480-LIMIT-PROMISED.
           IF COLM-PROMISED-YTD < 25.00
               MOVE 25.00 TO COLM-PROMISED-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF COLM-PROMISED-YTD > 50000.00
               MOVE 50000.00 TO COLM-PROMISED-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3490-CHECK-NEXT-DUE.
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
      * DAILY INTEREST ON COLLECTED AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3500-ACCRUE-COLLECTED.
           IF COLT-COLLECTED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLT-COLLECTED-AMT * COLM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3510-LIMIT-ITEM.
           IF WS-WORK-AMT < 0.00
               MOVE 0.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 500.00
               MOVE 500.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE BAND TIER FOR THE WRITEOFF YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3520-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-BAND-RATE(1) TO COLM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF COLM-WRITEOFF-YTD NOT < WS-FEE-BAND-LOW(WS-TX)
                   MOVE WS-FEE-BAND-RATE(WS-TX) TO COLM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF COLM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO COLM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE QUEUE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3530-DESCRIBE-QUEUE.
           EVALUATE COLM-QUEUE-CD
               WHEN 'DL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'ER'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'HA'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'HW'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'ME'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'PX'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ZY'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE PAST DUE TIER TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3540-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAST-DUE-TIER-RATE(1) TO COLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF COLM-FEE-AMT NOT < WS-PAST-DUE-TIER-LOW(WS-TX)
                   MOVE WS-PAST-DUE-TIER-RATE(WS-TX) TO COLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF COLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO COLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WRITEOFF YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO COLLECTED AMT.
      *----------------------------------------------------------------*
       3550-PRORATE-COLLECTED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE COLM-COLLECTED-AMT ROUNDED =
                   COLM-WRITEOFF-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO COLM-COLLECTED-AMT
           END-IF
           ADD COLM-COLLECTED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PAST DUE AMT IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3560-LIMIT-PAST-DUE.
           IF COLM-PAST-DUE-AMT < 100.00
               MOVE 100.00 TO COLM-PAST-DUE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF COLM-PAST-DUE-AMT > 50000.00
               MOVE 50000.00 TO COLM-PAST-DUE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3570-COUNT-CYCLE.
           IF COLM-CYCLE-CNT < 9999999
               ADD 1 TO COLM-CYCLE-CNT
           END-IF
           IF COLM-CYCLE-CNT NOT < 10
               MOVE 'Y' TO COLM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3580-ACCRUE-FEE.
           IF COLM-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLM-FEE-AMT * COLM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO COLM-PAST-DUE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAST DUE AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3590-ACCRUE-PAST-DUE.
           IF COLT-PAST-DUE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLT-PAST-DUE-AMT * COLM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO COLM-WRITEOFF-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WRITEOFF AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3600-SPLIT-WRITEOFF.
           COMPUTE WS-WORK-AMT = COLM-WRITEOFF-AMT / 2
           COMPUTE WS-WORK-AMT-2 = COLM-WRITEOFF-AMT - (WS-WORK-AMT * 2)
           COMPUTE COLM-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3610-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF COLM-EFFECTIVE-DATE > 0 AND COLM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD COLT-COLLECTED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD COLT-COLLECTED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD COLT-COLLECTED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD COLT-COLLECTED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLT-COLLECTED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * BE -> GV WHEN EXPIRY DATE IS PAST
      * GV -> JM WHEN EXPIRY DATE IS PAST
      * JM -> LC WHEN EXPIRY DATE IS PAST
      * LC -> MK WHEN EXPIRY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3620-SET-CYCLE.
           EVALUATE COLM-CYCLE-CD
               WHEN 'BE'
                   IF COLM-EXPIRY-DATE > 0 AND COLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GV' TO COLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GV'
                   IF COLM-EXPIRY-DATE > 0 AND COLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JM' TO COLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JM'
                   IF COLM-EXPIRY-DATE > 0 AND COLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LC' TO COLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LC'
                   IF COLM-EXPIRY-DATE > 0 AND COLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MK' TO COLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E379 DESC REQUIRED
      * E163 FEE AMT NEGATIVE
      * E322 FEE AMT OVER 10000
      * E929 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3630-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN COLM-DESC = SPACES
                   MOVE 'E379' TO WS-REJECT-CODE
               WHEN COLT-FEE-AMT < 0
                   MOVE 'E163' TO WS-REJECT-CODE
               WHEN COLT-FEE-AMT > 10000.00
                   MOVE 'E322' TO WS-REJECT-CODE
               WHEN COLM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E929' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3640-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF COLM-EFFECTIVE-DATE > 0 AND COLM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD COLM-WRITEOFF-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD COLM-WRITEOFF-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD COLM-WRITEOFF-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD COLM-WRITEOFF-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLM-WRITEOFF-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * COLLECTED YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3650-FLAG-COLLECTED.
           IF COLM-COLLECTED-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE COLM-COLLECTED-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PROMISED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FEE AMT.
      *----------------------------------------------------------------*
       3660-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE COLT-FEE-AMT ROUNDED =
                   COLM-PROMISED-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO COLT-FEE-AMT
           END-IF
           ADD COLT-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 120 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3670-CHECK-EFF.
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
           IF WS-DAYS > 120
               MOVE 'Y' TO COLT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3680-ACCRUE-FEE-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * COLM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO COLM-PAST-DUE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3690-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF COLM-LAST-MAINT-DATE > 0 AND COLM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD COLM-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD COLM-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD COLM-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD COLM-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLM-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3700-COUNT-RETRY.
           IF COLM-RETRY-CNT < 9999999
               ADD 1 TO COLM-RETRY-CNT
           END-IF
           IF COLM-RETRY-CNT NOT < 6
               MOVE 'Y' TO COLM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WRITEOFF AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3710-FLAG-WRITEOFF.
           IF COLT-WRITEOFF-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE COLT-WRITEOFF-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3720-DESCRIBE-STATUS.
           EVALUATE COLM-STATUS-CD
               WHEN 'PS'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'UR'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'XM'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'YA'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE FEE BANDX TIER FOR THE PROMISED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3730-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-BANDX-RATE(1) TO COLM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF COLM-PROMISED-AMT NOT < WS-FEE-BANDX-LOW(WS-TX)
                   MOVE WS-FEE-BANDX-RATE(WS-TX) TO COLM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF COLM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO COLM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * PS -> UR WHEN EFF DATE IS PAST
      * UR -> XM WHEN EFF DATE IS PAST
      * XM -> YA WHEN EFF DATE IS PAST
      * YA -> PS WHEN EFF DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3740-SET-STATUS.
           EVALUATE COLM-STATUS-CD
               WHEN 'PS'
                   IF COLT-EFF-DATE > 0 AND COLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UR' TO COLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UR'
                   IF COLT-EFF-DATE > 0 AND COLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XM' TO COLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XM'
                   IF COLT-EFF-DATE > 0 AND COLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YA' TO COLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YA'
                   IF COLT-EFF-DATE > 0 AND COLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PS' TO COLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E852 CUST ID REQUIRED
      * E925 WRITEOFF YTD NEGATIVE
      * E754 WRITEOFF YTD OVER 1000000
      * E699 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3750-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN COLM-CUST-ID = SPACES
                   MOVE 'E852' TO WS-REJECT-CODE
               WHEN COLM-WRITEOFF-YTD < 0
                   MOVE 'E925' TO WS-REJECT-CODE
               WHEN COLM-WRITEOFF-YTD > 1000000.00
                   MOVE 'E754' TO WS-REJECT-CODE
               WHEN COLT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E699' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WRITEOFF YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3760-ACCRUE-WRITEOFF.
           IF COLM-WRITEOFF-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLM-WRITEOFF-YTD * COLM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO COLM-PROMISED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 15 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3770-CHECK-EXPIRY.
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
           IF WS-DAYS > 15
               MOVE 'Y' TO COLM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E157 NAME REQUIRED
      * E257 PROMISED AMT NEGATIVE
      * E156 PROMISED AMT OVER 5000
      * E707 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3780-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN COLM-NAME = SPACES
                   MOVE 'E157' TO WS-REJECT-CODE
               WHEN COLM-PROMISED-AMT < 0
                   MOVE 'E257' TO WS-REJECT-CODE
               WHEN COLM-PROMISED-AMT > 5000.00
                   MOVE 'E156' TO WS-REJECT-CODE
               WHEN COLM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E707' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAST DUE YTD IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3790-LIMIT-PAST-DUE.
           IF COLM-PAST-DUE-YTD < 25.00
               MOVE 25.00 TO COLM-PAST-DUE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF COLM-PAST-DUE-YTD > 500.00
               MOVE 500.00 TO COLM-PAST-DUE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAST DUE FEE ON THE FEE YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3800-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN COLM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO COLT-PAST-DUE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE COLT-PAST-DUE-AMT ROUNDED = COLM-FEE-YTD *
                       COLM-PENALTY-PCT / 200
               WHEN COLM-QUEUE-CD = 'DL'
                   MOVE 10.00 TO COLT-PAST-DUE-AMT
               WHEN COLM-QUEUE-CD = 'ER'
                   MOVE 3.00 TO COLT-PAST-DUE-AMT
               WHEN OTHER
                   COMPUTE COLT-PAST-DUE-AMT ROUNDED = COLM-FEE-YTD *
                       COLM-PENALTY-PCT / 100
           END-EVALUATE
           IF COLT-PAST-DUE-AMT > 0 AND COLT-PAST-DUE-AMT < 5.00
               MOVE 5.00 TO COLT-PAST-DUE-AMT
           END-IF
           IF COLT-PAST-DUE-AMT > 50.00
               MOVE 50.00 TO COLT-PAST-DUE-AMT
           END-IF
           ADD COLT-PAST-DUE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON COLLECTED AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3810-ACCRUE-COLLECTED.
           IF COLT-COLLECTED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLT-COLLECTED-AMT * COLM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO COLM-WRITEOFF-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3820-COUNT-CYCLE.
           IF COLM-CYCLE-CNT < 9999999
               ADD 1 TO COLM-CYCLE-CNT
           END-IF
           IF COLM-CYCLE-CNT NOT < 5
               MOVE 'Y' TO COLM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3830-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   COLM-FEE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3840-AGE-LAST-PAY.
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
                   ADD COLM-PAST-DUE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD COLM-PAST-DUE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD COLM-PAST-DUE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD COLM-PAST-DUE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLM-PAST-DUE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E382 CONTACT REQUIRED
      * E898 COLLECTED YTD NEGATIVE
      * E587 COLLECTED YTD OVER 1000000
      * E982 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3850-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN COLM-CONTACT = SPACES
                   MOVE 'E382' TO WS-REJECT-CODE
               WHEN COLM-COLLECTED-YTD < 0
                   MOVE 'E898' TO WS-REJECT-CODE
               WHEN COLM-COLLECTED-YTD > 1000000.00
                   MOVE 'E587' TO WS-REJECT-CODE
               WHEN COLM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E982' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE COLLECTED FEE ON THE WRITEOFF YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3860-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN COLM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO COLT-COLLECTED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE COLT-COLLECTED-AMT ROUNDED =
                       COLM-WRITEOFF-YTD * COLM-PENALTY-PCT / 200
               WHEN COLM-CYCLE-CD = 'BE'
                   MOVE 20.00 TO COLT-COLLECTED-AMT
               WHEN COLM-CYCLE-CD = 'GV'
                   MOVE 10.00 TO COLT-COLLECTED-AMT
               WHEN OTHER
                   COMPUTE COLT-COLLECTED-AMT ROUNDED =
                       COLM-WRITEOFF-YTD * COLM-PENALTY-PCT / 100
           END-EVALUATE
           IF COLT-COLLECTED-AMT > 0 AND COLT-COLLECTED-AMT < 5.00
               MOVE 5.00 TO COLT-COLLECTED-AMT
           END-IF
           IF COLT-COLLECTED-AMT > 75.00
               MOVE 75.00 TO COLT-COLLECTED-AMT
           END-IF
           ADD COLT-COLLECTED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE WRITEOFF AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3870-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = COLM-REGION-CD
                   ADD COLM-WRITEOFF-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE COLM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE COLM-WRITEOFF-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3880-DESCRIBE-REGION.
           EVALUATE COLM-REGION-CD
               WHEN 'AT'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'AU'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'XR'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'XU'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3890-AGE-NEXT-REVIEW.
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
                   ADD COLT-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD COLT-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD COLT-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD COLT-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLT-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE PAST DUE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO COLLECTED YTD.
      *----------------------------------------------------------------*
       3900-PRORATE-COLLECTED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE COLM-COLLECTED-YTD ROUNDED =
                   COLM-PAST-DUE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO COLM-COLLECTED-YTD
           END-IF
           ADD COLM-COLLECTED-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE FEE BAND TIER FOR THE WRITEOFF YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3910-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-BAND-RATE(1) TO COLM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF COLM-WRITEOFF-YTD NOT < WS-FEE-BAND-LOW(WS-TX)
                   MOVE WS-FEE-BAND-RATE(WS-TX) TO COLM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF COLM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO COLM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE BANDX TIER FOR THE PROMISED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3920-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-BANDX-RATE(1) TO COLM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF COLM-PROMISED-YTD NOT < WS-FEE-BANDX-LOW(WS-TX)
                   MOVE WS-FEE-BANDX-RATE(WS-TX) TO COLM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF COLM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO COLM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * COLLECTED AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3930-FLAG-COLLECTED.
           IF COLT-COLLECTED-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE COLT-COLLECTED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * COLLECTED AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3940-FLAG-COLLECTED.
           IF COLM-COLLECTED-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE COLM-COLLECTED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WRITEOFF AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WRITEOFF AMT.
      *----------------------------------------------------------------*
       3950-PRORATE-WRITEOFF.
           IF WS-PERIOD-DAYS > 0
               COMPUTE COLM-WRITEOFF-AMT ROUNDED =
                   COLT-WRITEOFF-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO COLM-WRITEOFF-AMT
           END-IF
           ADD COLM-WRITEOFF-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 90 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3960-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF COLM-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO COLM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN NOTICE DATE IS PAST
      * BAT -> MAL WHEN NOTICE DATE IS PAST
      * MAL -> TEL WHEN NOTICE DATE IS PAST
      * TEL -> ONL WHEN NOTICE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3970-SET-CHANNEL.
           EVALUATE COLT-CHANNEL-CD
               WHEN 'BRN'
                   IF COLM-NOTICE-DATE > 0 AND COLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO COLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF COLM-NOTICE-DATE > 0 AND COLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO COLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF COLM-NOTICE-DATE > 0 AND COLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO COLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF COLM-NOTICE-DATE > 0 AND COLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO COLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE PAST DUE YTD FEE ON THE WRITEOFF AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3980-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN COLT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO COLM-PAST-DUE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLM-OVERRIDE-FLAG = 'Y'
                   COMPUTE COLM-PAST-DUE-YTD ROUNDED = COLT-WRITEOFF-AMT
                       * COLM-MARGIN / 200
               WHEN COLT-TRAN-CD = 'PR'
                   MOVE 10.00 TO COLM-PAST-DUE-YTD
               WHEN COLT-TRAN-CD = 'CL'
                   MOVE 20.00 TO COLM-PAST-DUE-YTD
               WHEN OTHER
                   COMPUTE COLM-PAST-DUE-YTD ROUNDED = COLT-WRITEOFF-AMT
                       * COLM-MARGIN / 100
           END-EVALUATE
           IF COLM-PAST-DUE-YTD > 0 AND COLM-PAST-DUE-YTD < 5.00
               MOVE 5.00 TO COLM-PAST-DUE-YTD
           END-IF
           IF COLM-PAST-DUE-YTD > 35.00
               MOVE 35.00 TO COLM-PAST-DUE-YTD
           END-IF
           ADD COLM-PAST-DUE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 45 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3990-CHECK-LAST-PAY.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO COLM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4000-FLAG-FEE.
           IF COLT-FEE-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE COLT-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4010-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = COLM-STATUS-CD
                   ADD COLM-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE COLM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE COLM-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4020-FLAG-FEE.
           IF COLM-FEE-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE COLM-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-90, 91-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4030-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF COLM-NOTICE-DATE > 0 AND COLM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD COLM-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD COLM-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD COLM-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD COLM-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLM-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 10 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4040-CHECK-NEXT-REVIEW.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO COLM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FEE YTD.
      *----------------------------------------------------------------*
       4050-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE COLM-FEE-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO COLM-FEE-YTD
           END-IF
           ADD COLM-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAST DUE AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4060-ACCRUE-PAST-DUE.
           IF COLT-PAST-DUE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLT-PAST-DUE-AMT * COLM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO COLT-FEE-AMT
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
      * PRORATE PROMISED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       4080-PRORATE-PROMISED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   COLM-PROMISED-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE AMT BY QUEUE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4090-ACCUM-QUEUE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = COLM-QUEUE-CD
                   ADD COLT-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE COLM-QUEUE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE COLT-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE BANDX TIER FOR THE COLLECTED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4100-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-BANDX-RATE(1) TO COLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF COLM-COLLECTED-YTD NOT < WS-FEE-BANDX-LOW(WS-TX)
                   MOVE WS-FEE-BANDX-RATE(WS-TX) TO COLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF COLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO COLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PROMISED AMT IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4110-LIMIT-PROMISED.
           IF COLM-PROMISED-AMT < 0.00
               MOVE 0.00 TO COLM-PROMISED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF COLM-PROMISED-AMT > 500.00
               MOVE 500.00 TO COLM-PROMISED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4120-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = COLM-FEE-YTD / 12
           COMPUTE WS-WORK-AMT-2 = COLM-FEE-YTD - (WS-WORK-AMT * 12)
           COMPUTE COLM-WRITEOFF-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WRITEOFF AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4130-SPLIT-WRITEOFF.
           COMPUTE WS-WORK-AMT = COLT-WRITEOFF-AMT / 12
           COMPUTE WS-WORK-AMT-2 = COLT-WRITEOFF-AMT - (WS-WORK-AMT *
               12)
           COMPUTE COLT-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE QUEUE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4140-DESCRIBE-QUEUE.
           EVALUATE COLM-QUEUE-CD
               WHEN 'DL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ER'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'HA'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'HW'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'ME'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'PX'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'ZY'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4150-ACCRUE-FEE.
           IF COLT-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLT-FEE-AMT * COLM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO COLT-PAST-DUE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E761 OPERATOR REQUIRED
      * E024 FEE AMT NEGATIVE
      * E424 FEE AMT OVER 10000
      * E610 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       4160-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN COLT-OPERATOR = SPACES
                   MOVE 'E761' TO WS-REJECT-CODE
               WHEN COLM-FEE-AMT < 0
                   MOVE 'E024' TO WS-REJECT-CODE
               WHEN COLM-FEE-AMT > 10000.00
                   MOVE 'E424' TO WS-REJECT-CODE
               WHEN COLT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E610' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4170-SPLIT-PAST-DUE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 12)
           COMPUTE COLM-PAST-DUE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 10 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4180-CHECK-EXPIRY.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO COLM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4190-FLAG-FEE.
           IF COLT-FEE-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE COLT-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * BE -> GV WHEN EFF DATE IS PAST
      * GV -> JM WHEN EFF DATE IS PAST
      * JM -> LC WHEN EFF DATE IS PAST
      * LC -> MK WHEN EFF DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4200-SET-CYCLE.
           EVALUATE COLM-CYCLE-CD
               WHEN 'BE'
                   IF COLT-EFF-DATE > 0 AND COLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GV' TO COLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GV'
                   IF COLT-EFF-DATE > 0 AND COLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JM' TO COLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JM'
                   IF COLT-EFF-DATE > 0 AND COLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LC' TO COLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LC'
                   IF COLT-EFF-DATE > 0 AND COLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MK' TO COLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       4210-COUNT-NOTICE.
           IF COLM-NOTICE-CNT < 9999999
               ADD 1 TO COLM-NOTICE-CNT
           END-IF
           IF COLM-NOTICE-CNT NOT < 10
               MOVE 'Y' TO COLM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAST DUE AMT IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4220-LIMIT-PAST-DUE.
           IF COLM-PAST-DUE-AMT < 10.00
               MOVE 10.00 TO COLM-PAST-DUE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF COLM-PAST-DUE-AMT > 500.00
               MOVE 500.00 TO COLM-PAST-DUE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE BANDX TIER FOR THE COLLECTED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4230-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-BANDX-RATE(1) TO COLM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF COLM-COLLECTED-AMT NOT < WS-FEE-BANDX-LOW(WS-TX)
                   MOVE WS-FEE-BANDX-RATE(WS-TX) TO COLM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF COLM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO COLM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAST DUE TIER TIER FOR THE PROMISED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4240-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAST-DUE-TIER-RATE(1) TO COLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF COLM-PROMISED-AMT NOT < WS-PAST-DUE-TIER-LOW(WS-TX)
                   MOVE WS-PAST-DUE-TIER-RATE(WS-TX) TO COLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF COLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO COLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E695 NAME REQUIRED
      * E048 PROMISED AMT NEGATIVE
      * E362 PROMISED AMT OVER 250000
      * E001 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       4250-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN COLM-NAME = SPACES
                   MOVE 'E695' TO WS-REJECT-CODE
               WHEN COLM-PROMISED-AMT < 0
                   MOVE 'E048' TO WS-REJECT-CODE
               WHEN COLM-PROMISED-AMT > 250000.00
                   MOVE 'E362' TO WS-REJECT-CODE
               WHEN COLM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E001' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E196 ADDR REQUIRED
      * E795 WORK AMT NEGATIVE
      * E564 WORK AMT OVER 5000
      * E652 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       4260-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN COLM-ADDR = SPACES
                   MOVE 'E196' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E795' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 5000.00
                   MOVE 'E564' TO WS-REJECT-CODE
               WHEN COLM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E652' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * PR -> CL WHEN LAST MAINT DATE IS PAST
      * CL -> LT WHEN LAST MAINT DATE IS PAST
      * LT -> WO WHEN LAST MAINT DATE IS PAST
      * WO -> RF WHEN LAST MAINT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4270-SET-TRAN.
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
      * ASSESS THE WORK FEE ON THE WRITEOFF AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4280-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN COLM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLT-REVERSAL-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = COLT-WRITEOFF-AMT *
                       COLM-PENALTY-PCT / 200
               WHEN COLM-REASON-CD = 'AT'
                   MOVE 7.00 TO WS-WORK-AMT
               WHEN COLM-REASON-CD = 'BJ'
                   MOVE 15.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = COLT-WRITEOFF-AMT *
                       COLM-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 2.00
               MOVE 2.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 25.00
               MOVE 25.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WRITEOFF AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4290-ACCRUE-WRITEOFF.
           IF COLT-WRITEOFF-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLT-WRITEOFF-AMT * COLM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO COLM-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       4300-COUNT-RETRY.
           IF COLM-RETRY-CNT < 9999999
               ADD 1 TO COLM-RETRY-CNT
           END-IF
           IF COLM-RETRY-CNT NOT < 10
               MOVE 'Y' TO COLM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 365 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4310-CHECK-EXPIRY.
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
           IF WS-DAYS > 365
               MOVE 'Y' TO COLM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAST DUE AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4320-ACCRUE-PAST-DUE.
           IF COLT-PAST-DUE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLT-PAST-DUE-AMT * COLM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO COLM-COLLECTED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 10 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4330-CHECK-NEXT-REVIEW.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO COLM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E213 OPERATOR REQUIRED
      * E490 PROMISED AMT NEGATIVE
      * E290 PROMISED AMT OVER 25000
      * E948 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       4340-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN COLT-OPERATOR = SPACES
                   MOVE 'E213' TO WS-REJECT-CODE
               WHEN COLM-PROMISED-AMT < 0
                   MOVE 'E490' TO WS-REJECT-CODE
               WHEN COLM-PROMISED-AMT > 25000.00
                   MOVE 'E290' TO WS-REJECT-CODE
               WHEN COLT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E948' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4350-AGE-NEXT-DUE.
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
                   ADD COLT-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD COLT-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD COLT-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD COLT-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLT-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4360-DESCRIBE-REASON.
           EVALUATE COLM-REASON-CD
               WHEN 'AT'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'BJ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'GG'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'ZR'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4370-AGE-EXPIRY.
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
                   ADD COLM-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD COLM-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD COLM-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD COLM-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLM-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WRITEOFF YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4380-SPLIT-WRITEOFF.
           COMPUTE WS-WORK-AMT = COLM-WRITEOFF-YTD / 2
           COMPUTE WS-WORK-AMT-2 = COLM-WRITEOFF-YTD - (WS-WORK-AMT * 2)
           COMPUTE COLM-PAST-DUE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4390-AGE-OPEN.
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
                   ADD COLM-PROMISED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD COLM-PROMISED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD COLM-PROMISED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD COLM-PROMISED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLM-PROMISED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E215 REJECT CD REQUIRED
      * E756 WORK AMT 2 NEGATIVE
      * E318 WORK AMT 2 OVER 10000
      * E920 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       4400-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN COLT-REJECT-CD = SPACES
                   MOVE 'E215' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E756' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 10000.00
                   MOVE 'E318' TO WS-REJECT-CODE
               WHEN COLM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E920' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WRITEOFF AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4410-SPLIT-WRITEOFF.
           COMPUTE WS-WORK-AMT = COLT-WRITEOFF-AMT / 3
           COMPUTE WS-WORK-AMT-2 = COLT-WRITEOFF-AMT - (WS-WORK-AMT * 3)
           COMPUTE COLM-WRITEOFF-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN CLOSE DATE IS PAST
      * BAT -> MAL WHEN CLOSE DATE IS PAST
      * MAL -> TEL WHEN CLOSE DATE IS PAST
      * TEL -> ONL WHEN CLOSE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4420-SET-CHANNEL.
           EVALUATE COLT-CHANNEL-CD
               WHEN 'BRN'
                   IF COLM-CLOSE-DATE > 0 AND COLM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO COLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF COLM-CLOSE-DATE > 0 AND COLM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO COLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF COLM-CLOSE-DATE > 0 AND COLM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO COLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF COLM-CLOSE-DATE > 0 AND COLM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO COLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 10 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4430-CHECK-EXPIRY.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO COLM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4440-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF COLM-EFFECTIVE-DATE > 0 AND COLM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD COLM-PAST-DUE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD COLM-PAST-DUE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD COLM-PAST-DUE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD COLM-PAST-DUE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLM-PAST-DUE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ACTION CD:
      * KJ -> TM WHEN NEXT REVIEW DATE IS PAST
      * TM -> VL WHEN NEXT REVIEW DATE IS PAST
      * VL -> ZS WHEN NEXT REVIEW DATE IS PAST
      * ZS -> KJ WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4450-SET-ACTION.
           EVALUATE COLM-ACTION-CD
               WHEN 'KJ'
                   IF COLM-NEXT-REVIEW-DATE > 0 AND
                       COLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'TM' TO COLM-ACTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TM'
                   IF COLM-NEXT-REVIEW-DATE > 0 AND
                       COLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'VL' TO COLM-ACTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VL'
                   IF COLM-NEXT-REVIEW-DATE > 0 AND
                       COLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'ZS' TO COLM-ACTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZS'
                   IF COLM-NEXT-REVIEW-DATE > 0 AND
                       COLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'KJ' TO COLM-ACTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE PAST DUE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PROMISED YTD.
      *----------------------------------------------------------------*
       4460-PRORATE-PROMISED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE COLM-PROMISED-YTD ROUNDED =
                   COLT-PAST-DUE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO COLM-PROMISED-YTD
           END-IF
           ADD COLM-PROMISED-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E918 ADDR REQUIRED
      * E075 PAST DUE AMT NEGATIVE
      * E766 PAST DUE AMT OVER 50000
      * E302 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       4470-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN COLM-ADDR = SPACES
                   MOVE 'E918' TO WS-REJECT-CODE
               WHEN COLT-PAST-DUE-AMT < 0
                   MOVE 'E075' TO WS-REJECT-CODE
               WHEN COLT-PAST-DUE-AMT > 50000.00
                   MOVE 'E766' TO WS-REJECT-CODE
               WHEN COLM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E302' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 10 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4480-CHECK-LAST-PAY.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO COLM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE COLLECTED YTD FEE ON THE COLLECTED AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       4490-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN COLT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO COLM-COLLECTED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE COLM-COLLECTED-YTD ROUNDED =
                       COLM-COLLECTED-AMT * COLM-FEE-PCT / 200
               WHEN COLM-QUEUE-CD = 'DL'
                   MOVE 7.00 TO COLM-COLLECTED-YTD
               WHEN COLM-QUEUE-CD = 'ER'
                   MOVE 10.00 TO COLM-COLLECTED-YTD
               WHEN OTHER
                   COMPUTE COLM-COLLECTED-YTD ROUNDED =
                       COLM-COLLECTED-AMT * COLM-FEE-PCT / 100
           END-EVALUATE
           IF COLM-COLLECTED-YTD > 0 AND COLM-COLLECTED-YTD < 10.00
               MOVE 10.00 TO COLM-COLLECTED-YTD
           END-IF
           IF COLM-COLLECTED-YTD > 150.00
               MOVE 150.00 TO COLM-COLLECTED-YTD
           END-IF
           ADD COLM-COLLECTED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       4500-AGE-NEXT-REVIEW.
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
                   ADD COLM-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD COLM-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD COLM-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD COLM-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLM-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE PROMISED AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4510-ACCUM-CHANNEL.
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
      * STATUS TRANSITIONS FOR QUEUE CD:
      * DL -> ER WHEN EXPIRY DATE IS PAST
      * ER -> HA WHEN EXPIRY DATE IS PAST
      * HA -> HW WHEN EXPIRY DATE IS PAST
      * HW -> ME WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4520-SET-QUEUE.
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
      * STATUS TRANSITIONS FOR STATUS CD:
      * PS -> UR WHEN POST DATE IS PAST
      * UR -> XM WHEN POST DATE IS PAST
      * XM -> YA WHEN POST DATE IS PAST
      * YA -> PS WHEN POST DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4530-SET-STATUS.
           EVALUATE COLM-STATUS-CD
               WHEN 'PS'
                   IF COLT-POST-DATE > 0 AND COLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UR' TO COLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UR'
                   IF COLT-POST-DATE > 0 AND COLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XM' TO COLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XM'
                   IF COLT-POST-DATE > 0 AND COLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YA' TO COLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YA'
                   IF COLT-POST-DATE > 0 AND COLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PS' TO COLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAST DUE AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4540-FLAG-PAST-DUE.
           IF COLM-PAST-DUE-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE COLM-PAST-DUE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAST DUE TIER TIER FOR THE COLLECTED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4550-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PAST-DUE-TIER-RATE(1) TO COLM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF COLM-COLLECTED-AMT NOT < WS-PAST-DUE-TIER-LOW(WS-TX)
                   MOVE WS-PAST-DUE-TIER-RATE(WS-TX) TO COLM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF COLM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO COLM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * COLLECTED AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4560-FLAG-COLLECTED.
           IF COLT-COLLECTED-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE COLT-COLLECTED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAST DUE TIER TIER FOR THE WRITEOFF AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4570-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAST-DUE-TIER-RATE(1) TO COLM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF COLT-WRITEOFF-AMT NOT < WS-PAST-DUE-TIER-LOW(WS-TX)
                   MOVE WS-PAST-DUE-TIER-RATE(WS-TX) TO COLM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF COLM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO COLM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WRITEOFF AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4580-SPLIT-WRITEOFF.
           COMPUTE WS-WORK-AMT = COLM-WRITEOFF-AMT / 2
           COMPUTE WS-WORK-AMT-2 = COLM-WRITEOFF-AMT - (WS-WORK-AMT * 2)
           COMPUTE COLM-COLLECTED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
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
                   MOVE CLLIM-COLLECTED-YTD
                     TO COLM-COLLECTED-YTD
                   MOVE CLLIM-PAST-DUE-AMT
                     TO COLM-PAST-DUE-AMT
                   MOVE CLLIM-PROMISED-YTD
                     TO COLM-PROMISED-YTD
                   MOVE CLLIM-WRITEOFF-YTD
                     TO COLM-WRITEOFF-YTD
                   MOVE CLLIM-REVIEW-FLAG
                     TO COLM-REVIEW-FLAG
                   MOVE CLLIM-PREFERRED-FLAG
                     TO COLM-PREFERRED-FLAG
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
           DISPLAY 'CLLAGE READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'CLLAGE' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
