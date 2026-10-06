      *================================================================*
      * PROGRAM  : TXXRNEW                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : TELEX WIRE MESSAGING - RENEWAL. READS THE TLX       *
      *            TRANSACTIONS, UPDATES THE TLX MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. TESTED TELEX FUNDS TRANSFER     *
      *            MESSAGES AND TEST KEY VALIDATION                    *
      * INPUT    : TLXMST (KSDS), TLXTRN, BANKCTL                      *
      * OUTPUT   : TLXOUT, TXXRNRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, TXPRC, TXELG, TXCAL     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1993-01-08 DPS  ORIGINAL                                       *
      * 2003-07-03 GAF  COVER CUTOFF MOVED PRB9060                     *
      * 2006-02-26 PQR  COVER EXCEPTIONS TO OPS QUEUE CHG0824          *
      * 2006-05-24 JLM  CHARGE LIMIT REVISED CHG8172                   *
      * 2009-09-14 EWS  SETTLE TABLE EXPANDED PRB6515                  *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TXXRNEW.
       AUTHOR. D P SHAW.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "TLXMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS TLXM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "TLXTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "TLXOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "TXXRNRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY TLXMST.
       FD  TRAN-FILE.
       COPY TLXTRN.
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
      * WS-QUEUED-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-QUEUED-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000242000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000546000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001250000000141000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003750000000773000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003800000000312500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004800000000362500'.
       01  WS-QUEUED-GRID-TBL REDEFINES WS-QUEUED-GRID-VALUES.
           05  WS-QUEUED-GRID-ROW OCCURS 6 TIMES.
               10  WS-QUEUED-GRID-LOW      PIC 9(09)V99.
               10  WS-QUEUED-GRID-RATE     PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-CORRESP-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CORRESP-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000627000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000813000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002600000000813000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003100000000551000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008100000000637000'.
       01  WS-CORRESP-GRID-TBL REDEFINES WS-CORRESP-GRID-VALUES.
           05  WS-CORRESP-GRID-ROW OCCURS 5 TIMES.
               10  WS-CORRESP-GRID-LOW     PIC 9(09)V99.
               10  WS-CORRESP-GRID-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-SETTLE-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-SETTLE-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000108000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000413500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000893000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000200000000093500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000300000000312000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000350000000898000'.
       01  WS-SETTLE-GRID-TBL REDEFINES WS-SETTLE-GRID-VALUES.
           05  WS-SETTLE-GRID-ROW OCCURS 6 TIMES.
               10  WS-SETTLE-GRID-LOW      PIC 9(09)V99.
               10  WS-SETTLE-GRID-RATE     PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  TXXRNEW  TELEX WIRE MESSAGING'.
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
       COPY TXPRCP.
       COPY TXELGP.
       COPY TXCALP.

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
               MOVE 'TLXMST' TO ABND-FILE-NAME
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
           MOVE TLXT-KEY TO TLXM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-DESCRIBE-BRANCH
               PERFORM 3010-AGE-LAST-ACT
               PERFORM 3020-SPLIT-QUEUED
               PERFORM 3030-ACCRUE-COVER
               PERFORM 3040-SPLIT-SETTLE
               PERFORM 3050-SPLIT-COVER
               PERFORM 3060-ACCRUE-SETTLE
               PERFORM 3070-SELECT-RATE
               PERFORM 3080-ASSESS-SUSPEND
               PERFORM 3090-SPLIT-QUEUED
               PERFORM 3100-CHECK-EFF
               PERFORM 3110-SET-TEST-KEY
               PERFORM 3120-COUNT-PAYMENT
               PERFORM 3130-LIMIT-COVER
               PERFORM 3140-SPLIT-SETTLE
               PERFORM 3150-SELECT-DISCOUNT
               PERFORM 3160-ACCRUE-WIRE
               PERFORM 3170-EDIT-DESC
               PERFORM 3180-SET-MSG-TYPE
               PERFORM 3190-SELECT-RATE
               PERFORM 3200-SELECT-FEE-PCT
               PERFORM 3210-AGE-NEXT-DUE
               PERFORM 3220-PRORATE-CORRESP
               PERFORM 3230-CHECK-CLOSE
               PERFORM 3240-PRORATE-ITEM
               PERFORM 3250-COUNT-PAYMENT
               PERFORM 3260-LIMIT-COVER
               PERFORM 3270-FLAG-QUEUED
               PERFORM 3280-FLAG-WIRE
               PERFORM 3290-DESCRIBE-BRANCH
               PERFORM 3300-FLAG-WIRE
               PERFORM 3310-CHECK-EFFECTIVE
               PERFORM 3320-LIMIT-SETTLE
               PERFORM 3330-LIMIT-CORRESP
               PERFORM 3340-CHECK-LAST-ACT
               PERFORM 3350-EDIT-ADDR
               PERFORM 3360-CHECK-NOTICE
               PERFORM 3370-SET-TRAN
               PERFORM 3380-SELECT-DISCOUNT
               PERFORM 3390-SPLIT-QUEUED
               PERFORM 3400-SET-CHANNEL
               PERFORM 3410-LIMIT-SETTLE
               PERFORM 3420-COUNT-PAYMENT
               PERFORM 3430-DESCRIBE-TRAN
               PERFORM 3440-ACCUM-BRANCH
               PERFORM 3450-ACCUM-CHANNEL
               PERFORM 3460-SPLIT-SETTLE
               PERFORM 3470-CHECK-LAST-PAY
               PERFORM 3480-PRORATE-CHARGE
               PERFORM 3490-FLAG-COVER
               PERFORM 3500-AGE-LAST-PAY
               PERFORM 3510-SPLIT-QUEUED
               PERFORM 3520-SPLIT-QUEUED
               PERFORM 3530-AGE-NEXT-REVIEW
               PERFORM 3540-SPLIT-CORRESP
               PERFORM 3550-LIMIT-WIRE
               PERFORM 3560-ACCRUE-WIRE
               PERFORM 3570-SELECT-FEE-PCT
               PERFORM 3580-PRORATE-CORRESP
               PERFORM 3590-COUNT-EXCEPT
               PERFORM 3600-SELECT-RATE
               PERFORM 3610-PRORATE-COVER
               PERFORM 3620-FLAG-WIRE
               PERFORM 3630-PRORATE-CORRESP
               PERFORM 3640-ACCRUE-SETTLE
               PERFORM 3650-SELECT-FEE-PCT
               PERFORM 3660-SET-TRAN
               PERFORM 3670-LIMIT-COVER
               PERFORM 3680-SELECT-FEE-PCT
               PERFORM 3690-EDIT-CONTACT
               PERFORM 3700-PRORATE-CORRESP
               PERFORM 3710-SELECT-DISCOUNT
               PERFORM 3720-SELECT-FEE-PCT
               PERFORM 3730-COUNT-ITEM
               PERFORM 3740-AGE-POST
               PERFORM 3750-CHECK-EFF
               PERFORM 3760-AGE-OPEN
               PERFORM 3770-CHECK-CLOSE
               PERFORM 3780-COUNT-SEQ
               PERFORM 3790-SET-CHANNEL
               PERFORM 3800-CHECK-NEXT-REVIEW
               PERFORM 3810-ASSESS-OVERRIDE
               PERFORM 3820-ACCRUE-SETTLE
               PERFORM 3830-FLAG-QUEUED
               PERFORM 3840-PRORATE-CORRESP
               PERFORM 3850-FLAG-WIRE
               PERFORM 3860-LIMIT-SETTLE
               PERFORM 3870-COUNT-EXCEPT
               PERFORM 3880-LIMIT-CHARGE
               PERFORM 3890-ACCUM-TEST-KEY
               PERFORM 3900-LIMIT-ITEM
               PERFORM 3910-SPLIT-WIRE
               PERFORM 3920-COUNT-PAYMENT
               PERFORM 3930-SET-CHANNEL
               PERFORM 3940-PRORATE-QUEUED
               PERFORM 3950-AGE-NEXT-DUE
               PERFORM 3960-EDIT-OPERATOR
               PERFORM 3970-ACCUM-TRAN
               PERFORM 3980-SELECT-DISCOUNT
               PERFORM 3990-DESCRIBE-TRAN
               PERFORM 4000-PRORATE-CORRESP
               PERFORM 4010-ACCUM-TRAN
               PERFORM 4020-DESCRIBE-BRANCH
               PERFORM 4030-COUNT-ITEM
               PERFORM 4040-LIMIT-CORRESP
               PERFORM 4050-CHECK-NEXT-REVIEW
               PERFORM 4060-SPLIT-CORRESP
               PERFORM 4070-DESCRIBE-REASON
               PERFORM 4080-ACCUM-TEST-KEY
               PERFORM 4090-SET-MSG-TYPE
               PERFORM 4100-DESCRIBE-REASON
               PERFORM 4110-LIMIT-CHARGE
               PERFORM 4120-SELECT-RATE
               PERFORM 4130-ACCUM-REGION
               PERFORM 4140-PRORATE-COVER
               PERFORM 7020-CALL-TXPRC
               PERFORM 7000-CALL-TXELG
               PERFORM 7030-CALL-TXCAL
               REWRITE TLX-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'TLXMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM TLX-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3000-DESCRIBE-BRANCH.
           EVALUATE TLXM-BRANCH-CD
               WHEN 'CC'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'KR'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'LD'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'LS'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'XL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3010-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF TLXM-LAST-ACT-DATE > 0 AND TLXM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TLXT-WIRE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD TLXT-WIRE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD TLXT-WIRE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TLXT-WIRE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLXT-WIRE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT QUEUED AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3020-SPLIT-QUEUED.
           COMPUTE WS-WORK-AMT = TLXT-QUEUED-AMT / 4
           COMPUTE WS-WORK-AMT-2 = TLXT-QUEUED-AMT - (WS-WORK-AMT * 4)
           COMPUTE TLXM-CHARGE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON COVER AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3030-ACCRUE-COVER.
           IF TLXM-COVER-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLXM-COVER-AMT * TLXM-RATE / 100 / 360
               ADD WS-WORK-AMT TO TLXM-SETTLE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SETTLE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3040-SPLIT-SETTLE.
           COMPUTE WS-WORK-AMT = TLXT-SETTLE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = TLXT-SETTLE-AMT - (WS-WORK-AMT * 3)
           COMPUTE TLXM-SETTLE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT COVER AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3050-SPLIT-COVER.
           COMPUTE WS-WORK-AMT = TLXT-COVER-AMT / 3
           COMPUTE WS-WORK-AMT-2 = TLXT-COVER-AMT - (WS-WORK-AMT * 3)
           COMPUTE TLXT-SETTLE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SETTLE YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3060-ACCRUE-SETTLE.
           IF TLXM-SETTLE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLXM-SETTLE-YTD * TLXM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO TLXM-WIRE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CORRESP GRID TIER FOR THE CHARGE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3070-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CORRESP-GRID-RATE(1) TO TLXM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF TLXM-CHARGE-AMT NOT < WS-CORRESP-GRID-LOW(WS-TX)
                   MOVE WS-CORRESP-GRID-RATE(WS-TX) TO TLXM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CORRESP FEE ON THE SETTLE YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3080-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN TLXM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO TLXM-CORRESP-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLXM-STOP-FLAG = 'Y'
                   COMPUTE TLXM-CORRESP-AMT ROUNDED = TLXM-SETTLE-YTD *
                       TLXM-FEE-PCT / 200
               WHEN TLXM-REASON-CD = 'DGN'
                   MOVE 10.00 TO TLXM-CORRESP-AMT
               WHEN TLXM-REASON-CD = 'JBT'
                   MOVE 7.00 TO TLXM-CORRESP-AMT
               WHEN OTHER
                   COMPUTE TLXM-CORRESP-AMT ROUNDED = TLXM-SETTLE-YTD *
                       TLXM-FEE-PCT / 100
           END-EVALUATE
           IF TLXM-CORRESP-AMT > 0 AND TLXM-CORRESP-AMT < 2.00
               MOVE 2.00 TO TLXM-CORRESP-AMT
           END-IF
           IF TLXM-CORRESP-AMT > 100.00
               MOVE 100.00 TO TLXM-CORRESP-AMT
           END-IF
           ADD TLXM-CORRESP-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT QUEUED AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3090-SPLIT-QUEUED.
           COMPUTE WS-WORK-AMT = TLXT-QUEUED-AMT / 12
           COMPUTE WS-WORK-AMT-2 = TLXT-QUEUED-AMT - (WS-WORK-AMT * 12)
           COMPUTE TLXM-QUEUED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 10 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3100-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF TLXT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO TLXT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TEST KEY CD:
      * EA -> GA WHEN NEXT REVIEW DATE IS PAST
      * GA -> HL WHEN NEXT REVIEW DATE IS PAST
      * HL -> JC WHEN NEXT REVIEW DATE IS PAST
      * JC -> KC WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3110-SET-TEST-KEY.
           EVALUATE TLXM-TEST-KEY-CD
               WHEN 'EA'
                   IF TLXM-NEXT-REVIEW-DATE > 0 AND
                       TLXM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'GA' TO TLXM-TEST-KEY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GA'
                   IF TLXM-NEXT-REVIEW-DATE > 0 AND
                       TLXM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'HL' TO TLXM-TEST-KEY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HL'
                   IF TLXM-NEXT-REVIEW-DATE > 0 AND
                       TLXM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'JC' TO TLXM-TEST-KEY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JC'
                   IF TLXM-NEXT-REVIEW-DATE > 0 AND
                       TLXM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'KC' TO TLXM-TEST-KEY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3120-COUNT-PAYMENT.
           IF TLXM-PAYMENT-CNT < 9999999
               ADD 1 TO TLXM-PAYMENT-CNT
           END-IF
           IF TLXM-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO TLXM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * COVER AMT IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3130-LIMIT-COVER.
           IF TLXT-COVER-AMT < 50.00
               MOVE 50.00 TO TLXT-COVER-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXT-COVER-AMT > 10000.00
               MOVE 10000.00 TO TLXT-COVER-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SETTLE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3140-SPLIT-SETTLE.
           COMPUTE WS-WORK-AMT = TLXM-SETTLE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = TLXM-SETTLE-AMT - (WS-WORK-AMT * 2)
           COMPUTE TLXM-CORRESP-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SETTLE GRID TIER FOR THE COVER AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3150-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-SETTLE-GRID-RATE(1) TO TLXM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF TLXM-COVER-AMT NOT < WS-SETTLE-GRID-LOW(WS-TX)
                   MOVE WS-SETTLE-GRID-RATE(WS-TX) TO TLXM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WIRE AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3160-ACCRUE-WIRE.
           IF TLXT-WIRE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLXT-WIRE-AMT * TLXM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO TLXM-COVER-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E570 DESC REQUIRED
      * E312 COVER AMT NEGATIVE
      * E062 COVER AMT OVER 50000
      * E309 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3170-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXM-DESC = SPACES
                   MOVE 'E570' TO WS-REJECT-CODE
               WHEN TLXM-COVER-AMT < 0
                   MOVE 'E312' TO WS-REJECT-CODE
               WHEN TLXM-COVER-AMT > 50000.00
                   MOVE 'E062' TO WS-REJECT-CODE
               WHEN TLXM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E309' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR MSG TYPE CD:
      * LG -> LX WHEN LAST PAY DATE IS PAST
      * LX -> ND WHEN LAST PAY DATE IS PAST
      * ND -> PT WHEN LAST PAY DATE IS PAST
      * PT -> LG WHEN LAST PAY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3180-SET-MSG-TYPE.
           EVALUATE TLXM-MSG-TYPE-CD
               WHEN 'LG'
                   IF TLXM-LAST-PAY-DATE > 0 AND TLXM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LX' TO TLXM-MSG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LX'
                   IF TLXM-LAST-PAY-DATE > 0 AND TLXM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ND' TO TLXM-MSG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ND'
                   IF TLXM-LAST-PAY-DATE > 0 AND TLXM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PT' TO TLXM-MSG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PT'
                   IF TLXM-LAST-PAY-DATE > 0 AND TLXM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LG' TO TLXM-MSG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE SETTLE GRID TIER FOR THE QUEUED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3190-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-SETTLE-GRID-RATE(1) TO TLXM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF TLXM-QUEUED-YTD NOT < WS-SETTLE-GRID-LOW(WS-TX)
                   MOVE WS-SETTLE-GRID-RATE(WS-TX) TO TLXM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SETTLE GRID TIER FOR THE QUEUED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3200-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-SETTLE-GRID-RATE(1) TO TLXM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF TLXM-QUEUED-YTD NOT < WS-SETTLE-GRID-LOW(WS-TX)
                   MOVE WS-SETTLE-GRID-RATE(WS-TX) TO TLXM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3210-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF TLXM-NEXT-DUE-DATE > 0 AND TLXM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TLXM-QUEUED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TLXM-QUEUED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD TLXM-QUEUED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TLXM-QUEUED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLXM-QUEUED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE CHARGE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CORRESP YTD.
      *----------------------------------------------------------------*
       3220-PRORATE-CORRESP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLXM-CORRESP-YTD ROUNDED =
                   TLXM-CHARGE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TLXM-CORRESP-YTD
           END-IF
           ADD TLXM-CORRESP-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3230-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF TLXM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO TLXM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3240-PRORATE-ITEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3250-COUNT-PAYMENT.
           IF TLXM-PAYMENT-CNT < 9999999
               ADD 1 TO TLXM-PAYMENT-CNT
           END-IF
           IF TLXM-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO TLXM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * COVER YTD IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3260-LIMIT-COVER.
           IF TLXM-COVER-YTD < 25.00
               MOVE 25.00 TO TLXM-COVER-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXM-COVER-YTD > 500.00
               MOVE 500.00 TO TLXM-COVER-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * QUEUED AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3270-FLAG-QUEUED.
           IF TLXT-QUEUED-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TLXT-QUEUED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * WIRE AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3280-FLAG-WIRE.
           IF TLXT-WIRE-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TLXT-WIRE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3290-DESCRIBE-BRANCH.
           EVALUATE TLXM-BRANCH-CD
               WHEN 'CC'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'KR'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'LD'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'LS'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'XL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WIRE YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3300-FLAG-WIRE.
           IF TLXM-WIRE-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TLXM-WIRE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3310-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF TLXM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO TLXM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SETTLE AMT IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3320-LIMIT-SETTLE.
           IF TLXT-SETTLE-AMT < 25.00
               MOVE 25.00 TO TLXT-SETTLE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXT-SETTLE-AMT > 500.00
               MOVE 500.00 TO TLXT-SETTLE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CORRESP AMT IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3330-LIMIT-CORRESP.
           IF TLXM-CORRESP-AMT < 100.00
               MOVE 100.00 TO TLXM-CORRESP-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXM-CORRESP-AMT > 5000.00
               MOVE 5000.00 TO TLXM-CORRESP-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 60 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3340-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF TLXM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO TLXM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E127 ADDR REQUIRED
      * E586 SETTLE AMT NEGATIVE
      * E872 SETTLE AMT OVER 1000000
      * E669 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3350-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXM-ADDR = SPACES
                   MOVE 'E127' TO WS-REJECT-CODE
               WHEN TLXM-SETTLE-AMT < 0
                   MOVE 'E586' TO WS-REJECT-CODE
               WHEN TLXM-SETTLE-AMT > 1000000.00
                   MOVE 'E872' TO WS-REJECT-CODE
               WHEN TLXM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E669' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3360-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF TLXM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO TLXM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * IN -> OT WHEN EXPIRY DATE IS PAST
      * OT -> RJ WHEN EXPIRY DATE IS PAST
      * RJ -> RP WHEN EXPIRY DATE IS PAST
      * RP -> CN WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3370-SET-TRAN.
           EVALUATE TLXT-TRAN-CD
               WHEN 'IN'
                   IF TLXM-EXPIRY-DATE > 0 AND TLXM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'OT' TO TLXT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'OT'
                   IF TLXM-EXPIRY-DATE > 0 AND TLXM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RJ' TO TLXT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RJ'
                   IF TLXM-EXPIRY-DATE > 0 AND TLXM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RP' TO TLXT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RP'
                   IF TLXM-EXPIRY-DATE > 0 AND TLXM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CN' TO TLXT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE QUEUED GRID TIER FOR THE COVER YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3380-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-QUEUED-GRID-RATE(1) TO TLXM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF TLXM-COVER-YTD NOT < WS-QUEUED-GRID-LOW(WS-TX)
                   MOVE WS-QUEUED-GRID-RATE(WS-TX) TO TLXM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT QUEUED AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3390-SPLIT-QUEUED.
           COMPUTE WS-WORK-AMT = TLXM-QUEUED-AMT / 3
           COMPUTE WS-WORK-AMT-2 = TLXM-QUEUED-AMT - (WS-WORK-AMT * 3)
           COMPUTE TLXM-CORRESP-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN POST DATE IS PAST
      * BAT -> MAL WHEN POST DATE IS PAST
      * MAL -> TEL WHEN POST DATE IS PAST
      * TEL -> ONL WHEN POST DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3400-SET-CHANNEL.
           EVALUATE TLXT-CHANNEL-CD
               WHEN 'BRN'
                   IF TLXT-POST-DATE > 0 AND TLXT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO TLXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF TLXT-POST-DATE > 0 AND TLXT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO TLXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF TLXT-POST-DATE > 0 AND TLXT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO TLXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF TLXT-POST-DATE > 0 AND TLXT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO TLXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SETTLE YTD IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3410-LIMIT-SETTLE.
           IF TLXM-SETTLE-YTD < 50.00
               MOVE 50.00 TO TLXM-SETTLE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXM-SETTLE-YTD > 5000.00
               MOVE 5000.00 TO TLXM-SETTLE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3420-COUNT-PAYMENT.
           IF TLXM-PAYMENT-CNT < 9999999
               ADD 1 TO TLXM-PAYMENT-CNT
           END-IF
           IF TLXM-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO TLXM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3430-DESCRIBE-TRAN.
           EVALUATE TLXT-TRAN-CD
               WHEN 'IN'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'OT'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'RJ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'RP'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'CN'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE SETTLE AMT BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3440-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLXM-BRANCH-CD
                   ADD TLXM-SETTLE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLXM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLXM-SETTLE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WIRE AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3450-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLXT-CHANNEL-CD
                   ADD TLXT-WIRE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLXT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLXT-WIRE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SETTLE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3460-SPLIT-SETTLE.
           COMPUTE WS-WORK-AMT = TLXM-SETTLE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = TLXM-SETTLE-AMT - (WS-WORK-AMT * 2)
           COMPUTE TLXM-QUEUED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 15 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3470-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF TLXM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO TLXM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CHARGE AMT.
      *----------------------------------------------------------------*
       3480-PRORATE-CHARGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLXM-CHARGE-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TLXM-CHARGE-AMT
           END-IF
           ADD TLXM-CHARGE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * COVER AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3490-FLAG-COVER.
           IF TLXM-COVER-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TLXM-COVER-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3500-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF TLXM-LAST-PAY-DATE > 0 AND TLXM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TLXM-QUEUED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TLXM-QUEUED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TLXM-QUEUED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TLXM-QUEUED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLXM-QUEUED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT QUEUED AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3510-SPLIT-QUEUED.
           COMPUTE WS-WORK-AMT = TLXM-QUEUED-AMT / 3
           COMPUTE WS-WORK-AMT-2 = TLXM-QUEUED-AMT - (WS-WORK-AMT * 3)
           COMPUTE TLXM-CORRESP-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3520-SPLIT-QUEUED.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 2)
           COMPUTE TLXM-QUEUED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3530-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF TLXM-NEXT-REVIEW-DATE > 0 AND TLXM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TLXM-CORRESP-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TLXM-CORRESP-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD TLXM-CORRESP-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TLXM-CORRESP-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLXM-CORRESP-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT CORRESP YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3540-SPLIT-CORRESP.
           COMPUTE WS-WORK-AMT = TLXM-CORRESP-YTD / 12
           COMPUTE WS-WORK-AMT-2 = TLXM-CORRESP-YTD - (WS-WORK-AMT * 12)
           COMPUTE TLXM-WIRE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WIRE YTD IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3550-LIMIT-WIRE.
           IF TLXM-WIRE-YTD < 100.00
               MOVE 100.00 TO TLXM-WIRE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXM-WIRE-YTD > 5000.00
               MOVE 5000.00 TO TLXM-WIRE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WIRE YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3560-ACCRUE-WIRE.
           IF TLXM-WIRE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLXM-WIRE-YTD * TLXM-RATE / 100 / 360
               ADD WS-WORK-AMT TO TLXM-QUEUED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SETTLE GRID TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3570-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-SETTLE-GRID-RATE(1) TO TLXM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF WS-WORK-AMT NOT < WS-SETTLE-GRID-LOW(WS-TX)
                   MOVE WS-SETTLE-GRID-RATE(WS-TX) TO TLXM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CHARGE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CORRESP YTD.
      *----------------------------------------------------------------*
       3580-PRORATE-CORRESP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLXM-CORRESP-YTD ROUNDED =
                   TLXM-CHARGE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TLXM-CORRESP-YTD
           END-IF
           ADD TLXM-CORRESP-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3590-COUNT-EXCEPT.
           IF TLXM-EXCEPT-CNT < 9999999
               ADD 1 TO TLXM-EXCEPT-CNT
           END-IF
           IF TLXM-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO TLXM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE QUEUED GRID TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3600-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-QUEUED-GRID-RATE(1) TO TLXM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF WS-WORK-AMT NOT < WS-QUEUED-GRID-LOW(WS-TX)
                   MOVE WS-QUEUED-GRID-RATE(WS-TX) TO TLXM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WIRE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO COVER AMT.
      *----------------------------------------------------------------*
       3610-PRORATE-COVER.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLXT-COVER-AMT ROUNDED =
                   TLXM-WIRE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TLXT-COVER-AMT
           END-IF
           ADD TLXT-COVER-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * WIRE YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3620-FLAG-WIRE.
           IF TLXM-WIRE-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TLXM-WIRE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE COVER YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CORRESP AMT.
      *----------------------------------------------------------------*
       3630-PRORATE-CORRESP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLXM-CORRESP-AMT ROUNDED =
                   TLXM-COVER-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TLXM-CORRESP-AMT
           END-IF
           ADD TLXM-CORRESP-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SETTLE YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3640-ACCRUE-SETTLE.
           IF TLXM-SETTLE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLXM-SETTLE-YTD * TLXM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO TLXT-WIRE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CORRESP GRID TIER FOR THE COVER AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3650-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CORRESP-GRID-RATE(1) TO TLXM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF TLXM-COVER-AMT NOT < WS-CORRESP-GRID-LOW(WS-TX)
                   MOVE WS-CORRESP-GRID-RATE(WS-TX) TO TLXM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * IN -> OT WHEN LAST ACT DATE IS PAST
      * OT -> RJ WHEN LAST ACT DATE IS PAST
      * RJ -> RP WHEN LAST ACT DATE IS PAST
      * RP -> CN WHEN LAST ACT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3660-SET-TRAN.
           EVALUATE TLXT-TRAN-CD
               WHEN 'IN'
                   IF TLXM-LAST-ACT-DATE > 0 AND TLXM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'OT' TO TLXT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'OT'
                   IF TLXM-LAST-ACT-DATE > 0 AND TLXM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RJ' TO TLXT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RJ'
                   IF TLXM-LAST-ACT-DATE > 0 AND TLXM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RP' TO TLXT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RP'
                   IF TLXM-LAST-ACT-DATE > 0 AND TLXM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CN' TO TLXT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * COVER YTD IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3670-LIMIT-COVER.
           IF TLXM-COVER-YTD < 100.00
               MOVE 100.00 TO TLXM-COVER-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXM-COVER-YTD > 500.00
               MOVE 500.00 TO TLXM-COVER-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE QUEUED GRID TIER FOR THE QUEUED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3680-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-QUEUED-GRID-RATE(1) TO TLXM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF TLXM-QUEUED-YTD NOT < WS-QUEUED-GRID-LOW(WS-TX)
                   MOVE WS-QUEUED-GRID-RATE(WS-TX) TO TLXM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E106 CONTACT REQUIRED
      * E112 QUEUED AMT NEGATIVE
      * E363 QUEUED AMT OVER 1000000
      * E154 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3690-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXM-CONTACT = SPACES
                   MOVE 'E106' TO WS-REJECT-CODE
               WHEN TLXT-QUEUED-AMT < 0
                   MOVE 'E112' TO WS-REJECT-CODE
               WHEN TLXT-QUEUED-AMT > 1000000.00
                   MOVE 'E363' TO WS-REJECT-CODE
               WHEN TLXM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E154' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE COVER YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CORRESP AMT.
      *----------------------------------------------------------------*
       3700-PRORATE-CORRESP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLXM-CORRESP-AMT ROUNDED =
                   TLXM-COVER-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TLXM-CORRESP-AMT
           END-IF
           ADD TLXM-CORRESP-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE SETTLE GRID TIER FOR THE CHARGE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3710-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-SETTLE-GRID-RATE(1) TO TLXM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF TLXM-CHARGE-AMT NOT < WS-SETTLE-GRID-LOW(WS-TX)
                   MOVE WS-SETTLE-GRID-RATE(WS-TX) TO TLXM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CORRESP GRID TIER FOR THE SETTLE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3720-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CORRESP-GRID-RATE(1) TO TLXM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF TLXM-SETTLE-YTD NOT < WS-CORRESP-GRID-LOW(WS-TX)
                   MOVE WS-CORRESP-GRID-RATE(WS-TX) TO TLXM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3730-COUNT-ITEM.
           IF TLXM-ITEM-CNT < 9999999
               ADD 1 TO TLXM-ITEM-CNT
           END-IF
           IF TLXM-ITEM-CNT NOT < 10
               MOVE 'Y' TO TLXM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3740-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF TLXT-POST-DATE > 0 AND TLXT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TLXM-CHARGE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD TLXM-CHARGE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TLXM-CHARGE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD TLXM-CHARGE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLXM-CHARGE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 180 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3750-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF TLXT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO TLXM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3760-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF TLXM-OPEN-DATE > 0 AND TLXM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TLXM-SETTLE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TLXM-SETTLE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TLXM-SETTLE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TLXM-SETTLE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLXM-SETTLE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3770-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF TLXM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO TLXM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3780-COUNT-SEQ.
           IF TLXT-SEQ < 9999999
               ADD 1 TO TLXT-SEQ
           END-IF
           IF TLXT-SEQ NOT < 3
               MOVE 'Y' TO TLXM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN POST DATE IS PAST
      * BAT -> MAL WHEN POST DATE IS PAST
      * MAL -> TEL WHEN POST DATE IS PAST
      * TEL -> ONL WHEN POST DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3790-SET-CHANNEL.
           EVALUATE TLXT-CHANNEL-CD
               WHEN 'BRN'
                   IF TLXT-POST-DATE > 0 AND TLXT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO TLXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF TLXT-POST-DATE > 0 AND TLXT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO TLXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF TLXT-POST-DATE > 0 AND TLXT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO TLXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF TLXT-POST-DATE > 0 AND TLXT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO TLXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 10 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3800-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF TLXM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO TLXM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SETTLE FEE ON THE WIRE AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3810-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN TLXM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO TLXM-SETTLE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLXM-SUSPEND-FLAG = 'Y'
                   COMPUTE TLXM-SETTLE-AMT ROUNDED = TLXM-WIRE-AMT *
                       TLXM-FEE-PCT / 200
               WHEN TLXT-TRAN-CD = 'IN'
                   MOVE 3.00 TO TLXM-SETTLE-AMT
               WHEN TLXT-TRAN-CD = 'OT'
                   MOVE 5.00 TO TLXM-SETTLE-AMT
               WHEN OTHER
                   COMPUTE TLXM-SETTLE-AMT ROUNDED = TLXM-WIRE-AMT *
                       TLXM-FEE-PCT / 100
           END-EVALUATE
           IF TLXM-SETTLE-AMT > 0 AND TLXM-SETTLE-AMT < 10.00
               MOVE 10.00 TO TLXM-SETTLE-AMT
           END-IF
           IF TLXM-SETTLE-AMT > 150.00
               MOVE 150.00 TO TLXM-SETTLE-AMT
           END-IF
           ADD TLXM-SETTLE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SETTLE YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3820-ACCRUE-SETTLE.
           IF TLXM-SETTLE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLXM-SETTLE-YTD * TLXM-RATE / 100 / 360
               ADD WS-WORK-AMT TO TLXM-SETTLE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * QUEUED YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3830-FLAG-QUEUED.
           IF TLXM-QUEUED-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TLXM-QUEUED-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE COVER AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CORRESP YTD.
      *----------------------------------------------------------------*
       3840-PRORATE-CORRESP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLXM-CORRESP-YTD ROUNDED =
                   TLXT-COVER-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TLXM-CORRESP-YTD
           END-IF
           ADD TLXM-CORRESP-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * WIRE YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3850-FLAG-WIRE.
           IF TLXM-WIRE-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TLXM-WIRE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SETTLE AMT IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3860-LIMIT-SETTLE.
           IF TLXM-SETTLE-AMT < 100.00
               MOVE 100.00 TO TLXM-SETTLE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXM-SETTLE-AMT > 2500.00
               MOVE 2500.00 TO TLXM-SETTLE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3870-COUNT-EXCEPT.
           IF TLXM-EXCEPT-CNT < 9999999
               ADD 1 TO TLXM-EXCEPT-CNT
           END-IF
           IF TLXM-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO TLXT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CHARGE AMT IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3880-LIMIT-CHARGE.
           IF TLXM-CHARGE-AMT < 10.00
               MOVE 10.00 TO TLXM-CHARGE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXM-CHARGE-AMT > 2500.00
               MOVE 2500.00 TO TLXM-CHARGE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE SETTLE YTD BY TEST KEY CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3890-ACCUM-TEST-KEY.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLXM-TEST-KEY-CD
                   ADD TLXM-SETTLE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLXM-TEST-KEY-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLXM-SETTLE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3900-LIMIT-ITEM.
           IF WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 1000.00
               MOVE 1000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WIRE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3910-SPLIT-WIRE.
           COMPUTE WS-WORK-AMT = TLXM-WIRE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = TLXM-WIRE-AMT - (WS-WORK-AMT * 3)
           COMPUTE TLXM-CHARGE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3920-COUNT-PAYMENT.
           IF TLXM-PAYMENT-CNT < 9999999
               ADD 1 TO TLXM-PAYMENT-CNT
           END-IF
           IF TLXM-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO TLXM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN LAST ACT DATE IS PAST
      * BAT -> MAL WHEN LAST ACT DATE IS PAST
      * MAL -> TEL WHEN LAST ACT DATE IS PAST
      * TEL -> ONL WHEN LAST ACT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3930-SET-CHANNEL.
           EVALUATE TLXT-CHANNEL-CD
               WHEN 'BRN'
                   IF TLXM-LAST-ACT-DATE > 0 AND TLXM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO TLXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF TLXM-LAST-ACT-DATE > 0 AND TLXM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO TLXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF TLXM-LAST-ACT-DATE > 0 AND TLXM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO TLXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF TLXM-LAST-ACT-DATE > 0 AND TLXM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO TLXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE COVER YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO QUEUED AMT.
      *----------------------------------------------------------------*
       3940-PRORATE-QUEUED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLXM-QUEUED-AMT ROUNDED =
                   TLXM-COVER-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TLXM-QUEUED-AMT
           END-IF
           ADD TLXM-QUEUED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3950-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF TLXM-NEXT-DUE-DATE > 0 AND TLXM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TLXM-COVER-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TLXM-COVER-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD TLXM-COVER-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TLXM-COVER-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLXM-COVER-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E676 OPERATOR REQUIRED
      * E546 WIRE AMT NEGATIVE
      * E728 WIRE AMT OVER 99999
      * E467 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3960-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXT-OPERATOR = SPACES
                   MOVE 'E676' TO WS-REJECT-CODE
               WHEN TLXM-WIRE-AMT < 0
                   MOVE 'E546' TO WS-REJECT-CODE
               WHEN TLXM-WIRE-AMT > 99999.00
                   MOVE 'E728' TO WS-REJECT-CODE
               WHEN TLXM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E467' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE SETTLE YTD BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3970-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLXT-TRAN-CD
                   ADD TLXM-SETTLE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLXT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLXM-SETTLE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE QUEUED GRID TIER FOR THE QUEUED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3980-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-QUEUED-GRID-RATE(1) TO TLXM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF TLXT-QUEUED-AMT NOT < WS-QUEUED-GRID-LOW(WS-TX)
                   MOVE WS-QUEUED-GRID-RATE(WS-TX) TO TLXM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3990-DESCRIBE-TRAN.
           EVALUATE TLXT-TRAN-CD
               WHEN 'IN'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'OT'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'RJ'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'RP'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'CN'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WIRE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CORRESP YTD.
      *----------------------------------------------------------------*
       4000-PRORATE-CORRESP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLXM-CORRESP-YTD ROUNDED =
                   TLXM-WIRE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TLXM-CORRESP-YTD
           END-IF
           ADD TLXM-CORRESP-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE CORRESP AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4010-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLXT-TRAN-CD
                   ADD TLXM-CORRESP-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLXT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLXM-CORRESP-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4020-DESCRIBE-BRANCH.
           EVALUATE TLXM-BRANCH-CD
               WHEN 'CC'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'KR'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'LD'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'LS'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'XL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       4030-COUNT-ITEM.
           IF TLXM-ITEM-CNT < 9999999
               ADD 1 TO TLXM-ITEM-CNT
           END-IF
           IF TLXM-ITEM-CNT NOT < 6
               MOVE 'Y' TO TLXM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CORRESP YTD IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4040-LIMIT-CORRESP.
           IF TLXM-CORRESP-YTD < 25.00
               MOVE 25.00 TO TLXM-CORRESP-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXM-CORRESP-YTD > 2500.00
               MOVE 2500.00 TO TLXM-CORRESP-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 15 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4050-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF TLXM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO TLXM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CORRESP AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4060-SPLIT-CORRESP.
           COMPUTE WS-WORK-AMT = TLXM-CORRESP-AMT / 3
           COMPUTE WS-WORK-AMT-2 = TLXM-CORRESP-AMT - (WS-WORK-AMT * 3)
           COMPUTE TLXM-WIRE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4070-DESCRIBE-REASON.
           EVALUATE TLXM-REASON-CD
               WHEN 'DGN'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'JBT'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'LSS'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'RDP'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'RPL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'SNE'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'UJS'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE COVER YTD BY TEST KEY CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4080-ACCUM-TEST-KEY.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLXM-TEST-KEY-CD
                   ADD TLXM-COVER-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLXM-TEST-KEY-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLXM-COVER-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR MSG TYPE CD:
      * LG -> LX WHEN NEXT DUE DATE IS PAST
      * LX -> ND WHEN NEXT DUE DATE IS PAST
      * ND -> PT WHEN NEXT DUE DATE IS PAST
      * PT -> LG WHEN NEXT DUE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4090-SET-MSG-TYPE.
           EVALUATE TLXM-MSG-TYPE-CD
               WHEN 'LG'
                   IF TLXM-NEXT-DUE-DATE > 0 AND TLXM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LX' TO TLXM-MSG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LX'
                   IF TLXM-NEXT-DUE-DATE > 0 AND TLXM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ND' TO TLXM-MSG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ND'
                   IF TLXM-NEXT-DUE-DATE > 0 AND TLXM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PT' TO TLXM-MSG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PT'
                   IF TLXM-NEXT-DUE-DATE > 0 AND TLXM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LG' TO TLXM-MSG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4100-DESCRIBE-REASON.
           EVALUATE TLXM-REASON-CD
               WHEN 'DGN'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'JBT'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'LSS'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'RDP'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'RPL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'SNE'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'UJS'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CHARGE YTD IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4110-LIMIT-CHARGE.
           IF TLXM-CHARGE-YTD < 50.00
               MOVE 50.00 TO TLXM-CHARGE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXM-CHARGE-YTD > 50000.00
               MOVE 50000.00 TO TLXM-CHARGE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SETTLE GRID TIER FOR THE QUEUED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4120-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-SETTLE-GRID-RATE(1) TO TLXM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF TLXM-QUEUED-AMT NOT < WS-SETTLE-GRID-LOW(WS-TX)
                   MOVE WS-SETTLE-GRID-RATE(WS-TX) TO TLXM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE COVER AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4130-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLXM-REGION-CD
                   ADD TLXM-COVER-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLXM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLXM-COVER-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WIRE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO COVER AMT.
      *----------------------------------------------------------------*
       4140-PRORATE-COVER.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLXT-COVER-AMT ROUNDED =
                   TLXM-WIRE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TLXT-COVER-AMT
           END-IF
           ADD TLXT-COVER-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PASS THE TLX FIELDS TO TXPRC AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7020-CALL-TXPRC.
           INITIALIZE TXPRC-PARMS
           MOVE TLXM-COVER-YTD
             TO TXPRC-COVER-YTD
           MOVE TLXM-QUEUED-AMT
             TO TXPRC-QUEUED-AMT
           MOVE TLXM-CORRESP-YTD
             TO TXPRC-CORRESP-YTD
           MOVE TLXM-SETTLE-YTD
             TO TXPRC-SETTLE-YTD
           MOVE TLXM-QUEUED-YTD
             TO TXPRC-QUEUED-YTD
           MOVE TLXM-SETTLE-AMT
             TO TXPRC-SETTLE-AMT
           MOVE TLXM-OPEN-DATE
             TO TXPRC-OPEN-DATE
           MOVE TLXM-NEXT-DUE-DATE
             TO TXPRC-NEXT-DUE-DATE
           MOVE TLXM-NOTICE-DATE
             TO TXPRC-NOTICE-DATE
           MOVE TLXM-EXPIRY-DATE
             TO TXPRC-EXPIRY-DATE
           MOVE TLXM-TEST-KEY-CD
             TO TXPRC-TEST-KEY-CD
           MOVE TLXM-REGION-CD
             TO TXPRC-REGION-CD
           MOVE TLXM-BRANCH-CD
             TO TXPRC-BRANCH-CD
           MOVE TLXM-REASON-CD
             TO TXPRC-REASON-CD
           MOVE TLXM-NOTICE-SENT-FLAG
             TO TXPRC-NOTICE-SENT-FLAG
           MOVE TLXM-STOP-FLAG
             TO TXPRC-STOP-FLAG
           MOVE TLXM-REVIEW-FLAG
             TO TXPRC-REVIEW-FLAG
           MOVE TLXM-PAYMENT-CNT
             TO TXPRC-PAYMENT-CNT
           MOVE TLXM-EXCEPT-CNT
             TO TXPRC-EXCEPT-CNT
           MOVE TLXM-RATE
             TO TXPRC-RATE
           MOVE TLXM-DISCOUNT
             TO TXPRC-DISCOUNT
           MOVE TLXM-ADDR
             TO TXPRC-ADDR
           MOVE TLXM-NAME
             TO TXPRC-NAME
           CALL 'TXPRC' USING TXPRC-PARMS
           EVALUATE TXPRC-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE TXPRC-CORRESP-YTD
                     TO TLXM-CORRESP-YTD
                   MOVE TXPRC-SETTLE-YTD
                     TO TLXM-SETTLE-YTD
                   MOVE TXPRC-SETTLE-AMT
                     TO TLXM-SETTLE-AMT
                   CONTINUE
               WHEN 08
                   MOVE 'RPRC' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-TXPRC' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE TLX FIELDS TO TXELG AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7000-CALL-TXELG.
           INITIALIZE TXELG-PARMS
           MOVE TLXM-WIRE-YTD
             TO TXELG-WIRE-YTD
           MOVE TLXM-CHARGE-AMT
             TO TXELG-CHARGE-AMT
           MOVE TLXM-QUEUED-AMT
             TO TXELG-QUEUED-AMT
           MOVE TLXM-CORRESP-YTD
             TO TXELG-CORRESP-YTD
           MOVE TLXM-CORRESP-AMT
             TO TXELG-CORRESP-AMT
           MOVE TLXM-WIRE-AMT
             TO TXELG-WIRE-AMT
           MOVE TLXM-LAST-PAY-DATE
             TO TXELG-LAST-PAY-DATE
           MOVE TLXM-NEXT-DUE-DATE
             TO TXELG-NEXT-DUE-DATE
           MOVE TLXM-EFFECTIVE-DATE
             TO TXELG-EFFECTIVE-DATE
           MOVE TLXM-CLOSE-DATE
             TO TXELG-CLOSE-DATE
           MOVE TLXM-MSG-TYPE-CD
             TO TXELG-MSG-TYPE-CD
           MOVE TLXM-REGION-CD
             TO TXELG-REGION-CD
           MOVE TLXM-TYPE-CD
             TO TXELG-TYPE-CD
           MOVE TLXM-REASON-CD
             TO TXELG-REASON-CD
           MOVE TLXM-SUSPEND-FLAG
             TO TXELG-SUSPEND-FLAG
           MOVE TLXM-STOP-FLAG
             TO TXELG-STOP-FLAG
           MOVE TLXM-OVERRIDE-FLAG
             TO TXELG-OVERRIDE-FLAG
           MOVE TLXM-PAYMENT-CNT
             TO TXELG-PAYMENT-CNT
           MOVE TLXM-NOTICE-CNT
             TO TXELG-NOTICE-CNT
           MOVE TLXM-FEE-PCT
             TO TXELG-FEE-PCT
           MOVE TLXM-RATE
             TO TXELG-RATE
           MOVE TLXM-ADDR
             TO TXELG-ADDR
           MOVE TLXM-DESC
             TO TXELG-DESC
           CALL 'TXELG' USING TXELG-PARMS
           EVALUATE TXELG-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE TXELG-WIRE-YTD
                     TO TLXM-WIRE-YTD
                   MOVE TXELG-CHARGE-AMT
                     TO TLXM-CHARGE-AMT
                   MOVE TXELG-QUEUED-AMT
                     TO TLXM-QUEUED-AMT
                   MOVE TXELG-STOP-FLAG
                     TO TLXM-STOP-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RELG' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-TXELG' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE TLX FIELDS TO TXCAL AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7030-CALL-TXCAL.
           INITIALIZE TXCAL-PARMS
           MOVE TLXM-CHARGE-AMT
             TO TXCAL-CHARGE-AMT
           MOVE TLXM-SETTLE-AMT
             TO TXCAL-SETTLE-AMT
           MOVE TLXM-QUEUED-YTD
             TO TXCAL-QUEUED-YTD
           MOVE TLXM-CHARGE-YTD
             TO TXCAL-CHARGE-YTD
           MOVE TLXM-COVER-YTD
             TO TXCAL-COVER-YTD
           MOVE TLXM-QUEUED-AMT
             TO TXCAL-QUEUED-AMT
           MOVE TLXM-LAST-ACT-DATE
             TO TXCAL-LAST-ACT-DATE
           MOVE TLXM-NEXT-DUE-DATE
             TO TXCAL-NEXT-DUE-DATE
           MOVE TLXM-EXPIRY-DATE
             TO TXCAL-EXPIRY-DATE
           MOVE TLXM-NEXT-REVIEW-DATE
             TO TXCAL-NEXT-REVIEW-DATE
           MOVE TLXM-REASON-CD
             TO TXCAL-REASON-CD
           MOVE TLXM-BRANCH-CD
             TO TXCAL-BRANCH-CD
           MOVE TLXM-TYPE-CD
             TO TXCAL-TYPE-CD
           MOVE TLXM-MSG-TYPE-CD
             TO TXCAL-MSG-TYPE-CD
           MOVE TLXM-PREFERRED-FLAG
             TO TXCAL-PREFERRED-FLAG
           MOVE TLXM-REVIEW-FLAG
             TO TXCAL-REVIEW-FLAG
           MOVE TLXM-SUSPEND-FLAG
             TO TXCAL-SUSPEND-FLAG
           MOVE TLXM-PAYMENT-CNT
             TO TXCAL-PAYMENT-CNT
           MOVE TLXM-EXCEPT-CNT
             TO TXCAL-EXCEPT-CNT
           MOVE TLXM-FEE-PCT
             TO TXCAL-FEE-PCT
           MOVE TLXM-RATE
             TO TXCAL-RATE
           MOVE TLXM-DESC
             TO TXCAL-DESC
           MOVE TLXM-NAME
             TO TXCAL-NAME
           CALL 'TXCAL' USING TXCAL-PARMS
           EVALUATE TXCAL-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE TXCAL-CHARGE-AMT
                     TO TLXM-CHARGE-AMT
                   MOVE TXCAL-SETTLE-AMT
                     TO TLXM-SETTLE-AMT
                   MOVE TXCAL-CHARGE-YTD
                     TO TLXM-CHARGE-YTD
                   MOVE TXCAL-PREFERRED-FLAG
                     TO TLXM-PREFERRED-FLAG
                   MOVE TXCAL-SUSPEND-FLAG
                     TO TLXM-SUSPEND-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RCAL' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7030-CALL-TXCAL' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE TLXT-KEY TO RD-KEY
           MOVE TLXT-TRAN-CD TO RD-CODE
           MOVE TLXT-COVER-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE TLXT-KEY TO RD-KEY
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
           DISPLAY 'TXXRNEW READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'TXXRNEW' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
