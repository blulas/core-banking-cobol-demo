      *================================================================*
      * PROGRAM  : TXXPURG                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : TELEX WIRE MESSAGING - PURGE AND ARCHIVE. READS THE *
      *            TLX TRANSACTIONS, UPDATES THE TLX MASTER AND        *
      *            PRODUCES THE CONTROL REPORT. TESTED TELEX FUNDS     *
      *            TRANSFER MESSAGES AND TEST KEY VALIDATION           *
      * INPUT    : TLXMST (KSDS), TLXTRN, BANKCTL                      *
      * OUTPUT   : TLXOUT, TXXPURPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, TXCAL, TXFEE            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1993-06-19 RJT  ORIGINAL                                       *
      * 1997-08-19 JLM  WIRE LIMIT REVISED REQ4831                     *
      * 1998-09-11 BHS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2005-03-23 PQR  CHARGE CUTOFF MOVED REQ7183                    *
      * 2009-11-21 EWS  SETTLE CALCULATION CORRECTED INC6486           *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TXXPURG.
       AUTHOR. R J TRAVERS.

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
           SELECT REPORT-FILE ASSIGN TO "TXXPURPT"
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
      * WS-SETTLE-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-SETTLE-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000825000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000558500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000350000000097000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000850000000236000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001350000000894000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001450000000252500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001500000000281000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006500000000538500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006550000000788500'.
       01  WS-SETTLE-GRID-TBL REDEFINES WS-SETTLE-GRID-VALUES.
           05  WS-SETTLE-GRID-ROW OCCURS 9 TIMES.
               10  WS-SETTLE-GRID-LOW      PIC 9(09)V99.
               10  WS-SETTLE-GRID-RATE     PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-CHARGE-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CHARGE-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000606500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000857500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010500000000252000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010550000000706000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011550000000344500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012050000000871000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014550000000689000'.
       01  WS-CHARGE-BAND-TBL REDEFINES WS-CHARGE-BAND-VALUES.
           05  WS-CHARGE-BAND-ROW OCCURS 7 TIMES.
               10  WS-CHARGE-BAND-LOW      PIC 9(09)V99.
               10  WS-CHARGE-BAND-RATE     PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  TXXPURG  TELEX WIRE MESSAGING'.
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
       COPY TXCALP.
       COPY TXFEEP.

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
               PERFORM 3000-DESCRIBE-TRAN
               PERFORM 3010-ACCRUE-QUEUED
               PERFORM 3020-SPLIT-CHARGE
               PERFORM 3030-ASSESS-SUSPEND
               PERFORM 3040-SELECT-DISCOUNT
               PERFORM 3050-SET-BRANCH
               PERFORM 3060-ASSESS-OVERRIDE
               PERFORM 3070-PRORATE-WIRE
               PERFORM 3080-ACCRUE-CHARGE
               PERFORM 3090-ASSESS-PREFERRED
               PERFORM 3100-SET-TRAN
               PERFORM 3110-SPLIT-QUEUED
               PERFORM 3120-SELECT-DISCOUNT
               PERFORM 3130-AGE-LAST-ACT
               PERFORM 3140-SPLIT-CORRESP
               PERFORM 3150-ASSESS-REVIEW
               PERFORM 3160-ACCRUE-WIRE
               PERFORM 3170-ACCRUE-WIRE
               PERFORM 3180-AGE-EFFECTIVE
               PERFORM 3190-FLAG-SETTLE
               PERFORM 3200-SELECT-RATE
               PERFORM 3210-ACCUM-REGION
               PERFORM 3220-ACCUM-TRAN
               PERFORM 3230-COUNT-EXCEPT
               PERFORM 3240-CHECK-LAST-ACT
               PERFORM 3250-ACCUM-TYPE
               PERFORM 3260-DESCRIBE-TYPE
               PERFORM 3270-EDIT-CUST-ID
               PERFORM 3280-COUNT-ITEM
               PERFORM 3290-SELECT-RATE
               PERFORM 3300-COUNT-PAYMENT
               PERFORM 3310-CHECK-NEXT-DUE
               PERFORM 3320-ACCRUE-SETTLE
               PERFORM 3330-SELECT-FEE-PCT
               PERFORM 3340-CHECK-OPEN
               PERFORM 3350-SET-BRANCH
               PERFORM 3360-ASSESS-NOTICE-SENT
               PERFORM 3370-DESCRIBE-BRANCH
               PERFORM 3380-EDIT-ADDR
               PERFORM 3390-DESCRIBE-TYPE
               PERFORM 3400-SET-BRANCH
               PERFORM 3410-DESCRIBE-MSG-TYPE
               PERFORM 3420-SET-BRANCH
               PERFORM 3430-FLAG-ITEM
               PERFORM 3440-CHECK-LAST-ACT
               PERFORM 3450-CHECK-EFFECTIVE
               PERFORM 3460-FLAG-SETTLE
               PERFORM 3470-DESCRIBE-BRANCH
               PERFORM 3480-EDIT-REJECT
               PERFORM 3490-AGE-EFF
               PERFORM 3500-SET-BRANCH
               PERFORM 3510-PRORATE-CORRESP
               PERFORM 3520-AGE-LAST-PAY
               PERFORM 3530-FLAG-CORRESP
               PERFORM 3540-AGE-NEXT-DUE
               PERFORM 3550-FLAG-CHARGE
               PERFORM 3560-AGE-EFFECTIVE
               PERFORM 3570-SPLIT-CHARGE
               PERFORM 3580-AGE-OPEN
               PERFORM 3590-COUNT-SEQ
               PERFORM 3600-COUNT-ITEM
               PERFORM 3610-DESCRIBE-BRANCH
               PERFORM 3620-SELECT-RATE
               PERFORM 3630-SPLIT-COVER
               PERFORM 3640-EDIT-DESC
               PERFORM 3650-AGE-NEXT-DUE
               PERFORM 3660-AGE-NOTICE
               PERFORM 3670-PRORATE-WIRE
               PERFORM 3680-ASSESS-SUSPEND
               PERFORM 3690-ASSESS-NOTICE-SENT
               PERFORM 3700-DESCRIBE-REGION
               PERFORM 3710-SPLIT-QUEUED
               PERFORM 3720-CHECK-NOTICE
               PERFORM 3730-EDIT-DESC
               PERFORM 3740-FLAG-CHARGE
               PERFORM 3750-SPLIT-QUEUED
               PERFORM 3760-SET-REGION
               PERFORM 3770-COUNT-EXCEPT
               PERFORM 3780-LIMIT-WIRE
               PERFORM 3790-SET-CHANNEL
               PERFORM 3800-ACCRUE-WIRE
               PERFORM 3810-ASSESS-REVERSAL
               PERFORM 3820-CHECK-LAST-ACT
               PERFORM 3830-DESCRIBE-REASON
               PERFORM 3840-COUNT-ITEM
               PERFORM 3850-ACCRUE-SETTLE
               PERFORM 3860-ACCUM-BRANCH
               PERFORM 3870-FLAG-ITEM
               PERFORM 3880-ACCRUE-SETTLE
               PERFORM 3890-ACCRUE-COVER
               PERFORM 3900-FLAG-CHARGE
               PERFORM 3910-LIMIT-SETTLE
               PERFORM 3920-SET-MSG-TYPE
               PERFORM 3930-FLAG-COVER
               PERFORM 3940-EDIT-CUST-ID
               PERFORM 3950-ACCUM-TYPE
               PERFORM 3960-FLAG-QUEUED
               PERFORM 3970-CHECK-LAST-PAY
               PERFORM 3980-DESCRIBE-BRANCH
               PERFORM 3990-LIMIT-WIRE
               PERFORM 4000-ACCRUE-CORRESP
               PERFORM 4010-ACCRUE-SETTLE
               PERFORM 4020-COUNT-ITEM
               PERFORM 4030-SELECT-DISCOUNT
               PERFORM 4040-AGE-EFFECTIVE
               PERFORM 4050-EDIT-OPERATOR
               PERFORM 4060-PRORATE-CHARGE
               PERFORM 4070-LIMIT-WIRE
               PERFORM 4080-LIMIT-QUEUED
               PERFORM 4090-COUNT-EXCEPT
               PERFORM 4100-SPLIT-QUEUED
               PERFORM 4110-AGE-OPEN
               PERFORM 4120-ACCRUE-CHARGE
               PERFORM 4130-PRORATE-COVER
               PERFORM 4140-SELECT-DISCOUNT
               PERFORM 4150-LIMIT-WIRE
               PERFORM 4160-EDIT-DESC
               PERFORM 4170-ASSESS-STOP
               PERFORM 4180-PRORATE-SETTLE
               PERFORM 4190-SET-TEST-KEY
               PERFORM 4200-FLAG-SETTLE
               PERFORM 4210-ACCRUE-CHARGE
               PERFORM 4220-FLAG-WIRE
               PERFORM 4230-FLAG-WIRE
               PERFORM 4240-ASSESS-OVERRIDE
               PERFORM 4250-ASSESS-NOTICE-SENT
               PERFORM 4260-SPLIT-CORRESP
               PERFORM 4270-LIMIT-SETTLE
               PERFORM 7030-CALL-TXCAL
               PERFORM 7010-CALL-TXFEE
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
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3000-DESCRIBE-TRAN.
           EVALUATE TLXT-TRAN-CD
               WHEN 'IN'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'OT'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'RJ'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'RP'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'CN'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON QUEUED YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3010-ACCRUE-QUEUED.
           IF TLXM-QUEUED-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLXM-QUEUED-YTD * TLXM-RATE / 100 / 360
               ADD WS-WORK-AMT TO TLXM-CORRESP-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CHARGE YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3020-SPLIT-CHARGE.
           COMPUTE WS-WORK-AMT = TLXM-CHARGE-YTD / 4
           COMPUTE WS-WORK-AMT-2 = TLXM-CHARGE-YTD - (WS-WORK-AMT * 4)
           COMPUTE TLXM-COVER-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE QUEUED YTD FEE ON THE WORK AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3030-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN TLXM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO TLXM-QUEUED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLXM-STOP-FLAG = 'Y'
                   COMPUTE TLXM-QUEUED-YTD ROUNDED = WS-WORK-AMT *
                       TLXM-FEE-PCT / 200
               WHEN TLXM-TEST-KEY-CD = 'EA'
                   MOVE 5.00 TO TLXM-QUEUED-YTD
               WHEN TLXM-TEST-KEY-CD = 'GA'
                   MOVE 10.00 TO TLXM-QUEUED-YTD
               WHEN OTHER
                   COMPUTE TLXM-QUEUED-YTD ROUNDED = WS-WORK-AMT *
                       TLXM-FEE-PCT / 100
           END-EVALUATE
           IF TLXM-QUEUED-YTD > 0 AND TLXM-QUEUED-YTD < 2.00
               MOVE 2.00 TO TLXM-QUEUED-YTD
           END-IF
           IF TLXM-QUEUED-YTD > 50.00
               MOVE 50.00 TO TLXM-QUEUED-YTD
           END-IF
           ADD TLXM-QUEUED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE CHARGE BAND TIER FOR THE WIRE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3040-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CHARGE-BAND-RATE(1) TO TLXM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF TLXT-WIRE-AMT NOT < WS-CHARGE-BAND-LOW(WS-TX)
                   MOVE WS-CHARGE-BAND-RATE(WS-TX) TO TLXM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * CC -> KR WHEN NEXT REVIEW DATE IS PAST
      * KR -> LD WHEN NEXT REVIEW DATE IS PAST
      * LD -> LS WHEN NEXT REVIEW DATE IS PAST
      * LS -> XL WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3050-SET-BRANCH.
           EVALUATE TLXM-BRANCH-CD
               WHEN 'CC'
                   IF TLXM-NEXT-REVIEW-DATE > 0 AND
                       TLXM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'KR' TO TLXM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KR'
                   IF TLXM-NEXT-REVIEW-DATE > 0 AND
                       TLXM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'LD' TO TLXM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LD'
                   IF TLXM-NEXT-REVIEW-DATE > 0 AND
                       TLXM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'LS' TO TLXM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LS'
                   IF TLXM-NEXT-REVIEW-DATE > 0 AND
                       TLXM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'XL' TO TLXM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WIRE YTD FEE ON THE SETTLE AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3060-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN TLXM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO TLXM-WIRE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLXM-STOP-FLAG = 'Y'
                   COMPUTE TLXM-WIRE-YTD ROUNDED = TLXT-SETTLE-AMT *
                       TLXM-RATE / 200
               WHEN TLXT-CHANNEL-CD = 'BRN'
                   MOVE 7.00 TO TLXM-WIRE-YTD
               WHEN TLXT-CHANNEL-CD = 'BAT'
                   MOVE 3.00 TO TLXM-WIRE-YTD
               WHEN OTHER
                   COMPUTE TLXM-WIRE-YTD ROUNDED = TLXT-SETTLE-AMT *
                       TLXM-RATE / 100
           END-EVALUATE
           IF TLXM-WIRE-YTD > 0 AND TLXM-WIRE-YTD < 10.00
               MOVE 10.00 TO TLXM-WIRE-YTD
           END-IF
           IF TLXM-WIRE-YTD > 50.00
               MOVE 50.00 TO TLXM-WIRE-YTD
           END-IF
           ADD TLXM-WIRE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE SETTLE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WIRE YTD.
      *----------------------------------------------------------------*
       3070-PRORATE-WIRE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLXM-WIRE-YTD ROUNDED =
                   TLXM-SETTLE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TLXM-WIRE-YTD
           END-IF
           ADD TLXM-WIRE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CHARGE AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3080-ACCRUE-CHARGE.
           IF TLXM-CHARGE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLXM-CHARGE-AMT * TLXM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WIRE FEE ON THE SETTLE AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3090-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN TLXM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO TLXT-WIRE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLXM-OVERRIDE-FLAG = 'Y'
                   COMPUTE TLXT-WIRE-AMT ROUNDED = TLXM-SETTLE-AMT *
                       TLXM-FEE-PCT / 200
               WHEN TLXM-TEST-KEY-CD = 'EA'
                   MOVE 12.00 TO TLXT-WIRE-AMT
               WHEN TLXM-TEST-KEY-CD = 'GA'
                   MOVE 3.00 TO TLXT-WIRE-AMT
               WHEN OTHER
                   COMPUTE TLXT-WIRE-AMT ROUNDED = TLXM-SETTLE-AMT *
                       TLXM-FEE-PCT / 100
           END-EVALUATE
           IF TLXT-WIRE-AMT > 0 AND TLXT-WIRE-AMT < 1.00
               MOVE 1.00 TO TLXT-WIRE-AMT
           END-IF
           IF TLXT-WIRE-AMT > 75.00
               MOVE 75.00 TO TLXT-WIRE-AMT
           END-IF
           ADD TLXT-WIRE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * IN -> OT WHEN OPEN DATE IS PAST
      * OT -> RJ WHEN OPEN DATE IS PAST
      * RJ -> RP WHEN OPEN DATE IS PAST
      * RP -> CN WHEN OPEN DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3100-SET-TRAN.
           EVALUATE TLXT-TRAN-CD
               WHEN 'IN'
                   IF TLXM-OPEN-DATE > 0 AND TLXM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'OT' TO TLXT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'OT'
                   IF TLXM-OPEN-DATE > 0 AND TLXM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RJ' TO TLXT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RJ'
                   IF TLXM-OPEN-DATE > 0 AND TLXM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RP' TO TLXT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RP'
                   IF TLXM-OPEN-DATE > 0 AND TLXM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CN' TO TLXT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT QUEUED YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3110-SPLIT-QUEUED.
           COMPUTE WS-WORK-AMT = TLXM-QUEUED-YTD / 2
           COMPUTE WS-WORK-AMT-2 = TLXM-QUEUED-YTD - (WS-WORK-AMT * 2)
           COMPUTE TLXM-WIRE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SETTLE GRID TIER FOR THE COVER AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3120-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-SETTLE-GRID-RATE(1) TO TLXM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
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
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3130-AGE-LAST-ACT.
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
                   ADD TLXM-COVER-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD TLXM-COVER-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD TLXM-COVER-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TLXM-COVER-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLXM-COVER-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT CORRESP AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3140-SPLIT-CORRESP.
           COMPUTE WS-WORK-AMT = TLXM-CORRESP-AMT / 12
           COMPUTE WS-WORK-AMT-2 = TLXM-CORRESP-AMT - (WS-WORK-AMT * 12)
           COMPUTE TLXM-WIRE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SETTLE FEE ON THE COVER YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3150-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN TLXM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO TLXT-SETTLE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLXM-PREFERRED-FLAG = 'Y'
                   COMPUTE TLXT-SETTLE-AMT ROUNDED = TLXM-COVER-YTD *
                       TLXM-DISCOUNT / 200
               WHEN TLXM-MSG-TYPE-CD = 'LG'
                   MOVE 3.00 TO TLXT-SETTLE-AMT
               WHEN TLXM-MSG-TYPE-CD = 'LX'
                   MOVE 15.00 TO TLXT-SETTLE-AMT
               WHEN OTHER
                   COMPUTE TLXT-SETTLE-AMT ROUNDED = TLXM-COVER-YTD *
                       TLXM-DISCOUNT / 100
           END-EVALUATE
           IF TLXT-SETTLE-AMT > 0 AND TLXT-SETTLE-AMT < 2.00
               MOVE 2.00 TO TLXT-SETTLE-AMT
           END-IF
           IF TLXT-SETTLE-AMT > 75.00
               MOVE 75.00 TO TLXT-SETTLE-AMT
           END-IF
           ADD TLXT-SETTLE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WIRE AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3160-ACCRUE-WIRE.
           IF TLXM-WIRE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLXM-WIRE-AMT * TLXM-RATE / 100 / 365
               ADD WS-WORK-AMT TO TLXT-SETTLE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WIRE AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3170-ACCRUE-WIRE.
           IF TLXT-WIRE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLXT-WIRE-AMT * TLXM-RATE / 100 / 365
               ADD WS-WORK-AMT TO TLXM-CORRESP-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3180-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF TLXM-EFFECTIVE-DATE > 0 AND TLXM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TLXM-SETTLE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TLXM-SETTLE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD TLXM-SETTLE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD TLXM-SETTLE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLXM-SETTLE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SETTLE YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3190-FLAG-SETTLE.
           IF TLXM-SETTLE-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TLXM-SETTLE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SETTLE GRID TIER FOR THE CORRESP YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3200-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-SETTLE-GRID-RATE(1) TO TLXM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF TLXM-CORRESP-YTD NOT < WS-SETTLE-GRID-LOW(WS-TX)
                   MOVE WS-SETTLE-GRID-RATE(WS-TX) TO TLXM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE QUEUED YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3210-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLXM-REGION-CD
                   ADD TLXM-QUEUED-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLXM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLXM-QUEUED-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE SETTLE YTD BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3220-ACCUM-TRAN.
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
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3230-COUNT-EXCEPT.
           IF TLXM-EXCEPT-CNT < 9999999
               ADD 1 TO TLXM-EXCEPT-CNT
           END-IF
           IF TLXM-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO TLXM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 120 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3240-CHECK-LAST-ACT.
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
           IF WS-DAYS > 120
               MOVE 'Y' TO TLXM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3250-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLXM-TYPE-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLXM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3260-DESCRIBE-TYPE.
           EVALUATE TLXM-TYPE-CD
               WHEN 'K'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'L'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'M'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'P'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'W'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'Z'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E700 CUST ID REQUIRED
      * E304 CHARGE YTD NEGATIVE
      * E662 CHARGE YTD OVER 5000
      * E771 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3270-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXM-CUST-ID = SPACES
                   MOVE 'E700' TO WS-REJECT-CODE
               WHEN TLXM-CHARGE-YTD < 0
                   MOVE 'E304' TO WS-REJECT-CODE
               WHEN TLXM-CHARGE-YTD > 5000.00
                   MOVE 'E662' TO WS-REJECT-CODE
               WHEN TLXM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E771' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3280-COUNT-ITEM.
           IF TLXM-ITEM-CNT < 9999999
               ADD 1 TO TLXM-ITEM-CNT
           END-IF
           IF TLXM-ITEM-CNT NOT < 10
               MOVE 'Y' TO TLXM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SETTLE GRID TIER FOR THE SETTLE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3290-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-SETTLE-GRID-RATE(1) TO TLXM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF TLXM-SETTLE-AMT NOT < WS-SETTLE-GRID-LOW(WS-TX)
                   MOVE WS-SETTLE-GRID-RATE(WS-TX) TO TLXM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3300-COUNT-PAYMENT.
           IF TLXM-PAYMENT-CNT < 9999999
               ADD 1 TO TLXM-PAYMENT-CNT
           END-IF
           IF TLXM-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO TLXM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3310-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF TLXM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO TLXM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SETTLE AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3320-ACCRUE-SETTLE.
           IF TLXM-SETTLE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLXM-SETTLE-AMT * TLXM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO TLXM-COVER-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SETTLE GRID TIER FOR THE WIRE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3330-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-SETTLE-GRID-RATE(1) TO TLXM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF TLXM-WIRE-AMT NOT < WS-SETTLE-GRID-LOW(WS-TX)
                   MOVE WS-SETTLE-GRID-RATE(WS-TX) TO TLXM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 45 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3340-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF TLXM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO TLXM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * CC -> KR WHEN CLOSE DATE IS PAST
      * KR -> LD WHEN CLOSE DATE IS PAST
      * LD -> LS WHEN CLOSE DATE IS PAST
      * LS -> XL WHEN CLOSE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3350-SET-BRANCH.
           EVALUATE TLXM-BRANCH-CD
               WHEN 'CC'
                   IF TLXM-CLOSE-DATE > 0 AND TLXM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KR' TO TLXM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KR'
                   IF TLXM-CLOSE-DATE > 0 AND TLXM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LD' TO TLXM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LD'
                   IF TLXM-CLOSE-DATE > 0 AND TLXM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LS' TO TLXM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LS'
                   IF TLXM-CLOSE-DATE > 0 AND TLXM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XL' TO TLXM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE CHARGE YTD FEE ON THE CORRESP AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3360-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN TLXM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO TLXM-CHARGE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLXM-STOP-FLAG = 'Y'
                   COMPUTE TLXM-CHARGE-YTD ROUNDED = TLXM-CORRESP-AMT *
                       TLXM-DISCOUNT / 200
               WHEN TLXM-REGION-CD = 'G'
                   MOVE 12.00 TO TLXM-CHARGE-YTD
               WHEN TLXM-REGION-CD = 'M'
                   MOVE 10.00 TO TLXM-CHARGE-YTD
               WHEN OTHER
                   COMPUTE TLXM-CHARGE-YTD ROUNDED = TLXM-CORRESP-AMT *
                       TLXM-DISCOUNT / 100
           END-EVALUATE
           IF TLXM-CHARGE-YTD > 0 AND TLXM-CHARGE-YTD < 10.00
               MOVE 10.00 TO TLXM-CHARGE-YTD
           END-IF
           IF TLXM-CHARGE-YTD > 100.00
               MOVE 100.00 TO TLXM-CHARGE-YTD
           END-IF
           ADD TLXM-CHARGE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3370-DESCRIBE-BRANCH.
           EVALUATE TLXM-BRANCH-CD
               WHEN 'CC'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'KR'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'LD'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'LS'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'XL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E141 ADDR REQUIRED
      * E824 WORK AMT NEGATIVE
      * E049 WORK AMT OVER 5000
      * E372 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3380-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXM-ADDR = SPACES
                   MOVE 'E141' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E824' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 5000.00
                   MOVE 'E049' TO WS-REJECT-CODE
               WHEN TLXM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E372' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3390-DESCRIBE-TYPE.
           EVALUATE TLXM-TYPE-CD
               WHEN 'K'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'L'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'M'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'P'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'W'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'Z'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * CC -> KR WHEN NEXT DUE DATE IS PAST
      * KR -> LD WHEN NEXT DUE DATE IS PAST
      * LD -> LS WHEN NEXT DUE DATE IS PAST
      * LS -> XL WHEN NEXT DUE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3400-SET-BRANCH.
           EVALUATE TLXM-BRANCH-CD
               WHEN 'CC'
                   IF TLXM-NEXT-DUE-DATE > 0 AND TLXM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KR' TO TLXM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KR'
                   IF TLXM-NEXT-DUE-DATE > 0 AND TLXM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LD' TO TLXM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LD'
                   IF TLXM-NEXT-DUE-DATE > 0 AND TLXM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LS' TO TLXM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LS'
                   IF TLXM-NEXT-DUE-DATE > 0 AND TLXM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XL' TO TLXM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE MSG TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3410-DESCRIBE-MSG-TYPE.
           EVALUATE TLXM-MSG-TYPE-CD
               WHEN 'LG'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'LX'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'ND'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'PT'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * CC -> KR WHEN NEXT REVIEW DATE IS PAST
      * KR -> LD WHEN NEXT REVIEW DATE IS PAST
      * LD -> LS WHEN NEXT REVIEW DATE IS PAST
      * LS -> XL WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3420-SET-BRANCH.
           EVALUATE TLXM-BRANCH-CD
               WHEN 'CC'
                   IF TLXM-NEXT-REVIEW-DATE > 0 AND
                       TLXM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'KR' TO TLXM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KR'
                   IF TLXM-NEXT-REVIEW-DATE > 0 AND
                       TLXM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'LD' TO TLXM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LD'
                   IF TLXM-NEXT-REVIEW-DATE > 0 AND
                       TLXM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'LS' TO TLXM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LS'
                   IF TLXM-NEXT-REVIEW-DATE > 0 AND
                       TLXM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'XL' TO TLXM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3430-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 90 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3440-CHECK-LAST-ACT.
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
           IF WS-DAYS > 90
               MOVE 'Y' TO TLXM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3450-CHECK-EFFECTIVE.
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
           IF WS-DAYS > 120
               MOVE 'Y' TO TLXM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SETTLE AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3460-FLAG-SETTLE.
           IF TLXT-SETTLE-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TLXT-SETTLE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3470-DESCRIBE-BRANCH.
           EVALUATE TLXM-BRANCH-CD
               WHEN 'CC'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'KR'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'LD'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'LS'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'XL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E071 REJECT CD REQUIRED
      * E958 WIRE AMT NEGATIVE
      * E055 WIRE AMT OVER 99999
      * E741 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3480-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXT-REJECT-CD = SPACES
                   MOVE 'E071' TO WS-REJECT-CODE
               WHEN TLXT-WIRE-AMT < 0
                   MOVE 'E958' TO WS-REJECT-CODE
               WHEN TLXT-WIRE-AMT > 99999.00
                   MOVE 'E055' TO WS-REJECT-CODE
               WHEN TLXM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E741' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3490-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF TLXT-EFF-DATE > 0 AND TLXT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TLXM-CORRESP-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TLXM-CORRESP-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TLXM-CORRESP-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TLXM-CORRESP-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLXM-CORRESP-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * CC -> KR WHEN EXPIRY DATE IS PAST
      * KR -> LD WHEN EXPIRY DATE IS PAST
      * LD -> LS WHEN EXPIRY DATE IS PAST
      * LS -> XL WHEN EXPIRY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3500-SET-BRANCH.
           EVALUATE TLXM-BRANCH-CD
               WHEN 'CC'
                   IF TLXM-EXPIRY-DATE > 0 AND TLXM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KR' TO TLXM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KR'
                   IF TLXM-EXPIRY-DATE > 0 AND TLXM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LD' TO TLXM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LD'
                   IF TLXM-EXPIRY-DATE > 0 AND TLXM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LS' TO TLXM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LS'
                   IF TLXM-EXPIRY-DATE > 0 AND TLXM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XL' TO TLXM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE SETTLE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CORRESP AMT.
      *----------------------------------------------------------------*
       3510-PRORATE-CORRESP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLXM-CORRESP-AMT ROUNDED =
                   TLXM-SETTLE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TLXM-CORRESP-AMT
           END-IF
           ADD TLXM-CORRESP-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3520-AGE-LAST-PAY.
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
                   ADD TLXM-CHARGE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD TLXM-CHARGE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD TLXM-CHARGE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TLXM-CHARGE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLXM-CHARGE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CORRESP YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3530-FLAG-CORRESP.
           IF TLXM-CORRESP-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TLXM-CORRESP-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3540-AGE-NEXT-DUE.
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
                   ADD TLXM-CORRESP-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TLXM-CORRESP-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD TLXM-CORRESP-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TLXM-CORRESP-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLXM-CORRESP-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CHARGE YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3550-FLAG-CHARGE.
           IF TLXM-CHARGE-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TLXM-CHARGE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3560-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF TLXM-EFFECTIVE-DATE > 0 AND TLXM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TLXT-COVER-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD TLXT-COVER-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TLXT-COVER-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TLXT-COVER-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLXT-COVER-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT CHARGE YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3570-SPLIT-CHARGE.
           COMPUTE WS-WORK-AMT = TLXM-CHARGE-YTD / 2
           COMPUTE WS-WORK-AMT-2 = TLXM-CHARGE-YTD - (WS-WORK-AMT * 2)
           COMPUTE TLXT-SETTLE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3580-AGE-OPEN.
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
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3590-COUNT-SEQ.
           IF TLXT-SEQ < 9999999
               ADD 1 TO TLXT-SEQ
           END-IF
           IF TLXT-SEQ NOT < 6
               MOVE 'Y' TO TLXM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3600-COUNT-ITEM.
           IF TLXM-ITEM-CNT < 9999999
               ADD 1 TO TLXM-ITEM-CNT
           END-IF
           IF TLXM-ITEM-CNT NOT < 6
               MOVE 'Y' TO TLXM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3610-DESCRIBE-BRANCH.
           EVALUATE TLXM-BRANCH-CD
               WHEN 'CC'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'KR'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'LD'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'LS'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'XL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE SETTLE GRID TIER FOR THE CORRESP AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3620-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-SETTLE-GRID-RATE(1) TO TLXM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF TLXM-CORRESP-AMT NOT < WS-SETTLE-GRID-LOW(WS-TX)
                   MOVE WS-SETTLE-GRID-RATE(WS-TX) TO TLXM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT COVER AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3630-SPLIT-COVER.
           COMPUTE WS-WORK-AMT = TLXT-COVER-AMT / 12
           COMPUTE WS-WORK-AMT-2 = TLXT-COVER-AMT - (WS-WORK-AMT * 12)
           COMPUTE TLXM-CHARGE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E835 DESC REQUIRED
      * E391 WIRE AMT NEGATIVE
      * E496 WIRE AMT OVER 5000
      * E820 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3640-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXM-DESC = SPACES
                   MOVE 'E835' TO WS-REJECT-CODE
               WHEN TLXT-WIRE-AMT < 0
                   MOVE 'E391' TO WS-REJECT-CODE
               WHEN TLXT-WIRE-AMT > 5000.00
                   MOVE 'E496' TO WS-REJECT-CODE
               WHEN TLXM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E820' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3650-AGE-NEXT-DUE.
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
                   ADD TLXM-QUEUED-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TLXM-QUEUED-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD TLXM-QUEUED-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD TLXM-QUEUED-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLXM-QUEUED-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3660-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF TLXM-NOTICE-DATE > 0 AND TLXM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TLXM-COVER-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TLXM-COVER-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD TLXM-COVER-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TLXM-COVER-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLXM-COVER-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE QUEUED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WIRE AMT.
      *----------------------------------------------------------------*
       3670-PRORATE-WIRE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLXT-WIRE-AMT ROUNDED =
                   TLXT-QUEUED-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TLXT-WIRE-AMT
           END-IF
           ADD TLXT-WIRE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE QUEUED FEE ON THE WORK AMT 2.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3680-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN TLXM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO TLXT-QUEUED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLXM-REVIEW-FLAG = 'Y'
                   COMPUTE TLXT-QUEUED-AMT ROUNDED = WS-WORK-AMT-2 *
                       TLXM-FEE-PCT / 200
               WHEN TLXM-TYPE-CD = 'K'
                   MOVE 12.00 TO TLXT-QUEUED-AMT
               WHEN TLXM-TYPE-CD = 'L'
                   MOVE 10.00 TO TLXT-QUEUED-AMT
               WHEN OTHER
                   COMPUTE TLXT-QUEUED-AMT ROUNDED = WS-WORK-AMT-2 *
                       TLXM-FEE-PCT / 100
           END-EVALUATE
           IF TLXT-QUEUED-AMT > 0 AND TLXT-QUEUED-AMT < 1.00
               MOVE 1.00 TO TLXT-QUEUED-AMT
           END-IF
           IF TLXT-QUEUED-AMT > 50.00
               MOVE 50.00 TO TLXT-QUEUED-AMT
           END-IF
           ADD TLXT-QUEUED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE WIRE FEE ON THE COVER AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3690-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN TLXM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO TLXT-WIRE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLXM-SUSPEND-FLAG = 'Y'
                   COMPUTE TLXT-WIRE-AMT ROUNDED = TLXM-COVER-AMT *
                       TLXM-RATE / 200
               WHEN TLXM-TYPE-CD = 'K'
                   MOVE 7.00 TO TLXT-WIRE-AMT
               WHEN TLXM-TYPE-CD = 'L'
                   MOVE 10.00 TO TLXT-WIRE-AMT
               WHEN OTHER
                   COMPUTE TLXT-WIRE-AMT ROUNDED = TLXM-COVER-AMT *
                       TLXM-RATE / 100
           END-EVALUATE
           IF TLXT-WIRE-AMT > 0 AND TLXT-WIRE-AMT < 1.00
               MOVE 1.00 TO TLXT-WIRE-AMT
           END-IF
           IF TLXT-WIRE-AMT > 100.00
               MOVE 100.00 TO TLXT-WIRE-AMT
           END-IF
           ADD TLXT-WIRE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3700-DESCRIBE-REGION.
           EVALUATE TLXM-REGION-CD
               WHEN 'G'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'M'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'T'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT QUEUED AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3710-SPLIT-QUEUED.
           COMPUTE WS-WORK-AMT = TLXM-QUEUED-AMT / 4
           COMPUTE WS-WORK-AMT-2 = TLXM-QUEUED-AMT - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3720-CHECK-NOTICE.
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
           IF WS-DAYS > 365
               MOVE 'Y' TO TLXT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E251 DESC REQUIRED
      * E660 WORK AMT 2 NEGATIVE
      * E453 WORK AMT 2 OVER 25000
      * E385 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3730-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXM-DESC = SPACES
                   MOVE 'E251' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E660' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 25000.00
                   MOVE 'E453' TO WS-REJECT-CODE
               WHEN TLXM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E385' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CHARGE YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3740-FLAG-CHARGE.
           IF TLXM-CHARGE-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TLXM-CHARGE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT QUEUED AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3750-SPLIT-QUEUED.
           COMPUTE WS-WORK-AMT = TLXM-QUEUED-AMT / 4
           COMPUTE WS-WORK-AMT-2 = TLXM-QUEUED-AMT - (WS-WORK-AMT * 4)
           COMPUTE TLXM-COVER-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * G -> M WHEN NOTICE DATE IS PAST
      * M -> T WHEN NOTICE DATE IS PAST
      * T -> G WHEN NOTICE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3760-SET-REGION.
           EVALUATE TLXM-REGION-CD
               WHEN 'G'
                   IF TLXM-NOTICE-DATE > 0 AND TLXM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO TLXM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF TLXM-NOTICE-DATE > 0 AND TLXM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO TLXM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF TLXM-NOTICE-DATE > 0 AND TLXM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'G' TO TLXM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3770-COUNT-EXCEPT.
           IF TLXM-EXCEPT-CNT < 9999999
               ADD 1 TO TLXM-EXCEPT-CNT
           END-IF
           IF TLXM-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO TLXM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WIRE AMT IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3780-LIMIT-WIRE.
           IF TLXM-WIRE-AMT < 50.00
               MOVE 50.00 TO TLXM-WIRE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXM-WIRE-AMT > 5000.00
               MOVE 5000.00 TO TLXM-WIRE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN LAST PAY DATE IS PAST
      * BAT -> MAL WHEN LAST PAY DATE IS PAST
      * MAL -> TEL WHEN LAST PAY DATE IS PAST
      * TEL -> ONL WHEN LAST PAY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3790-SET-CHANNEL.
           EVALUATE TLXT-CHANNEL-CD
               WHEN 'BRN'
                   IF TLXM-LAST-PAY-DATE > 0 AND TLXM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO TLXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF TLXM-LAST-PAY-DATE > 0 AND TLXM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO TLXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF TLXM-LAST-PAY-DATE > 0 AND TLXM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO TLXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF TLXM-LAST-PAY-DATE > 0 AND TLXM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO TLXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WIRE AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3800-ACCRUE-WIRE.
           IF TLXM-WIRE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLXM-WIRE-AMT * TLXM-RATE / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE QUEUED AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3810-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN TLXT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLXM-REVIEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = TLXM-QUEUED-AMT *
                       TLXM-FEE-PCT / 200
               WHEN TLXT-TRAN-CD = 'IN'
                   MOVE 20.00 TO WS-WORK-AMT
               WHEN TLXT-TRAN-CD = 'OT'
                   MOVE 20.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = TLXM-QUEUED-AMT *
                       TLXM-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 2.00
               MOVE 2.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 150.00
               MOVE 150.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 90 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3820-CHECK-LAST-ACT.
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
           IF WS-DAYS > 90
               MOVE 'Y' TO TLXM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3830-DESCRIBE-REASON.
           EVALUATE TLXM-REASON-CD
               WHEN 'DGN'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'JBT'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'LSS'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'RDP'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'RPL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'SNE'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'UJS'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3840-COUNT-ITEM.
           IF TLXM-ITEM-CNT < 9999999
               ADD 1 TO TLXM-ITEM-CNT
           END-IF
           IF TLXM-ITEM-CNT NOT < 10
               MOVE 'Y' TO TLXM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SETTLE AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3850-ACCRUE-SETTLE.
           IF TLXM-SETTLE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLXM-SETTLE-AMT * TLXM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO TLXT-QUEUED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE COVER AMT BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3860-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLXM-BRANCH-CD
                   ADD TLXM-COVER-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLXM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLXM-COVER-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3870-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SETTLE AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3880-ACCRUE-SETTLE.
           IF TLXM-SETTLE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLXM-SETTLE-AMT * TLXM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO TLXM-COVER-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON COVER AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3890-ACCRUE-COVER.
           IF TLXT-COVER-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLXT-COVER-AMT * TLXM-RATE / 100 / 365
               ADD WS-WORK-AMT TO TLXM-CHARGE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CHARGE AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3900-FLAG-CHARGE.
           IF TLXM-CHARGE-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TLXM-CHARGE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SETTLE AMT IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3910-LIMIT-SETTLE.
           IF TLXT-SETTLE-AMT < 0.00
               MOVE 0.00 TO TLXT-SETTLE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXT-SETTLE-AMT > 2500.00
               MOVE 2500.00 TO TLXT-SETTLE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR MSG TYPE CD:
      * LG -> LX WHEN LAST ACT DATE IS PAST
      * LX -> ND WHEN LAST ACT DATE IS PAST
      * ND -> PT WHEN LAST ACT DATE IS PAST
      * PT -> LG WHEN LAST ACT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3920-SET-MSG-TYPE.
           EVALUATE TLXM-MSG-TYPE-CD
               WHEN 'LG'
                   IF TLXM-LAST-ACT-DATE > 0 AND TLXM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LX' TO TLXM-MSG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LX'
                   IF TLXM-LAST-ACT-DATE > 0 AND TLXM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ND' TO TLXM-MSG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ND'
                   IF TLXM-LAST-ACT-DATE > 0 AND TLXM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PT' TO TLXM-MSG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PT'
                   IF TLXM-LAST-ACT-DATE > 0 AND TLXM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LG' TO TLXM-MSG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * COVER AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3930-FLAG-COVER.
           IF TLXM-COVER-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TLXM-COVER-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E349 CUST ID REQUIRED
      * E719 SETTLE AMT NEGATIVE
      * E314 SETTLE AMT OVER 1000000
      * E120 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3940-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXM-CUST-ID = SPACES
                   MOVE 'E349' TO WS-REJECT-CODE
               WHEN TLXM-SETTLE-AMT < 0
                   MOVE 'E719' TO WS-REJECT-CODE
               WHEN TLXM-SETTLE-AMT > 1000000.00
                   MOVE 'E314' TO WS-REJECT-CODE
               WHEN TLXM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E120' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WIRE YTD BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3950-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLXM-TYPE-CD
                   ADD TLXM-WIRE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLXM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLXM-WIRE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * QUEUED AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3960-FLAG-QUEUED.
           IF TLXM-QUEUED-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TLXM-QUEUED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 90 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3970-CHECK-LAST-PAY.
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
           IF WS-DAYS > 90
               MOVE 'Y' TO TLXM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3980-DESCRIBE-BRANCH.
           EVALUATE TLXM-BRANCH-CD
               WHEN 'CC'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'KR'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'LD'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'LS'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'XL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WIRE YTD IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3990-LIMIT-WIRE.
           IF TLXM-WIRE-YTD < 50.00
               MOVE 50.00 TO TLXM-WIRE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXM-WIRE-YTD > 10000.00
               MOVE 10000.00 TO TLXM-WIRE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CORRESP YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4000-ACCRUE-CORRESP.
           IF TLXM-CORRESP-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLXM-CORRESP-YTD * TLXM-RATE / 100 / 360
               ADD WS-WORK-AMT TO TLXT-QUEUED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SETTLE AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4010-ACCRUE-SETTLE.
           IF TLXT-SETTLE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLXT-SETTLE-AMT * TLXM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO TLXT-COVER-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       4020-COUNT-ITEM.
           IF TLXM-ITEM-CNT < 9999999
               ADD 1 TO TLXM-ITEM-CNT
           END-IF
           IF TLXM-ITEM-CNT NOT < 10
               MOVE 'Y' TO TLXM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CHARGE BAND TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4030-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CHARGE-BAND-RATE(1) TO TLXM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF WS-WORK-AMT NOT < WS-CHARGE-BAND-LOW(WS-TX)
                   MOVE WS-CHARGE-BAND-RATE(WS-TX) TO TLXM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4040-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF TLXM-EFFECTIVE-DATE > 0 AND TLXM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TLXM-COVER-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TLXM-COVER-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD TLXM-COVER-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD TLXM-COVER-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLXM-COVER-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E601 OPERATOR REQUIRED
      * E511 COVER YTD NEGATIVE
      * E028 COVER YTD OVER 5000
      * E739 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       4050-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXT-OPERATOR = SPACES
                   MOVE 'E601' TO WS-REJECT-CODE
               WHEN TLXM-COVER-YTD < 0
                   MOVE 'E511' TO WS-REJECT-CODE
               WHEN TLXM-COVER-YTD > 5000.00
                   MOVE 'E028' TO WS-REJECT-CODE
               WHEN TLXT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E739' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WIRE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CHARGE YTD.
      *----------------------------------------------------------------*
       4060-PRORATE-CHARGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLXM-CHARGE-YTD ROUNDED =
                   TLXM-WIRE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TLXM-CHARGE-YTD
           END-IF
           ADD TLXM-CHARGE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * WIRE YTD IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4070-LIMIT-WIRE.
           IF TLXM-WIRE-YTD < 100.00
               MOVE 100.00 TO TLXM-WIRE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXM-WIRE-YTD > 1000.00
               MOVE 1000.00 TO TLXM-WIRE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * QUEUED AMT IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4080-LIMIT-QUEUED.
           IF TLXT-QUEUED-AMT < 10.00
               MOVE 10.00 TO TLXT-QUEUED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXT-QUEUED-AMT > 10000.00
               MOVE 10000.00 TO TLXT-QUEUED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4090-COUNT-EXCEPT.
           IF TLXM-EXCEPT-CNT < 9999999
               ADD 1 TO TLXM-EXCEPT-CNT
           END-IF
           IF TLXM-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO TLXT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT QUEUED AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4100-SPLIT-QUEUED.
           COMPUTE WS-WORK-AMT = TLXT-QUEUED-AMT / 4
           COMPUTE WS-WORK-AMT-2 = TLXT-QUEUED-AMT - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4110-AGE-OPEN.
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
                   ADD TLXM-CORRESP-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TLXM-CORRESP-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD TLXM-CORRESP-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TLXM-CORRESP-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLXM-CORRESP-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CHARGE YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4120-ACCRUE-CHARGE.
           IF TLXM-CHARGE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLXM-CHARGE-YTD * TLXM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO TLXM-QUEUED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE COVER YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       4130-PRORATE-COVER.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLXM-COVER-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE SETTLE GRID TIER FOR THE SETTLE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4140-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-SETTLE-GRID-RATE(1) TO TLXM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF TLXM-SETTLE-YTD NOT < WS-SETTLE-GRID-LOW(WS-TX)
                   MOVE WS-SETTLE-GRID-RATE(WS-TX) TO TLXM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WIRE YTD IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4150-LIMIT-WIRE.
           IF TLXM-WIRE-YTD < 50.00
               MOVE 50.00 TO TLXM-WIRE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXM-WIRE-YTD > 2500.00
               MOVE 2500.00 TO TLXM-WIRE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E907 DESC REQUIRED
      * E988 WORK AMT 2 NEGATIVE
      * E383 WORK AMT 2 OVER 50000
      * E899 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       4160-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXM-DESC = SPACES
                   MOVE 'E907' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E988' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 50000.00
                   MOVE 'E383' TO WS-REJECT-CODE
               WHEN TLXM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E899' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE QUEUED FEE ON THE CORRESP YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4170-ASSESS-STOP.
           EVALUATE TRUE
               WHEN TLXM-STOP-FLAG = 'Y'
                   MOVE ZERO TO TLXT-QUEUED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLXM-PREFERRED-FLAG = 'Y'
                   COMPUTE TLXT-QUEUED-AMT ROUNDED = TLXM-CORRESP-YTD *
                       TLXM-FEE-PCT / 200
               WHEN TLXT-CHANNEL-CD = 'BRN'
                   MOVE 3.00 TO TLXT-QUEUED-AMT
               WHEN TLXT-CHANNEL-CD = 'BAT'
                   MOVE 10.00 TO TLXT-QUEUED-AMT
               WHEN OTHER
                   COMPUTE TLXT-QUEUED-AMT ROUNDED = TLXM-CORRESP-YTD *
                       TLXM-FEE-PCT / 100
           END-EVALUATE
           IF TLXT-QUEUED-AMT > 0 AND TLXT-QUEUED-AMT < 10.00
               MOVE 10.00 TO TLXT-QUEUED-AMT
           END-IF
           IF TLXT-QUEUED-AMT > 100.00
               MOVE 100.00 TO TLXT-QUEUED-AMT
           END-IF
           ADD TLXT-QUEUED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE SETTLE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       4180-PRORATE-SETTLE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   TLXT-SETTLE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TEST KEY CD:
      * EA -> GA WHEN LAST PAY DATE IS PAST
      * GA -> HL WHEN LAST PAY DATE IS PAST
      * HL -> JC WHEN LAST PAY DATE IS PAST
      * JC -> KC WHEN LAST PAY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4190-SET-TEST-KEY.
           EVALUATE TLXM-TEST-KEY-CD
               WHEN 'EA'
                   IF TLXM-LAST-PAY-DATE > 0 AND TLXM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GA' TO TLXM-TEST-KEY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GA'
                   IF TLXM-LAST-PAY-DATE > 0 AND TLXM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HL' TO TLXM-TEST-KEY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HL'
                   IF TLXM-LAST-PAY-DATE > 0 AND TLXM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JC' TO TLXM-TEST-KEY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JC'
                   IF TLXM-LAST-PAY-DATE > 0 AND TLXM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KC' TO TLXM-TEST-KEY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SETTLE AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4200-FLAG-SETTLE.
           IF TLXM-SETTLE-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TLXM-SETTLE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CHARGE YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4210-ACCRUE-CHARGE.
           IF TLXM-CHARGE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLXM-CHARGE-YTD * TLXM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO TLXM-CORRESP-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WIRE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4220-FLAG-WIRE.
           IF TLXT-WIRE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TLXT-WIRE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * WIRE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4230-FLAG-WIRE.
           IF TLXM-WIRE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TLXM-WIRE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE WORK AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       4240-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN TLXM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLXM-STOP-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = WS-WORK-AMT *
                       TLXM-DISCOUNT / 200
               WHEN TLXM-TEST-KEY-CD = 'EA'
                   MOVE 10.00 TO WS-WORK-AMT-2
               WHEN TLXM-TEST-KEY-CD = 'GA'
                   MOVE 20.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = WS-WORK-AMT *
                       TLXM-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 10.00
               MOVE 10.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 150.00
               MOVE 150.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE SETTLE YTD FEE ON THE WIRE AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4250-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN TLXM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO TLXM-SETTLE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLXM-PREFERRED-FLAG = 'Y'
                   COMPUTE TLXM-SETTLE-YTD ROUNDED = TLXT-WIRE-AMT *
                       TLXM-RATE / 200
               WHEN TLXM-BRANCH-CD = 'CC'
                   MOVE 5.00 TO TLXM-SETTLE-YTD
               WHEN TLXM-BRANCH-CD = 'KR'
                   MOVE 10.00 TO TLXM-SETTLE-YTD
               WHEN OTHER
                   COMPUTE TLXM-SETTLE-YTD ROUNDED = TLXT-WIRE-AMT *
                       TLXM-RATE / 100
           END-EVALUATE
           IF TLXM-SETTLE-YTD > 0 AND TLXM-SETTLE-YTD < 5.00
               MOVE 5.00 TO TLXM-SETTLE-YTD
           END-IF
           IF TLXM-SETTLE-YTD > 75.00
               MOVE 75.00 TO TLXM-SETTLE-YTD
           END-IF
           ADD TLXM-SETTLE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT CORRESP AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4260-SPLIT-CORRESP.
           COMPUTE WS-WORK-AMT = TLXM-CORRESP-AMT / 2
           COMPUTE WS-WORK-AMT-2 = TLXM-CORRESP-AMT - (WS-WORK-AMT * 2)
           COMPUTE TLXT-QUEUED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SETTLE AMT IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4270-LIMIT-SETTLE.
           IF TLXT-SETTLE-AMT < 50.00
               MOVE 50.00 TO TLXT-SETTLE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXT-SETTLE-AMT > 500.00
               MOVE 500.00 TO TLXT-SETTLE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

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
                   MOVE TXCAL-QUEUED-YTD
                     TO TLXM-QUEUED-YTD
                   MOVE TXCAL-PREFERRED-FLAG
                     TO TLXM-PREFERRED-FLAG
                   MOVE TXCAL-REVIEW-FLAG
                     TO TLXM-REVIEW-FLAG
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

      *----------------------------------------------------------------*
      * PASS THE TLX FIELDS TO TXFEE AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7010-CALL-TXFEE.
           INITIALIZE TXFEE-PARMS
           MOVE TLXM-SETTLE-AMT
             TO TXFEE-SETTLE-AMT
           MOVE TLXM-SETTLE-YTD
             TO TXFEE-SETTLE-YTD
           MOVE TLXM-COVER-AMT
             TO TXFEE-COVER-AMT
           MOVE TLXM-QUEUED-YTD
             TO TXFEE-QUEUED-YTD
           MOVE TLXM-WIRE-YTD
             TO TXFEE-WIRE-YTD
           MOVE TLXM-CORRESP-AMT
             TO TXFEE-CORRESP-AMT
           MOVE TLXM-CLOSE-DATE
             TO TXFEE-CLOSE-DATE
           MOVE TLXM-LAST-PAY-DATE
             TO TXFEE-LAST-PAY-DATE
           MOVE TLXM-LAST-ACT-DATE
             TO TXFEE-LAST-ACT-DATE
           MOVE TLXM-NEXT-DUE-DATE
             TO TXFEE-NEXT-DUE-DATE
           MOVE TLXM-REGION-CD
             TO TXFEE-REGION-CD
           MOVE TLXM-TYPE-CD
             TO TXFEE-TYPE-CD
           MOVE TLXM-BRANCH-CD
             TO TXFEE-BRANCH-CD
           MOVE TLXM-TEST-KEY-CD
             TO TXFEE-TEST-KEY-CD
           MOVE TLXM-NOTICE-SENT-FLAG
             TO TXFEE-NOTICE-SENT-FLAG
           MOVE TLXM-SUSPEND-FLAG
             TO TXFEE-SUSPEND-FLAG
           MOVE TLXM-PREFERRED-FLAG
             TO TXFEE-PREFERRED-FLAG
           MOVE TLXM-EXCEPT-CNT
             TO TXFEE-EXCEPT-CNT
           MOVE TLXM-ITEM-CNT
             TO TXFEE-ITEM-CNT
           MOVE TLXM-FEE-PCT
             TO TXFEE-FEE-PCT
           MOVE TLXM-RATE
             TO TXFEE-RATE
           MOVE TLXM-CUST-ID
             TO TXFEE-CUST-ID
           MOVE TLXM-CONTACT
             TO TXFEE-CONTACT
           CALL 'TXFEE' USING TXFEE-PARMS
           EVALUATE TXFEE-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE TXFEE-SETTLE-YTD
                     TO TLXM-SETTLE-YTD
                   MOVE TXFEE-COVER-AMT
                     TO TLXM-COVER-AMT
                   MOVE TXFEE-NOTICE-SENT-FLAG
                     TO TLXM-NOTICE-SENT-FLAG
                   MOVE TXFEE-PREFERRED-FLAG
                     TO TLXM-PREFERRED-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RFEE' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-TXFEE' TO ABND-PARAGRAPH
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
           DISPLAY 'TXXPURG READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'TXXPURG' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
