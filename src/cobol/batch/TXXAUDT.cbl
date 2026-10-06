      *================================================================*
      * PROGRAM  : TXXAUDT                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : TELEX WIRE MESSAGING - AUDIT TRAIL. READS THE TLX   *
      *            TRANSACTIONS, UPDATES THE TLX MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. TESTED TELEX FUNDS TRANSFER     *
      *            MESSAGES AND TEST KEY VALIDATION                    *
      * INPUT    : TLXMST (KSDS), TLXTRN, BANKCTL                      *
      * OUTPUT   : TLXOUT, TXXAURPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, TXPRC, TXELG            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1993-10-26 RJT  ORIGINAL                                       *
      * 1993-09-23 DPS  COVER NOTICE TEXT PER LEGAL CHG7828            *
      * 1994-02-13 SKN  AUDIT FINDING - QUEUED CONTROL TOTALS REQ3557  *
      * 1995-06-09 JLM  NEW COVER REPORT COLUMNS PRB6453               *
      * 1996-05-14 EWS  CHARGE RERUN PROTECTION PRB0455                *
      * 1999-04-11 CRP  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2001-12-14 DPS  PERFORMANCE - WIRE READ AHEAD INC5725          *
      * 2001-09-18 TWH  COVER CUTOFF MOVED PRB2218                     *
      * 2004-04-09 PQR  WIRE STATUS CODES ADDED REQ1240                *
      * 2004-08-22 GAF  QUEUED NOTICE TEXT PER LEGAL REQ9352           *
      * 2005-08-27 RJT  PERFORMANCE - QUEUED READ AHEAD REQ0483        *
      * 2007-06-23 GAF  WIRE TABLE EXPANDED REQ0241                    *
      * 2008-10-19 JLM  CORRESP TABLE EXPANDED CHG8115                 *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TXXAUDT.
       AUTHOR. J L MARSH.

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
           SELECT REPORT-FILE ASSIGN TO "TXXAURPT"
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
                                           VALUE 14.000000.
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
      * WS-COVER-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-COVER-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000234500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000183500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000750000000254000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005750000000508000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005800000000498000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006800000000838000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007800000000771000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007850000000481000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008350000000144000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008450000000459000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013450000000062000'.
       01  WS-COVER-SCHED-TBL REDEFINES WS-COVER-SCHED-VALUES.
           05  WS-COVER-SCHED-ROW OCCURS 11 TIMES.
               10  WS-COVER-SCHED-LOW      PIC 9(09)V99.
               10  WS-COVER-SCHED-RATE     PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-WIRE-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-WIRE-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000050000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000417000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010500000000690500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011500000000086500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014000000000183000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014050000000237500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00024050000000261000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00024550000000575500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025550000000082000'.
       01  WS-WIRE-SCHED-TBL REDEFINES WS-WIRE-SCHED-VALUES.
           05  WS-WIRE-SCHED-ROW OCCURS 9 TIMES.
               10  WS-WIRE-SCHED-LOW       PIC 9(09)V99.
               10  WS-WIRE-SCHED-RATE      PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-COVER-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-COVER-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000373000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000280500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000896000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007500000000288000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000558500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010100000000591000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015100000000070500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016100000000293000'.
       01  WS-COVER-BAND-TBL REDEFINES WS-COVER-BAND-VALUES.
           05  WS-COVER-BAND-ROW OCCURS 8 TIMES.
               10  WS-COVER-BAND-LOW       PIC 9(09)V99.
               10  WS-COVER-BAND-RATE      PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-COVER-SCHEDX - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-COVER-SCHEDX-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000697000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000198000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005050000000890000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006050000000328000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008550000000863000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008800000000061500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00009300000000244000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00009350000000279500'.
       01  WS-COVER-SCHEDX-TBL REDEFINES WS-COVER-SCHEDX-VALUES.
           05  WS-COVER-SCHEDX-ROW OCCURS 8 TIMES.
               10  WS-COVER-SCHEDX-LOW     PIC 9(09)V99.
               10  WS-COVER-SCHEDX-RATE    PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  TXXAUDT  TELEX WIRE MESSAGING'.
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
               PERFORM 3000-EDIT-MEMO
               PERFORM 3010-DESCRIBE-TYPE
               PERFORM 3020-EDIT-DESC
               PERFORM 3030-CHECK-LAST-PAY
               PERFORM 3040-ACCUM-CHANNEL
               PERFORM 3050-SELECT-DISCOUNT
               PERFORM 3060-ACCRUE-SETTLE
               PERFORM 3070-FLAG-WIRE
               PERFORM 3080-SELECT-RATE
               PERFORM 3090-SELECT-RATE
               PERFORM 3100-PRORATE-ITEM
               PERFORM 3110-SELECT-RATE
               PERFORM 3120-PRORATE-COVER
               PERFORM 3130-ASSESS-REVIEW
               PERFORM 3140-LIMIT-WIRE
               PERFORM 3150-PRORATE-CORRESP
               PERFORM 3160-COUNT-EXCEPT
               PERFORM 3170-ACCRUE-CORRESP
               PERFORM 3180-SPLIT-CHARGE
               PERFORM 3190-AGE-POST
               PERFORM 3200-ASSESS-SUSPEND
               PERFORM 3210-CHECK-EXPIRY
               PERFORM 3220-CHECK-EFFECTIVE
               PERFORM 3230-LIMIT-SETTLE
               PERFORM 3240-PRORATE-SETTLE
               PERFORM 3250-AGE-LAST-PAY
               PERFORM 3260-ASSESS-REVIEW
               PERFORM 3270-DESCRIBE-TRAN
               PERFORM 3280-ACCRUE-CHARGE
               PERFORM 3290-LIMIT-QUEUED
               PERFORM 3300-SELECT-FEE-PCT
               PERFORM 3310-SELECT-RATE
               PERFORM 3320-ASSESS-REVIEW
               PERFORM 3330-COUNT-SEQ
               PERFORM 3340-EDIT-DESC
               PERFORM 3350-ASSESS-PREFERRED
               PERFORM 3360-COUNT-SEQ
               PERFORM 3370-FLAG-CORRESP
               PERFORM 3380-CHECK-EFF
               PERFORM 3390-COUNT-PAYMENT
               PERFORM 3400-CHECK-NOTICE
               PERFORM 3410-AGE-EFF
               PERFORM 3420-ACCRUE-SETTLE
               PERFORM 3430-SPLIT-COVER
               PERFORM 3440-COUNT-SEQ
               PERFORM 3450-SPLIT-COVER
               PERFORM 3460-ACCUM-BRANCH
               PERFORM 3470-SPLIT-CHARGE
               PERFORM 3480-AGE-LAST-PAY
               PERFORM 3490-AGE-EFFECTIVE
               PERFORM 3500-SET-REASON
               PERFORM 3510-DESCRIBE-TRAN
               PERFORM 3520-SPLIT-CORRESP
               PERFORM 3530-CHECK-NOTICE
               PERFORM 3540-ASSESS-STOP
               PERFORM 3550-LIMIT-SETTLE
               PERFORM 3560-CHECK-LAST-PAY
               PERFORM 3570-ACCRUE-SETTLE
               PERFORM 3580-PRORATE-SETTLE
               PERFORM 3590-DESCRIBE-TRAN
               PERFORM 3600-ACCRUE-QUEUED
               PERFORM 3610-FLAG-QUEUED
               PERFORM 3620-SET-TYPE
               PERFORM 3630-DESCRIBE-MSG-TYPE
               PERFORM 3640-DESCRIBE-CHANNEL
               PERFORM 3650-FLAG-ITEM
               PERFORM 3660-ACCUM-REGION
               PERFORM 3670-SELECT-RATE
               PERFORM 3680-EDIT-CONTACT
               PERFORM 3690-SET-TRAN
               PERFORM 3700-CHECK-EXPIRY
               PERFORM 3710-AGE-EFFECTIVE
               PERFORM 3720-FLAG-WIRE
               PERFORM 3730-AGE-LAST-ACT
               PERFORM 3740-SELECT-DISCOUNT
               PERFORM 3750-DESCRIBE-TEST-KEY
               PERFORM 3760-ACCRUE-QUEUED
               PERFORM 3770-COUNT-EXCEPT
               PERFORM 3780-ACCUM-TYPE
               PERFORM 3790-AGE-EFFECTIVE
               PERFORM 3800-FLAG-COVER
               PERFORM 3810-CHECK-EXPIRY
               PERFORM 3820-FLAG-CORRESP
               PERFORM 3830-SET-TRAN
               PERFORM 3840-PRORATE-COVER
               PERFORM 3850-COUNT-SEQ
               PERFORM 3860-COUNT-PAYMENT
               PERFORM 3870-SELECT-FEE-PCT
               PERFORM 3880-AGE-NEXT-DUE
               PERFORM 3890-FLAG-CHARGE
               PERFORM 3900-FLAG-ITEM
               PERFORM 3910-ASSESS-NOTICE-SENT
               PERFORM 3920-LIMIT-SETTLE
               PERFORM 3930-DESCRIBE-TRAN
               PERFORM 3940-ASSESS-PREFERRED
               PERFORM 3950-COUNT-SEQ
               PERFORM 3960-PRORATE-CORRESP
               PERFORM 3970-SELECT-RATE
               PERFORM 3980-ASSESS-REVIEW
               PERFORM 3990-PRORATE-SETTLE
               PERFORM 4000-CHECK-LAST-PAY
               PERFORM 4010-COUNT-SEQ
               PERFORM 4020-AGE-NOTICE
               PERFORM 4030-LIMIT-SETTLE
               PERFORM 4040-SPLIT-WIRE
               PERFORM 4050-EDIT-MEMO
               PERFORM 4060-LIMIT-QUEUED
               PERFORM 4070-FLAG-ITEM
               PERFORM 4080-LIMIT-QUEUED
               PERFORM 4090-SPLIT-QUEUED
               PERFORM 4100-COUNT-EXCEPT
               PERFORM 4110-LIMIT-CORRESP
               PERFORM 4120-FLAG-ITEM
               PERFORM 4130-SPLIT-SETTLE
               PERFORM 4140-LIMIT-ITEM
               PERFORM 4150-SET-TEST-KEY
               PERFORM 4160-SPLIT-WIRE
               PERFORM 4170-DESCRIBE-TRAN
               PERFORM 4180-ACCUM-TEST-KEY
               PERFORM 4190-DESCRIBE-MSG-TYPE
               PERFORM 4200-PRORATE-COVER
               PERFORM 4210-ASSESS-OVERRIDE
               PERFORM 4220-AGE-NEXT-REVIEW
               PERFORM 4230-AGE-NOTICE
               PERFORM 4240-SPLIT-QUEUED
               PERFORM 4250-ACCUM-REGION
               PERFORM 4260-AGE-NOTICE
               PERFORM 7020-CALL-TXPRC
               PERFORM 7000-CALL-TXELG
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
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E512 MEMO REQUIRED
      * E821 WORK AMT 2 NEGATIVE
      * E953 WORK AMT 2 OVER 1000000
      * E772 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3000-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXT-MEMO = SPACES
                   MOVE 'E512' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E821' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 1000000.00
                   MOVE 'E953' TO WS-REJECT-CODE
               WHEN TLXT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E772' TO WS-REJECT-CODE
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
       3010-DESCRIBE-TYPE.
           EVALUATE TLXM-TYPE-CD
               WHEN 'K'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'L'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'M'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'P'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'W'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'Z'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E785 DESC REQUIRED
      * E175 COVER AMT NEGATIVE
      * E647 COVER AMT OVER 10000
      * E780 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3020-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXM-DESC = SPACES
                   MOVE 'E785' TO WS-REJECT-CODE
               WHEN TLXT-COVER-AMT < 0
                   MOVE 'E175' TO WS-REJECT-CODE
               WHEN TLXT-COVER-AMT > 10000.00
                   MOVE 'E647' TO WS-REJECT-CODE
               WHEN TLXM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E780' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3030-CHECK-LAST-PAY.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO TLXM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3040-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLXT-CHANNEL-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLXT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE WIRE SCHED TIER FOR THE SETTLE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3050-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-WIRE-SCHED-RATE(1) TO TLXM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF TLXM-SETTLE-YTD NOT < WS-WIRE-SCHED-LOW(WS-TX)
                   MOVE WS-WIRE-SCHED-RATE(WS-TX) TO TLXM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SETTLE AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3060-ACCRUE-SETTLE.
           IF TLXT-SETTLE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLXT-SETTLE-AMT * TLXM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO TLXM-CHARGE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WIRE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3070-FLAG-WIRE.
           IF TLXT-WIRE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TLXT-WIRE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE COVER SCHED TIER FOR THE WIRE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3080-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-COVER-SCHED-RATE(1) TO TLXM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF TLXM-WIRE-AMT NOT < WS-COVER-SCHED-LOW(WS-TX)
                   MOVE WS-COVER-SCHED-RATE(WS-TX) TO TLXM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE COVER SCHED TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3090-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-COVER-SCHED-RATE(1) TO TLXM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF WS-WORK-AMT NOT < WS-COVER-SCHED-LOW(WS-TX)
                   MOVE WS-COVER-SCHED-RATE(WS-TX) TO TLXM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3100-PRORATE-ITEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE COVER SCHED TIER FOR THE QUEUED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3110-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-COVER-SCHED-RATE(1) TO TLXM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF TLXM-QUEUED-AMT NOT < WS-COVER-SCHED-LOW(WS-TX)
                   MOVE WS-COVER-SCHED-RATE(WS-TX) TO TLXM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CHARGE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO COVER AMT.
      *----------------------------------------------------------------*
       3120-PRORATE-COVER.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLXT-COVER-AMT ROUNDED =
                   TLXM-CHARGE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TLXT-COVER-AMT
           END-IF
           ADD TLXT-COVER-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE WIRE YTD FEE ON THE QUEUED AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3130-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN TLXM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO TLXM-WIRE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLXM-OVERRIDE-FLAG = 'Y'
                   COMPUTE TLXM-WIRE-YTD ROUNDED = TLXM-QUEUED-AMT *
                       TLXM-DISCOUNT / 200
               WHEN TLXT-TRAN-CD = 'IN'
                   MOVE 12.00 TO TLXM-WIRE-YTD
               WHEN TLXT-TRAN-CD = 'OT'
                   MOVE 15.00 TO TLXM-WIRE-YTD
               WHEN OTHER
                   COMPUTE TLXM-WIRE-YTD ROUNDED = TLXM-QUEUED-AMT *
                       TLXM-DISCOUNT / 100
           END-EVALUATE
           IF TLXM-WIRE-YTD > 0 AND TLXM-WIRE-YTD < 10.00
               MOVE 10.00 TO TLXM-WIRE-YTD
           END-IF
           IF TLXM-WIRE-YTD > 150.00
               MOVE 150.00 TO TLXM-WIRE-YTD
           END-IF
           ADD TLXM-WIRE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * WIRE AMT IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3140-LIMIT-WIRE.
           IF TLXT-WIRE-AMT < 0.00
               MOVE 0.00 TO TLXT-WIRE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXT-WIRE-AMT > 10000.00
               MOVE 10000.00 TO TLXT-WIRE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CORRESP AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3150-PRORATE-CORRESP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   TLXM-CORRESP-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3160-COUNT-EXCEPT.
           IF TLXM-EXCEPT-CNT < 9999999
               ADD 1 TO TLXM-EXCEPT-CNT
           END-IF
           IF TLXM-EXCEPT-CNT NOT < 3
               MOVE 'Y' TO TLXM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CORRESP YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3170-ACCRUE-CORRESP.
           IF TLXM-CORRESP-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLXM-CORRESP-YTD * TLXM-RATE / 100 / 360
               ADD WS-WORK-AMT TO TLXM-CHARGE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CHARGE YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3180-SPLIT-CHARGE.
           COMPUTE WS-WORK-AMT = TLXM-CHARGE-YTD / 12
           COMPUTE WS-WORK-AMT-2 = TLXM-CHARGE-YTD - (WS-WORK-AMT * 12)
           COMPUTE TLXM-CORRESP-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3190-AGE-POST.
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
                   ADD TLXT-COVER-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD TLXT-COVER-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TLXT-COVER-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD TLXT-COVER-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLXT-COVER-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE SETTLE FEE ON THE COVER AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3200-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN TLXM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO TLXT-SETTLE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLXM-OVERRIDE-FLAG = 'Y'
                   COMPUTE TLXT-SETTLE-AMT ROUNDED = TLXT-COVER-AMT *
                       TLXM-RATE / 200
               WHEN TLXM-REASON-CD = 'DGN'
                   MOVE 3.00 TO TLXT-SETTLE-AMT
               WHEN TLXM-REASON-CD = 'JBT'
                   MOVE 15.00 TO TLXT-SETTLE-AMT
               WHEN OTHER
                   COMPUTE TLXT-SETTLE-AMT ROUNDED = TLXT-COVER-AMT *
                       TLXM-RATE / 100
           END-EVALUATE
           IF TLXT-SETTLE-AMT > 0 AND TLXT-SETTLE-AMT < 2.00
               MOVE 2.00 TO TLXT-SETTLE-AMT
           END-IF
           IF TLXT-SETTLE-AMT > 150.00
               MOVE 150.00 TO TLXT-SETTLE-AMT
           END-IF
           ADD TLXT-SETTLE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 15 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3210-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF TLXM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO TLXM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3220-CHECK-EFFECTIVE.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO TLXM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SETTLE AMT IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3230-LIMIT-SETTLE.
           IF TLXM-SETTLE-AMT < 25.00
               MOVE 25.00 TO TLXM-SETTLE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXM-SETTLE-AMT > 1000.00
               MOVE 1000.00 TO TLXM-SETTLE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CORRESP AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO SETTLE AMT.
      *----------------------------------------------------------------*
       3240-PRORATE-SETTLE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLXM-SETTLE-AMT ROUNDED =
                   TLXM-CORRESP-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TLXM-SETTLE-AMT
           END-IF
           ADD TLXM-SETTLE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3250-AGE-LAST-PAY.
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
                   ADD TLXT-QUEUED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TLXT-QUEUED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD TLXT-QUEUED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD TLXT-QUEUED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLXT-QUEUED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE QUEUED FEE ON THE QUEUED YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3260-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN TLXM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO TLXT-QUEUED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLXM-OVERRIDE-FLAG = 'Y'
                   COMPUTE TLXT-QUEUED-AMT ROUNDED = TLXM-QUEUED-YTD *
                       TLXM-RATE / 200
               WHEN TLXM-REASON-CD = 'DGN'
                   MOVE 5.00 TO TLXT-QUEUED-AMT
               WHEN TLXM-REASON-CD = 'JBT'
                   MOVE 12.00 TO TLXT-QUEUED-AMT
               WHEN OTHER
                   COMPUTE TLXT-QUEUED-AMT ROUNDED = TLXM-QUEUED-YTD *
                       TLXM-RATE / 100
           END-EVALUATE
           IF TLXT-QUEUED-AMT > 0 AND TLXT-QUEUED-AMT < 5.00
               MOVE 5.00 TO TLXT-QUEUED-AMT
           END-IF
           IF TLXT-QUEUED-AMT > 75.00
               MOVE 75.00 TO TLXT-QUEUED-AMT
           END-IF
           ADD TLXT-QUEUED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3270-DESCRIBE-TRAN.
           EVALUATE TLXT-TRAN-CD
               WHEN 'IN'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'OT'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'RJ'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'RP'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'CN'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CHARGE AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3280-ACCRUE-CHARGE.
           IF TLXM-CHARGE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLXM-CHARGE-AMT * TLXM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO TLXT-SETTLE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * QUEUED AMT IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3290-LIMIT-QUEUED.
           IF TLXT-QUEUED-AMT < 25.00
               MOVE 25.00 TO TLXT-QUEUED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXT-QUEUED-AMT > 1000.00
               MOVE 1000.00 TO TLXT-QUEUED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE WIRE SCHED TIER FOR THE CHARGE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3300-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-WIRE-SCHED-RATE(1) TO TLXM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF TLXM-CHARGE-AMT NOT < WS-WIRE-SCHED-LOW(WS-TX)
                   MOVE WS-WIRE-SCHED-RATE(WS-TX) TO TLXM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE COVER BAND TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3310-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-COVER-BAND-RATE(1) TO TLXM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF WS-WORK-AMT-2 NOT < WS-COVER-BAND-LOW(WS-TX)
                   MOVE WS-COVER-BAND-RATE(WS-TX) TO TLXM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SETTLE YTD FEE ON THE SETTLE AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3320-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN TLXM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO TLXM-SETTLE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLXM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE TLXM-SETTLE-YTD ROUNDED = TLXT-SETTLE-AMT *
                       TLXM-FEE-PCT / 200
               WHEN TLXM-REASON-CD = 'DGN'
                   MOVE 15.00 TO TLXM-SETTLE-YTD
               WHEN TLXM-REASON-CD = 'JBT'
                   MOVE 20.00 TO TLXM-SETTLE-YTD
               WHEN OTHER
                   COMPUTE TLXM-SETTLE-YTD ROUNDED = TLXT-SETTLE-AMT *
                       TLXM-FEE-PCT / 100
           END-EVALUATE
           IF TLXM-SETTLE-YTD > 0 AND TLXM-SETTLE-YTD < 1.00
               MOVE 1.00 TO TLXM-SETTLE-YTD
           END-IF
           IF TLXM-SETTLE-YTD > 150.00
               MOVE 150.00 TO TLXM-SETTLE-YTD
           END-IF
           ADD TLXM-SETTLE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3330-COUNT-SEQ.
           IF TLXT-SEQ < 9999999
               ADD 1 TO TLXT-SEQ
           END-IF
           IF TLXT-SEQ NOT < 6
               MOVE 'Y' TO TLXM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E856 DESC REQUIRED
      * E792 WORK AMT 2 NEGATIVE
      * E143 WORK AMT 2 OVER 5000
      * E513 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3340-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXM-DESC = SPACES
                   MOVE 'E856' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E792' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 5000.00
                   MOVE 'E143' TO WS-REJECT-CODE
               WHEN TLXM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E513' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CHARGE FEE ON THE COVER AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3350-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN TLXM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO TLXM-CHARGE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLXM-OVERRIDE-FLAG = 'Y'
                   COMPUTE TLXM-CHARGE-AMT ROUNDED = TLXM-COVER-AMT *
                       TLXM-RATE / 200
               WHEN TLXT-TRAN-CD = 'IN'
                   MOVE 10.00 TO TLXM-CHARGE-AMT
               WHEN TLXT-TRAN-CD = 'OT'
                   MOVE 12.00 TO TLXM-CHARGE-AMT
               WHEN OTHER
                   COMPUTE TLXM-CHARGE-AMT ROUNDED = TLXM-COVER-AMT *
                       TLXM-RATE / 100
           END-EVALUATE
           IF TLXM-CHARGE-AMT > 0 AND TLXM-CHARGE-AMT < 10.00
               MOVE 10.00 TO TLXM-CHARGE-AMT
           END-IF
           IF TLXM-CHARGE-AMT > 75.00
               MOVE 75.00 TO TLXM-CHARGE-AMT
           END-IF
           ADD TLXM-CHARGE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3360-COUNT-SEQ.
           IF TLXT-SEQ < 9999999
               ADD 1 TO TLXT-SEQ
           END-IF
           IF TLXT-SEQ NOT < 12
               MOVE 'Y' TO TLXM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CORRESP YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3370-FLAG-CORRESP.
           IF TLXM-CORRESP-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TLXM-CORRESP-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 30 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3380-CHECK-EFF.
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
           IF WS-DAYS > 30
               MOVE 'Y' TO TLXM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3390-COUNT-PAYMENT.
           IF TLXM-PAYMENT-CNT < 9999999
               ADD 1 TO TLXM-PAYMENT-CNT
           END-IF
           IF TLXM-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO TLXM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3400-CHECK-NOTICE.
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
           IF WS-DAYS > 90
               MOVE 'Y' TO TLXT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3410-AGE-EFF.
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
               WHEN WS-DAYS NOT > 15
                   ADD TLXM-CORRESP-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD TLXM-CORRESP-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TLXM-CORRESP-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLXM-CORRESP-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SETTLE YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3420-ACCRUE-SETTLE.
           IF TLXM-SETTLE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLXM-SETTLE-YTD * TLXM-RATE / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT COVER AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3430-SPLIT-COVER.
           COMPUTE WS-WORK-AMT = TLXT-COVER-AMT / 3
           COMPUTE WS-WORK-AMT-2 = TLXT-COVER-AMT - (WS-WORK-AMT * 3)
           COMPUTE TLXM-WIRE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3440-COUNT-SEQ.
           IF TLXT-SEQ < 9999999
               ADD 1 TO TLXT-SEQ
           END-IF
           IF TLXT-SEQ NOT < 12
               MOVE 'Y' TO TLXM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT COVER AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3450-SPLIT-COVER.
           COMPUTE WS-WORK-AMT = TLXT-COVER-AMT / 12
           COMPUTE WS-WORK-AMT-2 = TLXT-COVER-AMT - (WS-WORK-AMT * 12)
           COMPUTE TLXM-COVER-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CHARGE AMT BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3460-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLXM-BRANCH-CD
                   ADD TLXM-CHARGE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLXM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLXM-CHARGE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CHARGE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3470-SPLIT-CHARGE.
           COMPUTE WS-WORK-AMT = TLXM-CHARGE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = TLXM-CHARGE-AMT - (WS-WORK-AMT * 2)
           COMPUTE TLXM-CORRESP-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3480-AGE-LAST-PAY.
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
                   ADD TLXM-COVER-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TLXM-COVER-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD TLXM-COVER-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TLXM-COVER-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLXM-COVER-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3490-AGE-EFFECTIVE.
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
                   ADD TLXM-WIRE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD TLXM-WIRE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TLXM-WIRE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TLXM-WIRE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLXM-WIRE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * DGN -> JBT WHEN EXPIRY DATE IS PAST
      * JBT -> LSS WHEN EXPIRY DATE IS PAST
      * LSS -> RDP WHEN EXPIRY DATE IS PAST
      * RDP -> RPL WHEN EXPIRY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3500-SET-REASON.
           EVALUATE TLXM-REASON-CD
               WHEN 'DGN'
                   IF TLXM-EXPIRY-DATE > 0 AND TLXM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JBT' TO TLXM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JBT'
                   IF TLXM-EXPIRY-DATE > 0 AND TLXM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LSS' TO TLXM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LSS'
                   IF TLXM-EXPIRY-DATE > 0 AND TLXM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RDP' TO TLXM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RDP'
                   IF TLXM-EXPIRY-DATE > 0 AND TLXM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RPL' TO TLXM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3510-DESCRIBE-TRAN.
           EVALUATE TLXT-TRAN-CD
               WHEN 'IN'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'OT'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'RJ'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'RP'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'CN'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT CORRESP YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3520-SPLIT-CORRESP.
           COMPUTE WS-WORK-AMT = TLXM-CORRESP-YTD / 4
           COMPUTE WS-WORK-AMT-2 = TLXM-CORRESP-YTD - (WS-WORK-AMT * 4)
           COMPUTE TLXM-SETTLE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3530-CHECK-NOTICE.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO TLXM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE COVER FEE ON THE CHARGE YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3540-ASSESS-STOP.
           EVALUATE TRUE
               WHEN TLXM-STOP-FLAG = 'Y'
                   MOVE ZERO TO TLXT-COVER-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLXM-SUSPEND-FLAG = 'Y'
                   COMPUTE TLXT-COVER-AMT ROUNDED = TLXM-CHARGE-YTD *
                       TLXM-FEE-PCT / 200
               WHEN TLXM-TYPE-CD = 'K'
                   MOVE 3.00 TO TLXT-COVER-AMT
               WHEN TLXM-TYPE-CD = 'L'
                   MOVE 12.00 TO TLXT-COVER-AMT
               WHEN OTHER
                   COMPUTE TLXT-COVER-AMT ROUNDED = TLXM-CHARGE-YTD *
                       TLXM-FEE-PCT / 100
           END-EVALUATE
           IF TLXT-COVER-AMT > 0 AND TLXT-COVER-AMT < 5.00
               MOVE 5.00 TO TLXT-COVER-AMT
           END-IF
           IF TLXT-COVER-AMT > 250.00
               MOVE 250.00 TO TLXT-COVER-AMT
           END-IF
           ADD TLXT-COVER-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SETTLE AMT IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3550-LIMIT-SETTLE.
           IF TLXM-SETTLE-AMT < 0.00
               MOVE 0.00 TO TLXM-SETTLE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXM-SETTLE-AMT > 50000.00
               MOVE 50000.00 TO TLXM-SETTLE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3560-CHECK-LAST-PAY.
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
           IF WS-DAYS > 120
               MOVE 'Y' TO TLXM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SETTLE YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3570-ACCRUE-SETTLE.
           IF TLXM-SETTLE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLXM-SETTLE-YTD * TLXM-RATE / 100 / 365
               ADD WS-WORK-AMT TO TLXM-CORRESP-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO SETTLE YTD.
      *----------------------------------------------------------------*
       3580-PRORATE-SETTLE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLXM-SETTLE-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TLXM-SETTLE-YTD
           END-IF
           ADD TLXM-SETTLE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3590-DESCRIBE-TRAN.
           EVALUATE TLXT-TRAN-CD
               WHEN 'IN'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'OT'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'RJ'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'RP'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'CN'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON QUEUED AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3600-ACCRUE-QUEUED.
           IF TLXT-QUEUED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLXT-QUEUED-AMT * TLXM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO TLXM-CORRESP-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * QUEUED AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3610-FLAG-QUEUED.
           IF TLXM-QUEUED-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TLXM-QUEUED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * K -> L WHEN LAST ACT DATE IS PAST
      * L -> M WHEN LAST ACT DATE IS PAST
      * M -> P WHEN LAST ACT DATE IS PAST
      * P -> W WHEN LAST ACT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3620-SET-TYPE.
           EVALUATE TLXM-TYPE-CD
               WHEN 'K'
                   IF TLXM-LAST-ACT-DATE > 0 AND TLXM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO TLXM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF TLXM-LAST-ACT-DATE > 0 AND TLXM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO TLXM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF TLXM-LAST-ACT-DATE > 0 AND TLXM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'P' TO TLXM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'P'
                   IF TLXM-LAST-ACT-DATE > 0 AND TLXM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO TLXM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE MSG TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3630-DESCRIBE-MSG-TYPE.
           EVALUATE TLXM-MSG-TYPE-CD
               WHEN 'LG'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'LX'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'ND'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'PT'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3640-DESCRIBE-CHANNEL.
           EVALUATE TLXT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3650-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WIRE YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3660-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLXM-REGION-CD
                   ADD TLXM-WIRE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLXM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLXM-WIRE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE COVER SCHED TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3670-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-COVER-SCHED-RATE(1) TO TLXM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF WS-WORK-AMT-2 NOT < WS-COVER-SCHED-LOW(WS-TX)
                   MOVE WS-COVER-SCHED-RATE(WS-TX) TO TLXM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E969 CONTACT REQUIRED
      * E467 WORK AMT NEGATIVE
      * E465 WORK AMT OVER 50000
      * E080 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3680-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXM-CONTACT = SPACES
                   MOVE 'E969' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E467' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 50000.00
                   MOVE 'E465' TO WS-REJECT-CODE
               WHEN TLXM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E080' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * IN -> OT WHEN CLOSE DATE IS PAST
      * OT -> RJ WHEN CLOSE DATE IS PAST
      * RJ -> RP WHEN CLOSE DATE IS PAST
      * RP -> CN WHEN CLOSE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3690-SET-TRAN.
           EVALUATE TLXT-TRAN-CD
               WHEN 'IN'
                   IF TLXM-CLOSE-DATE > 0 AND TLXM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'OT' TO TLXT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'OT'
                   IF TLXM-CLOSE-DATE > 0 AND TLXM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RJ' TO TLXT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RJ'
                   IF TLXM-CLOSE-DATE > 0 AND TLXM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RP' TO TLXT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RP'
                   IF TLXM-CLOSE-DATE > 0 AND TLXM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CN' TO TLXT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3700-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF TLXM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO TLXM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3710-AGE-EFFECTIVE.
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
               WHEN WS-DAYS NOT > 120
                   ADD TLXM-COVER-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TLXM-COVER-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLXM-COVER-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WIRE YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3720-FLAG-WIRE.
           IF TLXM-WIRE-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TLXM-WIRE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3730-AGE-LAST-ACT.
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
                   ADD TLXM-WIRE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD TLXM-WIRE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD TLXM-WIRE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TLXM-WIRE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLXM-WIRE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE COVER BAND TIER FOR THE SETTLE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3740-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-COVER-BAND-RATE(1) TO TLXM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF TLXM-SETTLE-YTD NOT < WS-COVER-BAND-LOW(WS-TX)
                   MOVE WS-COVER-BAND-RATE(WS-TX) TO TLXM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TEST KEY CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3750-DESCRIBE-TEST-KEY.
           EVALUATE TLXM-TEST-KEY-CD
               WHEN 'EA'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'GA'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'HL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'JC'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'KC'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'KD'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'PJ'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON QUEUED AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3760-ACCRUE-QUEUED.
           IF TLXT-QUEUED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLXT-QUEUED-AMT * TLXM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3770-COUNT-EXCEPT.
           IF TLXM-EXCEPT-CNT < 9999999
               ADD 1 TO TLXM-EXCEPT-CNT
           END-IF
           IF TLXM-EXCEPT-CNT NOT < 3
               MOVE 'Y' TO TLXM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE SETTLE AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3780-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLXM-TYPE-CD
                   ADD TLXM-SETTLE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLXM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLXM-SETTLE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3790-AGE-EFFECTIVE.
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
                   ADD TLXM-CORRESP-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TLXM-CORRESP-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD TLXM-CORRESP-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD TLXM-CORRESP-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLXM-CORRESP-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * COVER YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3800-FLAG-COVER.
           IF TLXM-COVER-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TLXM-COVER-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 10 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3810-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF TLXM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO TLXM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CORRESP AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3820-FLAG-CORRESP.
           IF TLXM-CORRESP-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TLXM-CORRESP-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * IN -> OT WHEN NEXT DUE DATE IS PAST
      * OT -> RJ WHEN NEXT DUE DATE IS PAST
      * RJ -> RP WHEN NEXT DUE DATE IS PAST
      * RP -> CN WHEN NEXT DUE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3830-SET-TRAN.
           EVALUATE TLXT-TRAN-CD
               WHEN 'IN'
                   IF TLXM-NEXT-DUE-DATE > 0 AND TLXM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'OT' TO TLXT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'OT'
                   IF TLXM-NEXT-DUE-DATE > 0 AND TLXM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RJ' TO TLXT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RJ'
                   IF TLXM-NEXT-DUE-DATE > 0 AND TLXM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RP' TO TLXT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RP'
                   IF TLXM-NEXT-DUE-DATE > 0 AND TLXM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CN' TO TLXT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE QUEUED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO COVER YTD.
      *----------------------------------------------------------------*
       3840-PRORATE-COVER.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLXM-COVER-YTD ROUNDED =
                   TLXT-QUEUED-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TLXM-COVER-YTD
           END-IF
           ADD TLXM-COVER-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3850-COUNT-SEQ.
           IF TLXT-SEQ < 9999999
               ADD 1 TO TLXT-SEQ
           END-IF
           IF TLXT-SEQ NOT < 5
               MOVE 'Y' TO TLXM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3860-COUNT-PAYMENT.
           IF TLXM-PAYMENT-CNT < 9999999
               ADD 1 TO TLXM-PAYMENT-CNT
           END-IF
           IF TLXM-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO TLXM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE COVER SCHEDX TIER FOR THE CORRESP YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3870-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-COVER-SCHEDX-RATE(1) TO TLXM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF TLXM-CORRESP-YTD NOT < WS-COVER-SCHEDX-LOW(WS-TX)
                   MOVE WS-COVER-SCHEDX-RATE(WS-TX) TO TLXM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3880-AGE-NEXT-DUE.
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
               WHEN WS-DAYS NOT > 45
                   ADD TLXM-COVER-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD TLXM-COVER-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLXM-COVER-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CHARGE YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3890-FLAG-CHARGE.
           IF TLXM-CHARGE-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TLXM-CHARGE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3900-FLAG-ITEM.
           IF WS-WORK-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE QUEUED FEE ON THE SETTLE YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3910-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN TLXM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO TLXT-QUEUED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLXM-OVERRIDE-FLAG = 'Y'
                   COMPUTE TLXT-QUEUED-AMT ROUNDED = TLXM-SETTLE-YTD *
                       TLXM-FEE-PCT / 200
               WHEN TLXM-MSG-TYPE-CD = 'LG'
                   MOVE 7.00 TO TLXT-QUEUED-AMT
               WHEN TLXM-MSG-TYPE-CD = 'LX'
                   MOVE 7.00 TO TLXT-QUEUED-AMT
               WHEN OTHER
                   COMPUTE TLXT-QUEUED-AMT ROUNDED = TLXM-SETTLE-YTD *
                       TLXM-FEE-PCT / 100
           END-EVALUATE
           IF TLXT-QUEUED-AMT > 0 AND TLXT-QUEUED-AMT < 1.00
               MOVE 1.00 TO TLXT-QUEUED-AMT
           END-IF
           IF TLXT-QUEUED-AMT > 75.00
               MOVE 75.00 TO TLXT-QUEUED-AMT
           END-IF
           ADD TLXT-QUEUED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SETTLE AMT IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3920-LIMIT-SETTLE.
           IF TLXM-SETTLE-AMT < 10.00
               MOVE 10.00 TO TLXM-SETTLE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXM-SETTLE-AMT > 5000.00
               MOVE 5000.00 TO TLXM-SETTLE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3930-DESCRIBE-TRAN.
           EVALUATE TLXT-TRAN-CD
               WHEN 'IN'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'OT'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'RJ'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'RP'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'CN'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE QUEUED AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3940-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN TLXM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLXM-SUSPEND-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = TLXM-QUEUED-AMT *
                       TLXM-DISCOUNT / 200
               WHEN TLXM-TEST-KEY-CD = 'EA'
                   MOVE 10.00 TO WS-WORK-AMT
               WHEN TLXM-TEST-KEY-CD = 'GA'
                   MOVE 5.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = TLXM-QUEUED-AMT *
                       TLXM-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 5.00
               MOVE 5.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 75.00
               MOVE 75.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3950-COUNT-SEQ.
           IF TLXT-SEQ < 9999999
               ADD 1 TO TLXT-SEQ
           END-IF
           IF TLXT-SEQ NOT < 10
               MOVE 'Y' TO TLXM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CORRESP AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CORRESP YTD.
      *----------------------------------------------------------------*
       3960-PRORATE-CORRESP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLXM-CORRESP-YTD ROUNDED =
                   TLXM-CORRESP-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TLXM-CORRESP-YTD
           END-IF
           ADD TLXM-CORRESP-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE COVER BAND TIER FOR THE CHARGE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3970-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-COVER-BAND-RATE(1) TO TLXM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF TLXM-CHARGE-AMT NOT < WS-COVER-BAND-LOW(WS-TX)
                   MOVE WS-COVER-BAND-RATE(WS-TX) TO TLXM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE WIRE YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3980-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN TLXM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLXM-OVERRIDE-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = TLXM-WIRE-YTD *
                       TLXM-DISCOUNT / 200
               WHEN TLXM-MSG-TYPE-CD = 'LG'
                   MOVE 3.00 TO WS-WORK-AMT
               WHEN TLXM-MSG-TYPE-CD = 'LX'
                   MOVE 10.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = TLXM-WIRE-YTD *
                       TLXM-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 1.00
               MOVE 1.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 35.00
               MOVE 35.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE SETTLE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3990-PRORATE-SETTLE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   TLXM-SETTLE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4000-CHECK-LAST-PAY.
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
           IF WS-DAYS > 120
               MOVE 'Y' TO TLXM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       4010-COUNT-SEQ.
           IF TLXT-SEQ < 9999999
               ADD 1 TO TLXT-SEQ
           END-IF
           IF TLXT-SEQ NOT < 10
               MOVE 'Y' TO TLXM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-45 AND OVER 45 DAYS.
      *----------------------------------------------------------------*
       4020-AGE-NOTICE.
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
                   ADD TLXT-COVER-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TLXT-COVER-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD TLXT-COVER-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 45
                   ADD TLXT-COVER-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLXT-COVER-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SETTLE YTD IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4030-LIMIT-SETTLE.
           IF TLXM-SETTLE-YTD < 10.00
               MOVE 10.00 TO TLXM-SETTLE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXM-SETTLE-YTD > 50000.00
               MOVE 50000.00 TO TLXM-SETTLE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WIRE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4040-SPLIT-WIRE.
           COMPUTE WS-WORK-AMT = TLXM-WIRE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = TLXM-WIRE-AMT - (WS-WORK-AMT * 4)
           COMPUTE TLXT-SETTLE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E925 MEMO REQUIRED
      * E528 COVER AMT NEGATIVE
      * E612 COVER AMT OVER 25000
      * E925 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       4050-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXT-MEMO = SPACES
                   MOVE 'E925' TO WS-REJECT-CODE
               WHEN TLXM-COVER-AMT < 0
                   MOVE 'E528' TO WS-REJECT-CODE
               WHEN TLXM-COVER-AMT > 25000.00
                   MOVE 'E612' TO WS-REJECT-CODE
               WHEN TLXT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E925' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * QUEUED AMT IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4060-LIMIT-QUEUED.
           IF TLXT-QUEUED-AMT < 25.00
               MOVE 25.00 TO TLXT-QUEUED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXT-QUEUED-AMT > 5000.00
               MOVE 5000.00 TO TLXT-QUEUED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4070-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * QUEUED AMT IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4080-LIMIT-QUEUED.
           IF TLXT-QUEUED-AMT < 10.00
               MOVE 10.00 TO TLXT-QUEUED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXT-QUEUED-AMT > 500.00
               MOVE 500.00 TO TLXT-QUEUED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT QUEUED AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4090-SPLIT-QUEUED.
           COMPUTE WS-WORK-AMT = TLXT-QUEUED-AMT / 2
           COMPUTE WS-WORK-AMT-2 = TLXT-QUEUED-AMT - (WS-WORK-AMT * 2)
           COMPUTE TLXM-WIRE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       4100-COUNT-EXCEPT.
           IF TLXM-EXCEPT-CNT < 9999999
               ADD 1 TO TLXM-EXCEPT-CNT
           END-IF
           IF TLXM-EXCEPT-CNT NOT < 3
               MOVE 'Y' TO TLXM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CORRESP YTD IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4110-LIMIT-CORRESP.
           IF TLXM-CORRESP-YTD < 100.00
               MOVE 100.00 TO TLXM-CORRESP-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXM-CORRESP-YTD > 2500.00
               MOVE 2500.00 TO TLXM-CORRESP-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4120-FLAG-ITEM.
           IF WS-WORK-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SETTLE YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4130-SPLIT-SETTLE.
           COMPUTE WS-WORK-AMT = TLXM-SETTLE-YTD / 4
           COMPUTE WS-WORK-AMT-2 = TLXM-SETTLE-YTD - (WS-WORK-AMT * 4)
           COMPUTE TLXM-CHARGE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4140-LIMIT-ITEM.
           IF WS-WORK-AMT < 50.00
               MOVE 50.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 2500.00
               MOVE 2500.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TEST KEY CD:
      * EA -> GA WHEN EXPIRY DATE IS PAST
      * GA -> HL WHEN EXPIRY DATE IS PAST
      * HL -> JC WHEN EXPIRY DATE IS PAST
      * JC -> KC WHEN EXPIRY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4150-SET-TEST-KEY.
           EVALUATE TLXM-TEST-KEY-CD
               WHEN 'EA'
                   IF TLXM-EXPIRY-DATE > 0 AND TLXM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GA' TO TLXM-TEST-KEY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GA'
                   IF TLXM-EXPIRY-DATE > 0 AND TLXM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HL' TO TLXM-TEST-KEY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HL'
                   IF TLXM-EXPIRY-DATE > 0 AND TLXM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JC' TO TLXM-TEST-KEY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JC'
                   IF TLXM-EXPIRY-DATE > 0 AND TLXM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KC' TO TLXM-TEST-KEY-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WIRE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4160-SPLIT-WIRE.
           COMPUTE WS-WORK-AMT = TLXT-WIRE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = TLXT-WIRE-AMT - (WS-WORK-AMT * 12)
           COMPUTE TLXM-CORRESP-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4170-DESCRIBE-TRAN.
           EVALUATE TLXT-TRAN-CD
               WHEN 'IN'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'OT'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'RJ'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'RP'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'CN'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY TEST KEY CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4180-ACCUM-TEST-KEY.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLXM-TEST-KEY-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLXM-TEST-KEY-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE MSG TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4190-DESCRIBE-MSG-TYPE.
           EVALUATE TLXM-MSG-TYPE-CD
               WHEN 'LG'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'LX'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ND'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'PT'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE QUEUED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO COVER AMT.
      *----------------------------------------------------------------*
       4200-PRORATE-COVER.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLXT-COVER-AMT ROUNDED =
                   TLXM-QUEUED-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TLXT-COVER-AMT
           END-IF
           ADD TLXT-COVER-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE QUEUED FEE ON THE CORRESP AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       4210-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN TLXM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO TLXM-QUEUED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLXT-REVERSAL-FLAG = 'Y'
                   COMPUTE TLXM-QUEUED-AMT ROUNDED = TLXM-CORRESP-AMT *
                       TLXM-RATE / 200
               WHEN TLXM-REGION-CD = 'G'
                   MOVE 5.00 TO TLXM-QUEUED-AMT
               WHEN TLXM-REGION-CD = 'M'
                   MOVE 7.00 TO TLXM-QUEUED-AMT
               WHEN OTHER
                   COMPUTE TLXM-QUEUED-AMT ROUNDED = TLXM-CORRESP-AMT *
                       TLXM-RATE / 100
           END-EVALUATE
           IF TLXM-QUEUED-AMT > 0 AND TLXM-QUEUED-AMT < 5.00
               MOVE 5.00 TO TLXM-QUEUED-AMT
           END-IF
           IF TLXM-QUEUED-AMT > 250.00
               MOVE 250.00 TO TLXM-QUEUED-AMT
           END-IF
           ADD TLXM-QUEUED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4220-AGE-NEXT-REVIEW.
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
                   ADD TLXM-SETTLE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD TLXM-SETTLE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TLXM-SETTLE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TLXM-SETTLE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLXM-SETTLE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4230-AGE-NOTICE.
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
                   ADD TLXM-COVER-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TLXM-COVER-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TLXM-COVER-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TLXM-COVER-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLXM-COVER-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT QUEUED YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4240-SPLIT-QUEUED.
           COMPUTE WS-WORK-AMT = TLXM-QUEUED-YTD / 3
           COMPUTE WS-WORK-AMT-2 = TLXM-QUEUED-YTD - (WS-WORK-AMT * 3)
           COMPUTE TLXM-SETTLE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE COVER YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4250-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLXM-REGION-CD
                   ADD TLXM-COVER-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLXM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLXM-COVER-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4260-AGE-NOTICE.
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
                   ADD TLXM-WIRE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TLXM-WIRE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TLXM-WIRE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TLXM-WIRE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLXM-WIRE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

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
                   MOVE TXPRC-SETTLE-AMT
                     TO TLXM-SETTLE-AMT
                   MOVE TXPRC-NOTICE-SENT-FLAG
                     TO TLXM-NOTICE-SENT-FLAG
                   MOVE TXPRC-STOP-FLAG
                     TO TLXM-STOP-FLAG
                   MOVE TXPRC-REVIEW-FLAG
                     TO TLXM-REVIEW-FLAG
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
                   MOVE TXELG-CORRESP-YTD
                     TO TLXM-CORRESP-YTD
                   MOVE TXELG-SUSPEND-FLAG
                     TO TLXM-SUSPEND-FLAG
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
           DISPLAY 'TXXAUDT READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'TXXAUDT' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
