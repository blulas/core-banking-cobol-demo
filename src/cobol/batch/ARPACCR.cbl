      *================================================================*
      * PROGRAM  : ARPACCR                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : ACCOUNT RECONCILEMENT - DAILY ACCRUAL. READS THE ARP*
      *            TRANSACTIONS, UPDATES THE ARP MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. FULL AND PARTIAL ACCOUNT        *
      *            RECONCILEMENT SERVICE FOR CORPORATES                *
      * INPUT    : ARPMST (KSDS), ARPTRN, BANKCTL                      *
      * OUTPUT   : ARPOUT, ARPACRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, ARSTAT, ARRISK          *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-01-17 PQR  ORIGINAL                                       *
      * 1998-04-27 PQR  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2007-10-21 TWH  VOID CUTOFF MOVED PRB6051                      *
      * 2008-07-01 TWH  AUDIT FINDING - STOP CONTROL TOTALS REQ4268    *
      * 2014-09-04 BHS  PERFORMANCE - ISSUED READ AHEAD CHG6885        *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ARPACCR.
       AUTHOR. B H SANDERS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "ARPMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS ARPM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "ARPTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "ARPOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "ARPACRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY ARPMST.
       FD  TRAN-FILE.
       COPY ARPTRN.
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
                                           VALUE 18.000000.
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
      * WS-OUTSTAND-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-OUTSTAND-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000580500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000730000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020000000000756000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020500000000692000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021000000000169000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023500000000669000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023750000000620000'.
       01  WS-OUTSTAND-TIER-TBL REDEFINES WS-OUTSTAND-TIER-VALUES.
           05  WS-OUTSTAND-TIER-ROW OCCURS 7 TIMES.
               10  WS-OUTSTAND-TIER-LOW    PIC 9(09)V99.
               10  WS-OUTSTAND-TIER-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-PAID-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PAID-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000390000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000054000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012500000000367000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017500000000463500'.
       01  WS-PAID-TIER-TBL REDEFINES WS-PAID-TIER-VALUES.
           05  WS-PAID-TIER-ROW OCCURS 4 TIMES.
               10  WS-PAID-TIER-LOW        PIC 9(09)V99.
               10  WS-PAID-TIER-RATE       PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  ARPACCR  ACCOUNT RECONCILEMENT'.
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
       COPY ARSTATP.
       COPY ARRISKP.

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
               MOVE 'ARPMST' TO ABND-FILE-NAME
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
           MOVE ARPT-KEY TO ARPM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-CHECK-LAST-MAINT
               PERFORM 3010-ASSESS-STOP
               PERFORM 3020-SELECT-DISCOUNT
               PERFORM 3030-ACCUM-CYCLE
               PERFORM 3040-DESCRIBE-CYCLE
               PERFORM 3050-ACCRUE-ISSUED
               PERFORM 3060-SET-BRANCH
               PERFORM 3070-COUNT-CYCLE
               PERFORM 3080-SPLIT-ISSUED
               PERFORM 3090-AGE-EFF
               PERFORM 3100-AGE-LAST-MAINT
               PERFORM 3110-LIMIT-OUTSTAND
               PERFORM 3120-ACCRUE-STOP
               PERFORM 3130-PRORATE-PAID
               PERFORM 3140-SPLIT-FEE
               PERFORM 3150-SET-BRANCH
               PERFORM 3160-ACCUM-RECON-TYPE
               PERFORM 3170-PRORATE-ISSUED
               PERFORM 3180-SPLIT-VOID
               PERFORM 3190-ASSESS-HOLD
               PERFORM 3200-SELECT-FEE-PCT
               PERFORM 3210-DESCRIBE-BRANCH
               PERFORM 3220-LIMIT-STOP
               PERFORM 3230-ACCRUE-FEE
               PERFORM 3240-AGE-POST
               PERFORM 3250-CHECK-NEXT-REVIEW
               PERFORM 3260-LIMIT-OUTSTAND
               PERFORM 3270-CHECK-OPEN
               PERFORM 3280-EDIT-MEMO
               PERFORM 3290-ASSESS-PREFERRED
               PERFORM 3300-CHECK-POST
               PERFORM 3310-PRORATE-STOP
               PERFORM 3320-FLAG-FEE
               PERFORM 3330-COUNT-PAYMENT
               PERFORM 3340-COUNT-SEQ
               PERFORM 3350-CHECK-NEXT-REVIEW
               PERFORM 3360-CHECK-LAST-MAINT
               PERFORM 3370-AGE-EFFECTIVE
               PERFORM 3380-CHECK-EFF
               PERFORM 3390-LIMIT-OUTSTAND
               PERFORM 3400-SET-SOURCE
               PERFORM 3410-ASSESS-EXEMPT
               PERFORM 3420-EDIT-CUST-ID
               PERFORM 3430-PRORATE-FEE
               PERFORM 3440-EDIT-CONTACT
               PERFORM 3450-SELECT-DISCOUNT
               PERFORM 3460-EDIT-REJECT
               PERFORM 3470-PRORATE-FEE
               PERFORM 3480-LIMIT-ITEM
               PERFORM 3490-ACCRUE-STOP
               PERFORM 3500-SPLIT-OUTSTAND
               PERFORM 3510-PRORATE-OUTSTAND
               PERFORM 3520-DESCRIBE-CHANNEL
               PERFORM 3530-SPLIT-ISSUED
               PERFORM 3540-DESCRIBE-BRANCH
               PERFORM 3550-PRORATE-ISSUED
               PERFORM 3560-ACCUM-TRAN
               PERFORM 3570-PRORATE-PAID
               PERFORM 3580-LIMIT-ISSUED
               PERFORM 3590-SELECT-DISCOUNT
               PERFORM 3600-ASSESS-REVERSAL
               PERFORM 3610-PRORATE-FEE
               PERFORM 3620-PRORATE-PAID
               PERFORM 3630-SPLIT-OUTSTAND
               PERFORM 3640-SELECT-PENALTY-PCT
               PERFORM 3650-PRORATE-ISSUED
               PERFORM 3660-COUNT-CYCLE
               PERFORM 3670-EDIT-REJECT
               PERFORM 3680-LIMIT-VOID
               PERFORM 3690-EDIT-DESC
               PERFORM 3700-ASSESS-NOTICE-SENT
               PERFORM 3710-AGE-CLOSE
               PERFORM 3720-SET-REGION
               PERFORM 3730-SELECT-DISCOUNT
               PERFORM 3740-DESCRIBE-BRANCH
               PERFORM 3750-EDIT-CONTACT
               PERFORM 3760-SET-CHANNEL
               PERFORM 3770-DESCRIBE-REGION
               PERFORM 3780-SET-REGION
               PERFORM 3790-DESCRIBE-REGION
               PERFORM 3800-SPLIT-PAID
               PERFORM 3810-DESCRIBE-CHANNEL
               PERFORM 3820-FLAG-FEE
               PERFORM 3830-SET-SOURCE
               PERFORM 3840-LIMIT-ISSUED
               PERFORM 3850-DESCRIBE-CYCLE
               PERFORM 3860-ASSESS-EXEMPT
               PERFORM 3870-LIMIT-ITEM
               PERFORM 3880-PRORATE-FEE
               PERFORM 3890-CHECK-CLOSE
               PERFORM 3900-PRORATE-OUTSTAND
               PERFORM 3910-SELECT-FEE-PCT
               PERFORM 3920-SELECT-PENALTY-PCT
               PERFORM 3930-SELECT-PENALTY-PCT
               PERFORM 3940-ACCRUE-OUTSTAND
               PERFORM 3950-SET-SOURCE
               PERFORM 3960-LIMIT-ISSUED
               PERFORM 3970-DESCRIBE-CYCLE
               PERFORM 3980-FLAG-VOID
               PERFORM 3990-SELECT-FEE-PCT
               PERFORM 4000-DESCRIBE-REGION
               PERFORM 4010-ACCRUE-STOP
               PERFORM 4020-CHECK-NEXT-DUE
               PERFORM 4030-ACCRUE-VOID
               PERFORM 4040-ASSESS-REVERSAL
               PERFORM 4050-ACCRUE-VOID
               PERFORM 4060-ASSESS-NOTICE-SENT
               PERFORM 4070-SPLIT-PAID
               PERFORM 4080-AGE-EFFECTIVE
               PERFORM 4090-CHECK-OPEN
               PERFORM 4100-EDIT-REF-NO
               PERFORM 4110-SPLIT-ISSUED
               PERFORM 4120-ASSESS-PREFERRED
               PERFORM 4130-SPLIT-OUTSTAND
               PERFORM 4140-ACCRUE-VOID
               PERFORM 4150-AGE-NEXT-REVIEW
               PERFORM 4160-SET-BRANCH
               PERFORM 4170-SELECT-DISCOUNT
               PERFORM 4180-EDIT-ADDR
               PERFORM 4190-ASSESS-HOLD
               PERFORM 4200-PRORATE-VOID
               PERFORM 4210-DESCRIBE-CYCLE
               PERFORM 4220-ACCUM-BRANCH
               PERFORM 4230-EDIT-DESC
               PERFORM 4240-DESCRIBE-BRANCH
               PERFORM 4250-ACCRUE-FEE-PCT
               PERFORM 4260-LIMIT-PAID
               PERFORM 4270-PRORATE-STOP
               PERFORM 4280-SPLIT-ISSUED
               PERFORM 4290-COUNT-SEQ
               PERFORM 4300-SELECT-PENALTY-PCT
               PERFORM 4310-SELECT-PENALTY-PCT
               PERFORM 4320-SPLIT-FEE
               PERFORM 4330-SELECT-PENALTY-PCT
               PERFORM 4340-LIMIT-VOID
               PERFORM 4350-EDIT-OPERATOR
               PERFORM 4360-AGE-OPEN
               PERFORM 4370-ASSESS-NOTICE-SENT
               PERFORM 4380-DESCRIBE-REGION
               PERFORM 4390-ASSESS-HOLD
               PERFORM 4400-EDIT-REF-NO
               PERFORM 4410-LIMIT-STOP
               PERFORM 4420-EDIT-MEMO
               PERFORM 4430-SET-BRANCH
               PERFORM 4440-SPLIT-ISSUED
               PERFORM 7010-CALL-ARSTAT
               PERFORM 7030-CALL-ARRISK
               REWRITE ARP-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'ARPMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM ARP-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 90 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3000-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF ARPM-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO ARPT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAID FEE ON THE OUTSTAND AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3010-ASSESS-STOP.
           EVALUATE TRUE
               WHEN ARPM-STOP-FLAG = 'Y'
                   MOVE ZERO TO ARPM-PAID-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARPM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE ARPM-PAID-AMT ROUNDED = ARPM-OUTSTAND-AMT *
                       ARPM-FEE-PCT / 200
               WHEN ARPM-RECON-TYPE-CD = 'EEH'
                   MOVE 15.00 TO ARPM-PAID-AMT
               WHEN ARPM-RECON-TYPE-CD = 'KBS'
                   MOVE 5.00 TO ARPM-PAID-AMT
               WHEN OTHER
                   COMPUTE ARPM-PAID-AMT ROUNDED = ARPM-OUTSTAND-AMT *
                       ARPM-FEE-PCT / 100
           END-EVALUATE
           IF ARPM-PAID-AMT > 0 AND ARPM-PAID-AMT < 5.00
               MOVE 5.00 TO ARPM-PAID-AMT
           END-IF
           IF ARPM-PAID-AMT > 35.00
               MOVE 35.00 TO ARPM-PAID-AMT
           END-IF
           ADD ARPM-PAID-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE OUTSTAND TIER TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3020-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-OUTSTAND-TIER-RATE(1) TO ARPM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF ARPT-FEE-AMT NOT < WS-OUTSTAND-TIER-LOW(WS-TX)
                   MOVE WS-OUTSTAND-TIER-RATE(WS-TX) TO ARPM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE YTD BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3030-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARPM-CYCLE-CD
                   ADD ARPM-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARPM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ARPM-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3040-DESCRIBE-CYCLE.
           EVALUATE ARPM-CYCLE-CD
               WHEN 'CP'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'MR'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'PY'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'SP'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'TP'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ISSUED AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3050-ACCRUE-ISSUED.
           IF ARPT-ISSUED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPT-ISSUED-AMT * ARPM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO ARPM-OUTSTAND-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * FN -> GM WHEN LAST PAY DATE IS PAST
      * GM -> GV WHEN LAST PAY DATE IS PAST
      * GV -> HK WHEN LAST PAY DATE IS PAST
      * HK -> VX WHEN LAST PAY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3060-SET-BRANCH.
           EVALUATE ARPM-BRANCH-CD
               WHEN 'FN'
                   IF ARPM-LAST-PAY-DATE > 0 AND ARPM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GM' TO ARPM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GM'
                   IF ARPM-LAST-PAY-DATE > 0 AND ARPM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GV' TO ARPM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GV'
                   IF ARPM-LAST-PAY-DATE > 0 AND ARPM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HK' TO ARPM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HK'
                   IF ARPM-LAST-PAY-DATE > 0 AND ARPM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VX' TO ARPM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3070-COUNT-CYCLE.
           IF ARPM-CYCLE-CNT < 9999999
               ADD 1 TO ARPM-CYCLE-CNT
           END-IF
           IF ARPM-CYCLE-CNT NOT < 12
               MOVE 'Y' TO ARPM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ISSUED YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3080-SPLIT-ISSUED.
           COMPUTE WS-WORK-AMT = ARPM-ISSUED-YTD / 2
           COMPUTE WS-WORK-AMT-2 = ARPM-ISSUED-YTD - (WS-WORK-AMT * 2)
           COMPUTE ARPM-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3090-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF ARPT-EFF-DATE > 0 AND ARPT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ARPM-STOP-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD ARPM-STOP-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD ARPM-STOP-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ARPM-STOP-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ARPM-STOP-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3100-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF ARPM-LAST-MAINT-DATE > 0 AND ARPM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ARPT-ISSUED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD ARPT-ISSUED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD ARPT-ISSUED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ARPT-ISSUED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ARPT-ISSUED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * OUTSTAND AMT IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3110-LIMIT-OUTSTAND.
           IF ARPT-OUTSTAND-AMT < 10.00
               MOVE 10.00 TO ARPT-OUTSTAND-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARPT-OUTSTAND-AMT > 500.00
               MOVE 500.00 TO ARPT-OUTSTAND-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STOP AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3120-ACCRUE-STOP.
           IF ARPM-STOP-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPM-STOP-AMT * ARPM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO ARPT-ISSUED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAID YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3130-PRORATE-PAID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPM-PAID-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT FEE YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3140-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = ARPM-FEE-YTD / 3
           COMPUTE WS-WORK-AMT-2 = ARPM-FEE-YTD - (WS-WORK-AMT * 3)
           COMPUTE ARPT-VOID-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * FN -> GM WHEN LAST PAY DATE IS PAST
      * GM -> GV WHEN LAST PAY DATE IS PAST
      * GV -> HK WHEN LAST PAY DATE IS PAST
      * HK -> VX WHEN LAST PAY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3150-SET-BRANCH.
           EVALUATE ARPM-BRANCH-CD
               WHEN 'FN'
                   IF ARPM-LAST-PAY-DATE > 0 AND ARPM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GM' TO ARPM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GM'
                   IF ARPM-LAST-PAY-DATE > 0 AND ARPM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GV' TO ARPM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GV'
                   IF ARPM-LAST-PAY-DATE > 0 AND ARPM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HK' TO ARPM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HK'
                   IF ARPM-LAST-PAY-DATE > 0 AND ARPM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VX' TO ARPM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE AMT BY RECON TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3160-ACCUM-RECON-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARPM-RECON-TYPE-CD
                   ADD ARPT-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARPM-RECON-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ARPT-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAID YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ISSUED AMT.
      *----------------------------------------------------------------*
       3170-PRORATE-ISSUED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPT-ISSUED-AMT ROUNDED =
                   ARPM-PAID-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ARPT-ISSUED-AMT
           END-IF
           ADD ARPT-ISSUED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT VOID AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3180-SPLIT-VOID.
           COMPUTE WS-WORK-AMT = ARPT-VOID-AMT / 2
           COMPUTE WS-WORK-AMT-2 = ARPT-VOID-AMT - (WS-WORK-AMT * 2)
           COMPUTE ARPT-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE OUTSTAND YTD FEE ON THE ISSUED AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3190-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN ARPM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO ARPM-OUTSTAND-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARPT-REVERSAL-FLAG = 'Y'
                   COMPUTE ARPM-OUTSTAND-YTD ROUNDED = ARPM-ISSUED-AMT *
                       ARPM-PENALTY-PCT / 200
               WHEN ARPM-BRANCH-CD = 'FN'
                   MOVE 12.00 TO ARPM-OUTSTAND-YTD
               WHEN ARPM-BRANCH-CD = 'GM'
                   MOVE 15.00 TO ARPM-OUTSTAND-YTD
               WHEN OTHER
                   COMPUTE ARPM-OUTSTAND-YTD ROUNDED = ARPM-ISSUED-AMT *
                       ARPM-PENALTY-PCT / 100
           END-EVALUATE
           IF ARPM-OUTSTAND-YTD > 0 AND ARPM-OUTSTAND-YTD < 2.00
               MOVE 2.00 TO ARPM-OUTSTAND-YTD
           END-IF
           IF ARPM-OUTSTAND-YTD > 50.00
               MOVE 50.00 TO ARPM-OUTSTAND-YTD
           END-IF
           ADD ARPM-OUTSTAND-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE PAID TIER TIER FOR THE ISSUED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3200-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAID-TIER-RATE(1) TO ARPM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF ARPM-ISSUED-YTD NOT < WS-PAID-TIER-LOW(WS-TX)
                   MOVE WS-PAID-TIER-RATE(WS-TX) TO ARPM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3210-DESCRIBE-BRANCH.
           EVALUATE ARPM-BRANCH-CD
               WHEN 'FN'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'GM'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'GV'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'HK'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'VX'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STOP AMT IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3220-LIMIT-STOP.
           IF ARPM-STOP-AMT < 100.00
               MOVE 100.00 TO ARPM-STOP-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARPM-STOP-AMT > 5000.00
               MOVE 5000.00 TO ARPM-STOP-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3230-ACCRUE-FEE.
           IF ARPM-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPM-FEE-AMT * ARPM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO ARPT-ISSUED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3240-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF ARPT-POST-DATE > 0 AND ARPT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ARPM-OUTSTAND-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ARPM-OUTSTAND-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD ARPM-OUTSTAND-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD ARPM-OUTSTAND-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ARPM-OUTSTAND-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 45 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3250-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF ARPM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO ARPM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * OUTSTAND AMT IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3260-LIMIT-OUTSTAND.
           IF ARPM-OUTSTAND-AMT < 10.00
               MOVE 10.00 TO ARPM-OUTSTAND-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARPM-OUTSTAND-AMT > 2500.00
               MOVE 2500.00 TO ARPM-OUTSTAND-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 45 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3270-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF ARPM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO ARPM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E145 MEMO REQUIRED
      * E194 VOID YTD NEGATIVE
      * E704 VOID YTD OVER 1000000
      * E811 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3280-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPT-MEMO = SPACES
                   MOVE 'E145' TO WS-REJECT-CODE
               WHEN ARPM-VOID-YTD < 0
                   MOVE 'E194' TO WS-REJECT-CODE
               WHEN ARPM-VOID-YTD > 1000000.00
                   MOVE 'E704' TO WS-REJECT-CODE
               WHEN ARPT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E811' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE FEE YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3290-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN ARPM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARPM-EXEMPT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = ARPM-FEE-YTD *
                       ARPM-DISCOUNT / 200
               WHEN ARPM-RECON-TYPE-CD = 'EEH'
                   MOVE 15.00 TO WS-WORK-AMT-2
               WHEN ARPM-RECON-TYPE-CD = 'KBS'
                   MOVE 20.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = ARPM-FEE-YTD *
                       ARPM-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 1.00
               MOVE 1.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 15 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3300-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF ARPT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO ARPT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE OUTSTAND AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO STOP YTD.
      *----------------------------------------------------------------*
       3310-PRORATE-STOP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPM-STOP-YTD ROUNDED =
                   ARPT-OUTSTAND-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ARPM-STOP-YTD
           END-IF
           ADD ARPM-STOP-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FEE YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3320-FLAG-FEE.
           IF ARPM-FEE-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ARPM-FEE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3330-COUNT-PAYMENT.
           IF ARPM-PAYMENT-CNT < 9999999
               ADD 1 TO ARPM-PAYMENT-CNT
           END-IF
           IF ARPM-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO ARPM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3340-COUNT-SEQ.
           IF ARPT-SEQ < 9999999
               ADD 1 TO ARPT-SEQ
           END-IF
           IF ARPT-SEQ NOT < 10
               MOVE 'Y' TO ARPM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 120 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3350-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF ARPM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO ARPM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 60 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3360-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF ARPM-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO ARPM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3370-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF ARPM-EFFECTIVE-DATE > 0 AND ARPM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 90 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3380-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF ARPT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO ARPM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * OUTSTAND YTD IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3390-LIMIT-OUTSTAND.
           IF ARPM-OUTSTAND-YTD < 0.00
               MOVE 0.00 TO ARPM-OUTSTAND-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARPM-OUTSTAND-YTD > 500.00
               MOVE 500.00 TO ARPM-OUTSTAND-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * GW -> MB WHEN LAST ACT DATE IS PAST
      * MB -> MH WHEN LAST ACT DATE IS PAST
      * MH -> UG WHEN LAST ACT DATE IS PAST
      * UG -> UR WHEN LAST ACT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3400-SET-SOURCE.
           EVALUATE ARPM-SOURCE-CD
               WHEN 'GW'
                   IF ARPM-LAST-ACT-DATE > 0 AND ARPM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MB' TO ARPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MB'
                   IF ARPM-LAST-ACT-DATE > 0 AND ARPM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MH' TO ARPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MH'
                   IF ARPM-LAST-ACT-DATE > 0 AND ARPM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UG' TO ARPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UG'
                   IF ARPM-LAST-ACT-DATE > 0 AND ARPM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UR' TO ARPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE FEE YTD FEE ON THE VOID AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3410-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN ARPM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO ARPM-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARPM-HOLD-FLAG = 'Y'
                   COMPUTE ARPM-FEE-YTD ROUNDED = ARPM-VOID-AMT *
                       ARPM-DISCOUNT / 200
               WHEN ARPM-CYCLE-CD = 'CP'
                   MOVE 5.00 TO ARPM-FEE-YTD
               WHEN ARPM-CYCLE-CD = 'MR'
                   MOVE 3.00 TO ARPM-FEE-YTD
               WHEN OTHER
                   COMPUTE ARPM-FEE-YTD ROUNDED = ARPM-VOID-AMT *
                       ARPM-DISCOUNT / 100
           END-EVALUATE
           IF ARPM-FEE-YTD > 0 AND ARPM-FEE-YTD < 10.00
               MOVE 10.00 TO ARPM-FEE-YTD
           END-IF
           IF ARPM-FEE-YTD > 25.00
               MOVE 25.00 TO ARPM-FEE-YTD
           END-IF
           ADD ARPM-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E540 CUST ID REQUIRED
      * E908 ISSUED AMT NEGATIVE
      * E009 ISSUED AMT OVER 25000
      * E627 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3420-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPM-CUST-ID = SPACES
                   MOVE 'E540' TO WS-REJECT-CODE
               WHEN ARPT-ISSUED-AMT < 0
                   MOVE 'E908' TO WS-REJECT-CODE
               WHEN ARPT-ISSUED-AMT > 25000.00
                   MOVE 'E009' TO WS-REJECT-CODE
               WHEN ARPM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E627' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FEE AMT.
      *----------------------------------------------------------------*
       3430-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPM-FEE-AMT ROUNDED =
                   ARPT-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ARPM-FEE-AMT
           END-IF
           ADD ARPM-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E493 CONTACT REQUIRED
      * E343 OUTSTAND AMT NEGATIVE
      * E454 OUTSTAND AMT OVER 25000
      * E043 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3440-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPM-CONTACT = SPACES
                   MOVE 'E493' TO WS-REJECT-CODE
               WHEN ARPT-OUTSTAND-AMT < 0
                   MOVE 'E343' TO WS-REJECT-CODE
               WHEN ARPT-OUTSTAND-AMT > 25000.00
                   MOVE 'E454' TO WS-REJECT-CODE
               WHEN ARPM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E043' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE OUTSTAND TIER TIER FOR THE OUTSTAND AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3450-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-OUTSTAND-TIER-RATE(1) TO ARPM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF ARPM-OUTSTAND-AMT NOT < WS-OUTSTAND-TIER-LOW(WS-TX)
                   MOVE WS-OUTSTAND-TIER-RATE(WS-TX) TO ARPM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E330 REJECT CD REQUIRED
      * E457 ISSUED AMT NEGATIVE
      * E280 ISSUED AMT OVER 25000
      * E253 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3460-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPT-REJECT-CD = SPACES
                   MOVE 'E330' TO WS-REJECT-CODE
               WHEN ARPT-ISSUED-AMT < 0
                   MOVE 'E457' TO WS-REJECT-CODE
               WHEN ARPT-ISSUED-AMT > 25000.00
                   MOVE 'E280' TO WS-REJECT-CODE
               WHEN ARPM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E253' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAID YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FEE AMT.
      *----------------------------------------------------------------*
       3470-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPT-FEE-AMT ROUNDED =
                   ARPM-PAID-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ARPT-FEE-AMT
           END-IF
           ADD ARPT-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3480-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 0.00
               MOVE 0.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 5000.00
               MOVE 5000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STOP YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3490-ACCRUE-STOP.
           IF ARPM-STOP-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPM-STOP-YTD * ARPM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO ARPM-VOID-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT OUTSTAND AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3500-SPLIT-OUTSTAND.
           COMPUTE WS-WORK-AMT = ARPM-OUTSTAND-AMT / 3
           COMPUTE WS-WORK-AMT-2 = ARPM-OUTSTAND-AMT - (WS-WORK-AMT * 3)
           COMPUTE ARPM-OUTSTAND-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE OUTSTAND AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3510-PRORATE-OUTSTAND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPT-OUTSTAND-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3520-DESCRIBE-CHANNEL.
           EVALUATE ARPT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT ISSUED AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3530-SPLIT-ISSUED.
           COMPUTE WS-WORK-AMT = ARPT-ISSUED-AMT / 4
           COMPUTE WS-WORK-AMT-2 = ARPT-ISSUED-AMT - (WS-WORK-AMT * 4)
           COMPUTE ARPM-PAID-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3540-DESCRIBE-BRANCH.
           EVALUATE ARPM-BRANCH-CD
               WHEN 'FN'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'GM'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'GV'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'HK'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'VX'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE ISSUED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ISSUED AMT.
      *----------------------------------------------------------------*
       3550-PRORATE-ISSUED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPM-ISSUED-AMT ROUNDED =
                   ARPT-ISSUED-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ARPM-ISSUED-AMT
           END-IF
           ADD ARPM-ISSUED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE ISSUED AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3560-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARPT-TRAN-CD
                   ADD ARPT-ISSUED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARPT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ARPT-ISSUED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ISSUED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PAID YTD.
      *----------------------------------------------------------------*
       3570-PRORATE-PAID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPM-PAID-YTD ROUNDED =
                   ARPM-ISSUED-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ARPM-PAID-YTD
           END-IF
           ADD ARPM-PAID-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ISSUED AMT IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3580-LIMIT-ISSUED.
           IF ARPT-ISSUED-AMT < 25.00
               MOVE 25.00 TO ARPT-ISSUED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARPT-ISSUED-AMT > 5000.00
               MOVE 5000.00 TO ARPT-ISSUED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAID TIER TIER FOR THE PAID YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3590-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-PAID-TIER-RATE(1) TO ARPM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF ARPM-PAID-YTD NOT < WS-PAID-TIER-LOW(WS-TX)
                   MOVE WS-PAID-TIER-RATE(WS-TX) TO ARPM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ISSUED YTD FEE ON THE ISSUED AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3600-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN ARPT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO ARPM-ISSUED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARPM-OVERRIDE-FLAG = 'Y'
                   COMPUTE ARPM-ISSUED-YTD ROUNDED = ARPT-ISSUED-AMT *
                       ARPM-DISCOUNT / 200
               WHEN ARPM-CYCLE-CD = 'CP'
                   MOVE 5.00 TO ARPM-ISSUED-YTD
               WHEN ARPM-CYCLE-CD = 'MR'
                   MOVE 15.00 TO ARPM-ISSUED-YTD
               WHEN OTHER
                   COMPUTE ARPM-ISSUED-YTD ROUNDED = ARPT-ISSUED-AMT *
                       ARPM-DISCOUNT / 100
           END-EVALUATE
           IF ARPM-ISSUED-YTD > 0 AND ARPM-ISSUED-YTD < 1.00
               MOVE 1.00 TO ARPM-ISSUED-YTD
           END-IF
           IF ARPM-ISSUED-YTD > 25.00
               MOVE 25.00 TO ARPM-ISSUED-YTD
           END-IF
           ADD ARPM-ISSUED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FEE AMT.
      *----------------------------------------------------------------*
       3610-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPT-FEE-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ARPT-FEE-AMT
           END-IF
           ADD ARPT-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE PAID YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3620-PRORATE-PAID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPM-PAID-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT OUTSTAND AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3630-SPLIT-OUTSTAND.
           COMPUTE WS-WORK-AMT = ARPT-OUTSTAND-AMT / 2
           COMPUTE WS-WORK-AMT-2 = ARPT-OUTSTAND-AMT - (WS-WORK-AMT * 2)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAID TIER TIER FOR THE STOP YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3640-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAID-TIER-RATE(1) TO ARPM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF ARPM-STOP-YTD NOT < WS-PAID-TIER-LOW(WS-TX)
                   MOVE WS-PAID-TIER-RATE(WS-TX) TO ARPM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ISSUED AMT.
      *----------------------------------------------------------------*
       3650-PRORATE-ISSUED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPM-ISSUED-AMT ROUNDED =
                   ARPM-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ARPM-ISSUED-AMT
           END-IF
           ADD ARPM-ISSUED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3660-COUNT-CYCLE.
           IF ARPM-CYCLE-CNT < 9999999
               ADD 1 TO ARPM-CYCLE-CNT
           END-IF
           IF ARPM-CYCLE-CNT NOT < 12
               MOVE 'Y' TO ARPM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E982 REJECT CD REQUIRED
      * E843 FEE AMT NEGATIVE
      * E425 FEE AMT OVER 50000
      * E033 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3670-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPT-REJECT-CD = SPACES
                   MOVE 'E982' TO WS-REJECT-CODE
               WHEN ARPT-FEE-AMT < 0
                   MOVE 'E843' TO WS-REJECT-CODE
               WHEN ARPT-FEE-AMT > 50000.00
                   MOVE 'E425' TO WS-REJECT-CODE
               WHEN ARPM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E033' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * VOID AMT IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3680-LIMIT-VOID.
           IF ARPT-VOID-AMT < 0.00
               MOVE 0.00 TO ARPT-VOID-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARPT-VOID-AMT > 500.00
               MOVE 500.00 TO ARPT-VOID-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E683 DESC REQUIRED
      * E626 VOID AMT NEGATIVE
      * E696 VOID AMT OVER 250000
      * E496 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3690-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPM-DESC = SPACES
                   MOVE 'E683' TO WS-REJECT-CODE
               WHEN ARPT-VOID-AMT < 0
                   MOVE 'E626' TO WS-REJECT-CODE
               WHEN ARPT-VOID-AMT > 250000.00
                   MOVE 'E696' TO WS-REJECT-CODE
               WHEN ARPM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E496' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE VOID YTD FEE ON THE WORK AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3700-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN ARPM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO ARPM-VOID-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARPM-EXEMPT-FLAG = 'Y'
                   COMPUTE ARPM-VOID-YTD ROUNDED = WS-WORK-AMT *
                       ARPM-FEE-PCT / 200
               WHEN ARPT-TRAN-CD = 'IS'
                   MOVE 12.00 TO ARPM-VOID-YTD
               WHEN ARPT-TRAN-CD = 'PD'
                   MOVE 12.00 TO ARPM-VOID-YTD
               WHEN OTHER
                   COMPUTE ARPM-VOID-YTD ROUNDED = WS-WORK-AMT *
                       ARPM-FEE-PCT / 100
           END-EVALUATE
           IF ARPM-VOID-YTD > 0 AND ARPM-VOID-YTD < 1.00
               MOVE 1.00 TO ARPM-VOID-YTD
           END-IF
           IF ARPM-VOID-YTD > 150.00
               MOVE 150.00 TO ARPM-VOID-YTD
           END-IF
           ADD ARPM-VOID-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3710-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF ARPM-CLOSE-DATE > 0 AND ARPM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ARPT-OUTSTAND-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD ARPT-OUTSTAND-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD ARPT-OUTSTAND-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD ARPT-OUTSTAND-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ARPT-OUTSTAND-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * AD -> BU WHEN EFFECTIVE DATE IS PAST
      * BU -> GM WHEN EFFECTIVE DATE IS PAST
      * GM -> SW WHEN EFFECTIVE DATE IS PAST
      * SW -> WD WHEN EFFECTIVE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3720-SET-REGION.
           EVALUATE ARPM-REGION-CD
               WHEN 'AD'
                   IF ARPM-EFFECTIVE-DATE > 0 AND ARPM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BU' TO ARPM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BU'
                   IF ARPM-EFFECTIVE-DATE > 0 AND ARPM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GM' TO ARPM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GM'
                   IF ARPM-EFFECTIVE-DATE > 0 AND ARPM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SW' TO ARPM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SW'
                   IF ARPM-EFFECTIVE-DATE > 0 AND ARPM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WD' TO ARPM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE OUTSTAND TIER TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3730-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-OUTSTAND-TIER-RATE(1) TO ARPM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF ARPM-FEE-AMT NOT < WS-OUTSTAND-TIER-LOW(WS-TX)
                   MOVE WS-OUTSTAND-TIER-RATE(WS-TX) TO ARPM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3740-DESCRIBE-BRANCH.
           EVALUATE ARPM-BRANCH-CD
               WHEN 'FN'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'GM'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'GV'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'HK'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'VX'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E056 CONTACT REQUIRED
      * E793 PAID AMT NEGATIVE
      * E564 PAID AMT OVER 99999
      * E245 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3750-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPM-CONTACT = SPACES
                   MOVE 'E056' TO WS-REJECT-CODE
               WHEN ARPM-PAID-AMT < 0
                   MOVE 'E793' TO WS-REJECT-CODE
               WHEN ARPM-PAID-AMT > 99999.00
                   MOVE 'E564' TO WS-REJECT-CODE
               WHEN ARPM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E245' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN LAST ACT DATE IS PAST
      * BAT -> MAL WHEN LAST ACT DATE IS PAST
      * MAL -> TEL WHEN LAST ACT DATE IS PAST
      * TEL -> ONL WHEN LAST ACT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3760-SET-CHANNEL.
           EVALUATE ARPT-CHANNEL-CD
               WHEN 'BRN'
                   IF ARPM-LAST-ACT-DATE > 0 AND ARPM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO ARPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF ARPM-LAST-ACT-DATE > 0 AND ARPM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO ARPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF ARPM-LAST-ACT-DATE > 0 AND ARPM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO ARPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF ARPM-LAST-ACT-DATE > 0 AND ARPM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO ARPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3770-DESCRIBE-REGION.
           EVALUATE ARPM-REGION-CD
               WHEN 'AD'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'BU'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'GM'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'SW'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'WD'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'XV'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * AD -> BU WHEN OPEN DATE IS PAST
      * BU -> GM WHEN OPEN DATE IS PAST
      * GM -> SW WHEN OPEN DATE IS PAST
      * SW -> WD WHEN OPEN DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3780-SET-REGION.
           EVALUATE ARPM-REGION-CD
               WHEN 'AD'
                   IF ARPM-OPEN-DATE > 0 AND ARPM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BU' TO ARPM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BU'
                   IF ARPM-OPEN-DATE > 0 AND ARPM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GM' TO ARPM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GM'
                   IF ARPM-OPEN-DATE > 0 AND ARPM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SW' TO ARPM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SW'
                   IF ARPM-OPEN-DATE > 0 AND ARPM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WD' TO ARPM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3790-DESCRIBE-REGION.
           EVALUATE ARPM-REGION-CD
               WHEN 'AD'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'BU'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'GM'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'SW'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'WD'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'XV'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT PAID AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3800-SPLIT-PAID.
           COMPUTE WS-WORK-AMT = ARPM-PAID-AMT / 12
           COMPUTE WS-WORK-AMT-2 = ARPM-PAID-AMT - (WS-WORK-AMT * 12)
           COMPUTE ARPM-STOP-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3810-DESCRIBE-CHANNEL.
           EVALUATE ARPT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FEE YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3820-FLAG-FEE.
           IF ARPM-FEE-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ARPM-FEE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * GW -> MB WHEN NEXT DUE DATE IS PAST
      * MB -> MH WHEN NEXT DUE DATE IS PAST
      * MH -> UG WHEN NEXT DUE DATE IS PAST
      * UG -> UR WHEN NEXT DUE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3830-SET-SOURCE.
           EVALUATE ARPM-SOURCE-CD
               WHEN 'GW'
                   IF ARPM-NEXT-DUE-DATE > 0 AND ARPM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MB' TO ARPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MB'
                   IF ARPM-NEXT-DUE-DATE > 0 AND ARPM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MH' TO ARPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MH'
                   IF ARPM-NEXT-DUE-DATE > 0 AND ARPM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UG' TO ARPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UG'
                   IF ARPM-NEXT-DUE-DATE > 0 AND ARPM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UR' TO ARPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ISSUED AMT IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3840-LIMIT-ISSUED.
           IF ARPM-ISSUED-AMT < 100.00
               MOVE 100.00 TO ARPM-ISSUED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARPM-ISSUED-AMT > 1000.00
               MOVE 1000.00 TO ARPM-ISSUED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3850-DESCRIBE-CYCLE.
           EVALUATE ARPM-CYCLE-CD
               WHEN 'CP'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'MR'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'PY'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'SP'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'TP'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE FEE YTD FEE ON THE PAID YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3860-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN ARPM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO ARPM-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARPM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE ARPM-FEE-YTD ROUNDED = ARPM-PAID-YTD *
                       ARPM-FEE-PCT / 200
               WHEN ARPM-SOURCE-CD = 'GW'
                   MOVE 12.00 TO ARPM-FEE-YTD
               WHEN ARPM-SOURCE-CD = 'MB'
                   MOVE 12.00 TO ARPM-FEE-YTD
               WHEN OTHER
                   COMPUTE ARPM-FEE-YTD ROUNDED = ARPM-PAID-YTD *
                       ARPM-FEE-PCT / 100
           END-EVALUATE
           IF ARPM-FEE-YTD > 0 AND ARPM-FEE-YTD < 1.00
               MOVE 1.00 TO ARPM-FEE-YTD
           END-IF
           IF ARPM-FEE-YTD > 75.00
               MOVE 75.00 TO ARPM-FEE-YTD
           END-IF
           ADD ARPM-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3870-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 0.00
               MOVE 0.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 10000.00
               MOVE 10000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE STOP AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FEE YTD.
      *----------------------------------------------------------------*
       3880-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPM-FEE-YTD ROUNDED =
                   ARPM-STOP-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ARPM-FEE-YTD
           END-IF
           ADD ARPM-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3890-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF ARPM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO ARPM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAID YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO OUTSTAND AMT.
      *----------------------------------------------------------------*
       3900-PRORATE-OUTSTAND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPT-OUTSTAND-AMT ROUNDED =
                   ARPM-PAID-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ARPT-OUTSTAND-AMT
           END-IF
           ADD ARPT-OUTSTAND-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE PAID TIER TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3910-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAID-TIER-RATE(1) TO ARPM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF ARPT-FEE-AMT NOT < WS-PAID-TIER-LOW(WS-TX)
                   MOVE WS-PAID-TIER-RATE(WS-TX) TO ARPM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE OUTSTAND TIER TIER FOR THE ISSUED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3920-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-OUTSTAND-TIER-RATE(1) TO ARPM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF ARPM-ISSUED-AMT NOT < WS-OUTSTAND-TIER-LOW(WS-TX)
                   MOVE WS-OUTSTAND-TIER-RATE(WS-TX) TO ARPM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAID TIER TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3930-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAID-TIER-RATE(1) TO ARPM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF WS-WORK-AMT NOT < WS-PAID-TIER-LOW(WS-TX)
                   MOVE WS-PAID-TIER-RATE(WS-TX) TO ARPM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON OUTSTAND AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3940-ACCRUE-OUTSTAND.
           IF ARPT-OUTSTAND-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPT-OUTSTAND-AMT * ARPM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO ARPT-VOID-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * GW -> MB WHEN NEXT DUE DATE IS PAST
      * MB -> MH WHEN NEXT DUE DATE IS PAST
      * MH -> UG WHEN NEXT DUE DATE IS PAST
      * UG -> UR WHEN NEXT DUE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3950-SET-SOURCE.
           EVALUATE ARPM-SOURCE-CD
               WHEN 'GW'
                   IF ARPM-NEXT-DUE-DATE > 0 AND ARPM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MB' TO ARPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MB'
                   IF ARPM-NEXT-DUE-DATE > 0 AND ARPM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MH' TO ARPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MH'
                   IF ARPM-NEXT-DUE-DATE > 0 AND ARPM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UG' TO ARPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UG'
                   IF ARPM-NEXT-DUE-DATE > 0 AND ARPM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UR' TO ARPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ISSUED AMT IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3960-LIMIT-ISSUED.
           IF ARPT-ISSUED-AMT < 50.00
               MOVE 50.00 TO ARPT-ISSUED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARPT-ISSUED-AMT > 2500.00
               MOVE 2500.00 TO ARPT-ISSUED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3970-DESCRIBE-CYCLE.
           EVALUATE ARPM-CYCLE-CD
               WHEN 'CP'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'MR'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'PY'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'SP'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'TP'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * VOID AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3980-FLAG-VOID.
           IF ARPM-VOID-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ARPM-VOID-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE OUTSTAND TIER TIER FOR THE OUTSTAND AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3990-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-OUTSTAND-TIER-RATE(1) TO ARPM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF ARPM-OUTSTAND-AMT NOT < WS-OUTSTAND-TIER-LOW(WS-TX)
                   MOVE WS-OUTSTAND-TIER-RATE(WS-TX) TO ARPM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4000-DESCRIBE-REGION.
           EVALUATE ARPM-REGION-CD
               WHEN 'AD'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'BU'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'GM'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'SW'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'WD'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'XV'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STOP YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4010-ACCRUE-STOP.
           IF ARPM-STOP-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPM-STOP-YTD * ARPM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO ARPM-STOP-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4020-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF ARPM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO ARPM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VOID AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4030-ACCRUE-VOID.
           IF ARPM-VOID-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPM-VOID-AMT * ARPM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO ARPM-ISSUED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ISSUED FEE ON THE VOID YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4040-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN ARPT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO ARPM-ISSUED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARPM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE ARPM-ISSUED-AMT ROUNDED = ARPM-VOID-YTD *
                       ARPM-FEE-PCT / 200
               WHEN ARPM-CYCLE-CD = 'CP'
                   MOVE 20.00 TO ARPM-ISSUED-AMT
               WHEN ARPM-CYCLE-CD = 'MR'
                   MOVE 5.00 TO ARPM-ISSUED-AMT
               WHEN OTHER
                   COMPUTE ARPM-ISSUED-AMT ROUNDED = ARPM-VOID-YTD *
                       ARPM-FEE-PCT / 100
           END-EVALUATE
           IF ARPM-ISSUED-AMT > 0 AND ARPM-ISSUED-AMT < 2.00
               MOVE 2.00 TO ARPM-ISSUED-AMT
           END-IF
           IF ARPM-ISSUED-AMT > 100.00
               MOVE 100.00 TO ARPM-ISSUED-AMT
           END-IF
           ADD ARPM-ISSUED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VOID AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4050-ACCRUE-VOID.
           IF ARPT-VOID-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPT-VOID-AMT * ARPM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO ARPT-ISSUED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE VOID AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       4060-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN ARPM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARPM-EXEMPT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = ARPT-VOID-AMT *
                       ARPM-FEE-PCT / 200
               WHEN ARPM-REGION-CD = 'AD'
                   MOVE 10.00 TO WS-WORK-AMT
               WHEN ARPM-REGION-CD = 'BU'
                   MOVE 3.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = ARPT-VOID-AMT *
                       ARPM-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 5.00
               MOVE 5.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 250.00
               MOVE 250.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT PAID AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4070-SPLIT-PAID.
           COMPUTE WS-WORK-AMT = ARPM-PAID-AMT / 12
           COMPUTE WS-WORK-AMT-2 = ARPM-PAID-AMT - (WS-WORK-AMT * 12)
           COMPUTE ARPM-OUTSTAND-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4080-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF ARPM-EFFECTIVE-DATE > 0 AND ARPM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ARPM-ISSUED-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ARPM-ISSUED-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD ARPM-ISSUED-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD ARPM-ISSUED-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ARPM-ISSUED-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 180 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4090-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF ARPM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO ARPM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E555 REF NO REQUIRED
      * E065 OUTSTAND YTD NEGATIVE
      * E518 OUTSTAND YTD OVER 5000
      * E228 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       4100-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPM-REF-NO = SPACES
                   MOVE 'E555' TO WS-REJECT-CODE
               WHEN ARPM-OUTSTAND-YTD < 0
                   MOVE 'E065' TO WS-REJECT-CODE
               WHEN ARPM-OUTSTAND-YTD > 5000.00
                   MOVE 'E518' TO WS-REJECT-CODE
               WHEN ARPM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E228' TO WS-REJECT-CODE
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
       4110-SPLIT-ISSUED.
           COMPUTE WS-WORK-AMT = ARPM-ISSUED-AMT / 2
           COMPUTE WS-WORK-AMT-2 = ARPM-ISSUED-AMT - (WS-WORK-AMT * 2)
           COMPUTE ARPM-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE VOID FEE ON THE ISSUED YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4120-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN ARPM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO ARPM-VOID-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARPM-STOP-FLAG = 'Y'
                   COMPUTE ARPM-VOID-AMT ROUNDED = ARPM-ISSUED-YTD *
                       ARPM-PENALTY-PCT / 200
               WHEN ARPM-RECON-TYPE-CD = 'EEH'
                   MOVE 15.00 TO ARPM-VOID-AMT
               WHEN ARPM-RECON-TYPE-CD = 'KBS'
                   MOVE 15.00 TO ARPM-VOID-AMT
               WHEN OTHER
                   COMPUTE ARPM-VOID-AMT ROUNDED = ARPM-ISSUED-YTD *
                       ARPM-PENALTY-PCT / 100
           END-EVALUATE
           IF ARPM-VOID-AMT > 0 AND ARPM-VOID-AMT < 2.00
               MOVE 2.00 TO ARPM-VOID-AMT
           END-IF
           IF ARPM-VOID-AMT > 75.00
               MOVE 75.00 TO ARPM-VOID-AMT
           END-IF
           ADD ARPM-VOID-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT OUTSTAND YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4130-SPLIT-OUTSTAND.
           COMPUTE WS-WORK-AMT = ARPM-OUTSTAND-YTD / 4
           COMPUTE WS-WORK-AMT-2 = ARPM-OUTSTAND-YTD - (WS-WORK-AMT * 4)
           COMPUTE ARPM-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VOID AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4140-ACCRUE-VOID.
           IF ARPT-VOID-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPT-VOID-AMT * ARPM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO ARPM-STOP-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4150-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF ARPM-NEXT-REVIEW-DATE > 0 AND ARPM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ARPT-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ARPT-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD ARPT-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ARPT-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ARPT-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * FN -> GM WHEN LAST PAY DATE IS PAST
      * GM -> GV WHEN LAST PAY DATE IS PAST
      * GV -> HK WHEN LAST PAY DATE IS PAST
      * HK -> VX WHEN LAST PAY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4160-SET-BRANCH.
           EVALUATE ARPM-BRANCH-CD
               WHEN 'FN'
                   IF ARPM-LAST-PAY-DATE > 0 AND ARPM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GM' TO ARPM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GM'
                   IF ARPM-LAST-PAY-DATE > 0 AND ARPM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GV' TO ARPM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GV'
                   IF ARPM-LAST-PAY-DATE > 0 AND ARPM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HK' TO ARPM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HK'
                   IF ARPM-LAST-PAY-DATE > 0 AND ARPM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VX' TO ARPM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE OUTSTAND TIER TIER FOR THE VOID AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4170-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-OUTSTAND-TIER-RATE(1) TO ARPM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF ARPM-VOID-AMT NOT < WS-OUTSTAND-TIER-LOW(WS-TX)
                   MOVE WS-OUTSTAND-TIER-RATE(WS-TX) TO ARPM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E780 ADDR REQUIRED
      * E868 ISSUED YTD NEGATIVE
      * E395 ISSUED YTD OVER 25000
      * E195 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       4180-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPM-ADDR = SPACES
                   MOVE 'E780' TO WS-REJECT-CODE
               WHEN ARPM-ISSUED-YTD < 0
                   MOVE 'E868' TO WS-REJECT-CODE
               WHEN ARPM-ISSUED-YTD > 25000.00
                   MOVE 'E395' TO WS-REJECT-CODE
               WHEN ARPM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E195' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FEE YTD FEE ON THE OUTSTAND AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4190-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN ARPM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO ARPM-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARPM-PREFERRED-FLAG = 'Y'
                   COMPUTE ARPM-FEE-YTD ROUNDED = ARPT-OUTSTAND-AMT *
                       ARPM-PENALTY-PCT / 200
               WHEN ARPM-SOURCE-CD = 'GW'
                   MOVE 7.00 TO ARPM-FEE-YTD
               WHEN ARPM-SOURCE-CD = 'MB'
                   MOVE 10.00 TO ARPM-FEE-YTD
               WHEN OTHER
                   COMPUTE ARPM-FEE-YTD ROUNDED = ARPT-OUTSTAND-AMT *
                       ARPM-PENALTY-PCT / 100
           END-EVALUATE
           IF ARPM-FEE-YTD > 0 AND ARPM-FEE-YTD < 1.00
               MOVE 1.00 TO ARPM-FEE-YTD
           END-IF
           IF ARPM-FEE-YTD > 100.00
               MOVE 100.00 TO ARPM-FEE-YTD
           END-IF
           ADD ARPM-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE VOID AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO VOID YTD.
      *----------------------------------------------------------------*
       4200-PRORATE-VOID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPM-VOID-YTD ROUNDED =
                   ARPT-VOID-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ARPM-VOID-YTD
           END-IF
           ADD ARPM-VOID-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4210-DESCRIBE-CYCLE.
           EVALUATE ARPM-CYCLE-CD
               WHEN 'CP'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'MR'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'PY'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'SP'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'TP'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE AMT BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4220-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARPM-BRANCH-CD
                   ADD ARPT-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARPM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ARPT-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E314 DESC REQUIRED
      * E850 OUTSTAND YTD NEGATIVE
      * E994 OUTSTAND YTD OVER 250000
      * E054 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       4230-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPM-DESC = SPACES
                   MOVE 'E314' TO WS-REJECT-CODE
               WHEN ARPM-OUTSTAND-YTD < 0
                   MOVE 'E850' TO WS-REJECT-CODE
               WHEN ARPM-OUTSTAND-YTD > 250000.00
                   MOVE 'E994' TO WS-REJECT-CODE
               WHEN ARPM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E054' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4240-DESCRIBE-BRANCH.
           EVALUATE ARPM-BRANCH-CD
               WHEN 'FN'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'GM'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'GV'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'HK'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'VX'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4250-ACCRUE-FEE-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * ARPM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO ARPM-OUTSTAND-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAID YTD IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4260-LIMIT-PAID.
           IF ARPM-PAID-YTD < 0.00
               MOVE 0.00 TO ARPM-PAID-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARPM-PAID-YTD > 10000.00
               MOVE 10000.00 TO ARPM-PAID-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE VOID AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO STOP AMT.
      *----------------------------------------------------------------*
       4270-PRORATE-STOP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPM-STOP-AMT ROUNDED =
                   ARPM-VOID-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ARPM-STOP-AMT
           END-IF
           ADD ARPM-STOP-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT ISSUED YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4280-SPLIT-ISSUED.
           COMPUTE WS-WORK-AMT = ARPM-ISSUED-YTD / 12
           COMPUTE WS-WORK-AMT-2 = ARPM-ISSUED-YTD - (WS-WORK-AMT * 12)
           COMPUTE ARPM-OUTSTAND-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4290-COUNT-SEQ.
           IF ARPT-SEQ < 9999999
               ADD 1 TO ARPT-SEQ
           END-IF
           IF ARPT-SEQ NOT < 10
               MOVE 'Y' TO ARPM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE OUTSTAND TIER TIER FOR THE STOP AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4300-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-OUTSTAND-TIER-RATE(1) TO ARPM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF ARPM-STOP-AMT NOT < WS-OUTSTAND-TIER-LOW(WS-TX)
                   MOVE WS-OUTSTAND-TIER-RATE(WS-TX) TO ARPM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE OUTSTAND TIER TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4310-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-OUTSTAND-TIER-RATE(1) TO ARPM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF ARPM-FEE-AMT NOT < WS-OUTSTAND-TIER-LOW(WS-TX)
                   MOVE WS-OUTSTAND-TIER-RATE(WS-TX) TO ARPM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4320-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = ARPM-FEE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = ARPM-FEE-AMT - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE OUTSTAND TIER TIER FOR THE FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4330-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-OUTSTAND-TIER-RATE(1) TO ARPM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF ARPM-FEE-YTD NOT < WS-OUTSTAND-TIER-LOW(WS-TX)
                   MOVE WS-OUTSTAND-TIER-RATE(WS-TX) TO ARPM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * VOID AMT IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4340-LIMIT-VOID.
           IF ARPM-VOID-AMT < 10.00
               MOVE 10.00 TO ARPM-VOID-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARPM-VOID-AMT > 5000.00
               MOVE 5000.00 TO ARPM-VOID-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E700 OPERATOR REQUIRED
      * E430 STOP YTD NEGATIVE
      * E706 STOP YTD OVER 99999
      * E331 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       4350-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPT-OPERATOR = SPACES
                   MOVE 'E700' TO WS-REJECT-CODE
               WHEN ARPM-STOP-YTD < 0
                   MOVE 'E430' TO WS-REJECT-CODE
               WHEN ARPM-STOP-YTD > 99999.00
                   MOVE 'E706' TO WS-REJECT-CODE
               WHEN ARPM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E331' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4360-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF ARPM-OPEN-DATE > 0 AND ARPM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE ISSUED FEE ON THE PAID YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4370-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN ARPM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO ARPT-ISSUED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARPM-HOLD-FLAG = 'Y'
                   COMPUTE ARPT-ISSUED-AMT ROUNDED = ARPM-PAID-YTD *
                       ARPM-PENALTY-PCT / 200
               WHEN ARPM-SOURCE-CD = 'GW'
                   MOVE 20.00 TO ARPT-ISSUED-AMT
               WHEN ARPM-SOURCE-CD = 'MB'
                   MOVE 12.00 TO ARPT-ISSUED-AMT
               WHEN OTHER
                   COMPUTE ARPT-ISSUED-AMT ROUNDED = ARPM-PAID-YTD *
                       ARPM-PENALTY-PCT / 100
           END-EVALUATE
           IF ARPT-ISSUED-AMT > 0 AND ARPT-ISSUED-AMT < 2.00
               MOVE 2.00 TO ARPT-ISSUED-AMT
           END-IF
           IF ARPT-ISSUED-AMT > 50.00
               MOVE 50.00 TO ARPT-ISSUED-AMT
           END-IF
           ADD ARPT-ISSUED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4380-DESCRIBE-REGION.
           EVALUATE ARPM-REGION-CD
               WHEN 'AD'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'BU'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'GM'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'SW'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'WD'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'XV'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE VOID FEE ON THE FEE AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4390-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN ARPM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO ARPM-VOID-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARPM-STOP-FLAG = 'Y'
                   COMPUTE ARPM-VOID-AMT ROUNDED = ARPT-FEE-AMT *
                       ARPM-PENALTY-PCT / 200
               WHEN ARPM-BRANCH-CD = 'FN'
                   MOVE 12.00 TO ARPM-VOID-AMT
               WHEN ARPM-BRANCH-CD = 'GM'
                   MOVE 15.00 TO ARPM-VOID-AMT
               WHEN OTHER
                   COMPUTE ARPM-VOID-AMT ROUNDED = ARPT-FEE-AMT *
                       ARPM-PENALTY-PCT / 100
           END-EVALUATE
           IF ARPM-VOID-AMT > 0 AND ARPM-VOID-AMT < 1.00
               MOVE 1.00 TO ARPM-VOID-AMT
           END-IF
           IF ARPM-VOID-AMT > 50.00
               MOVE 50.00 TO ARPM-VOID-AMT
           END-IF
           ADD ARPM-VOID-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E071 REF NO REQUIRED
      * E874 ISSUED YTD NEGATIVE
      * E124 ISSUED YTD OVER 25000
      * E976 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       4400-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPM-REF-NO = SPACES
                   MOVE 'E071' TO WS-REJECT-CODE
               WHEN ARPM-ISSUED-YTD < 0
                   MOVE 'E874' TO WS-REJECT-CODE
               WHEN ARPM-ISSUED-YTD > 25000.00
                   MOVE 'E124' TO WS-REJECT-CODE
               WHEN ARPM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E976' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STOP AMT IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4410-LIMIT-STOP.
           IF ARPM-STOP-AMT < 10.00
               MOVE 10.00 TO ARPM-STOP-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARPM-STOP-AMT > 10000.00
               MOVE 10000.00 TO ARPM-STOP-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E505 MEMO REQUIRED
      * E055 VOID AMT NEGATIVE
      * E982 VOID AMT OVER 5000
      * E790 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       4420-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPT-MEMO = SPACES
                   MOVE 'E505' TO WS-REJECT-CODE
               WHEN ARPT-VOID-AMT < 0
                   MOVE 'E055' TO WS-REJECT-CODE
               WHEN ARPT-VOID-AMT > 5000.00
                   MOVE 'E982' TO WS-REJECT-CODE
               WHEN ARPM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E790' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * FN -> GM WHEN NEXT REVIEW DATE IS PAST
      * GM -> GV WHEN NEXT REVIEW DATE IS PAST
      * GV -> HK WHEN NEXT REVIEW DATE IS PAST
      * HK -> VX WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4430-SET-BRANCH.
           EVALUATE ARPM-BRANCH-CD
               WHEN 'FN'
                   IF ARPM-NEXT-REVIEW-DATE > 0 AND
                       ARPM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'GM' TO ARPM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GM'
                   IF ARPM-NEXT-REVIEW-DATE > 0 AND
                       ARPM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'GV' TO ARPM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GV'
                   IF ARPM-NEXT-REVIEW-DATE > 0 AND
                       ARPM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'HK' TO ARPM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HK'
                   IF ARPM-NEXT-REVIEW-DATE > 0 AND
                       ARPM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'VX' TO ARPM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT ISSUED AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4440-SPLIT-ISSUED.
           COMPUTE WS-WORK-AMT = ARPM-ISSUED-AMT / 3
           COMPUTE WS-WORK-AMT-2 = ARPM-ISSUED-AMT - (WS-WORK-AMT * 3)
           COMPUTE ARPM-VOID-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE ARP FIELDS TO ARSTAT AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7010-CALL-ARSTAT.
           INITIALIZE ARSTAT-PARMS
           MOVE ARPM-ISSUED-YTD
             TO ARSTAT-ISSUED-YTD
           MOVE ARPM-FEE-AMT
             TO ARSTAT-FEE-AMT
           MOVE ARPM-OUTSTAND-AMT
             TO ARSTAT-OUTSTAND-AMT
           MOVE ARPM-PAID-AMT
             TO ARSTAT-PAID-AMT
           MOVE ARPM-ISSUED-AMT
             TO ARSTAT-ISSUED-AMT
           MOVE ARPM-OUTSTAND-YTD
             TO ARSTAT-OUTSTAND-YTD
           MOVE ARPM-CLOSE-DATE
             TO ARSTAT-CLOSE-DATE
           MOVE ARPM-LAST-STMT-DATE
             TO ARSTAT-LAST-STMT-DATE
           MOVE ARPM-NEXT-REVIEW-DATE
             TO ARSTAT-NEXT-REVIEW-DATE
           MOVE ARPM-EFFECTIVE-DATE
             TO ARSTAT-EFFECTIVE-DATE
           MOVE ARPM-BRANCH-CD
             TO ARSTAT-BRANCH-CD
           MOVE ARPM-CHANNEL-CD
             TO ARSTAT-CHANNEL-CD
           MOVE ARPM-CYCLE-CD
             TO ARSTAT-CYCLE-CD
           MOVE ARPM-SOURCE-CD
             TO ARSTAT-SOURCE-CD
           MOVE ARPM-NOTICE-SENT-FLAG
             TO ARSTAT-NOTICE-SENT-FLAG
           MOVE ARPM-PREFERRED-FLAG
             TO ARSTAT-PREFERRED-FLAG
           MOVE ARPM-OVERRIDE-FLAG
             TO ARSTAT-OVERRIDE-FLAG
           MOVE ARPM-ITEM-CNT
             TO ARSTAT-ITEM-CNT
           MOVE ARPM-CYCLE-CNT
             TO ARSTAT-CYCLE-CNT
           MOVE ARPM-PENALTY-PCT
             TO ARSTAT-PENALTY-PCT
           MOVE ARPM-DISCOUNT
             TO ARSTAT-DISCOUNT
           MOVE ARPM-CUST-ID
             TO ARSTAT-CUST-ID
           MOVE ARPM-REF-NO
             TO ARSTAT-REF-NO
           CALL 'ARSTAT' USING ARSTAT-PARMS
           EVALUATE ARSTAT-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE ARSTAT-ISSUED-YTD
                     TO ARPM-ISSUED-YTD
                   MOVE ARSTAT-PAID-AMT
                     TO ARPM-PAID-AMT
                   MOVE ARSTAT-ISSUED-AMT
                     TO ARPM-ISSUED-AMT
                   MOVE ARSTAT-PREFERRED-FLAG
                     TO ARPM-PREFERRED-FLAG
                   MOVE ARSTAT-OVERRIDE-FLAG
                     TO ARPM-OVERRIDE-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RTAT' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-ARSTAT' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE ARP FIELDS TO ARRISK AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7030-CALL-ARRISK.
           INITIALIZE ARRISK-PARMS
           MOVE ARPM-ISSUED-YTD
             TO ARRISK-ISSUED-YTD
           MOVE ARPM-PAID-YTD
             TO ARRISK-PAID-YTD
           MOVE ARPM-VOID-YTD
             TO ARRISK-VOID-YTD
           MOVE ARPM-STOP-YTD
             TO ARRISK-STOP-YTD
           MOVE ARPM-ISSUED-AMT
             TO ARRISK-ISSUED-AMT
           MOVE ARPM-OUTSTAND-YTD
             TO ARRISK-OUTSTAND-YTD
           MOVE ARPM-LAST-STMT-DATE
             TO ARRISK-LAST-STMT-DATE
           MOVE ARPM-NEXT-REVIEW-DATE
             TO ARRISK-NEXT-REVIEW-DATE
           MOVE ARPM-LAST-PAY-DATE
             TO ARRISK-LAST-PAY-DATE
           MOVE ARPM-NEXT-DUE-DATE
             TO ARRISK-NEXT-DUE-DATE
           MOVE ARPM-CYCLE-CD
             TO ARRISK-CYCLE-CD
           MOVE ARPM-CHANNEL-CD
             TO ARRISK-CHANNEL-CD
           MOVE ARPM-REGION-CD
             TO ARRISK-REGION-CD
           MOVE ARPM-BRANCH-CD
             TO ARRISK-BRANCH-CD
           MOVE ARPM-NOTICE-SENT-FLAG
             TO ARRISK-NOTICE-SENT-FLAG
           MOVE ARPM-STOP-FLAG
             TO ARRISK-STOP-FLAG
           MOVE ARPM-HOLD-FLAG
             TO ARRISK-HOLD-FLAG
           MOVE ARPM-ITEM-CNT
             TO ARRISK-ITEM-CNT
           MOVE ARPM-CYCLE-CNT
             TO ARRISK-CYCLE-CNT
           MOVE ARPM-PENALTY-PCT
             TO ARRISK-PENALTY-PCT
           MOVE ARPM-DISCOUNT
             TO ARRISK-DISCOUNT
           MOVE ARPM-ADDR
             TO ARRISK-ADDR
           MOVE ARPM-DESC
             TO ARRISK-DESC
           CALL 'ARRISK' USING ARRISK-PARMS
           EVALUATE ARRISK-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE ARRISK-PAID-YTD
                     TO ARPM-PAID-YTD
                   MOVE ARRISK-VOID-YTD
                     TO ARPM-VOID-YTD
                   MOVE ARRISK-STOP-YTD
                     TO ARPM-STOP-YTD
                   MOVE ARRISK-OUTSTAND-YTD
                     TO ARPM-OUTSTAND-YTD
                   MOVE ARRISK-NOTICE-SENT-FLAG
                     TO ARPM-NOTICE-SENT-FLAG
                   MOVE ARRISK-STOP-FLAG
                     TO ARPM-STOP-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RISK' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7030-CALL-ARRISK' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE ARPT-KEY TO RD-KEY
           MOVE ARPT-TRAN-CD TO RD-CODE
           MOVE ARPT-OUTSTAND-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE ARPT-KEY TO RD-KEY
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
           DISPLAY 'ARPACCR READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'ARPACCR' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
