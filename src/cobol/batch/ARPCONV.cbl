      *================================================================*
      * PROGRAM  : ARPCONV                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : ACCOUNT RECONCILEMENT - FILE CONVERSION. READS THE  *
      *            ARP TRANSACTIONS, UPDATES THE ARP MASTER AND        *
      *            PRODUCES THE CONTROL REPORT. FULL AND PARTIAL       *
      *            ACCOUNT RECONCILEMENT SERVICE FOR CORPORATES        *
      * INPUT    : ARPMST (KSDS), ARPTRN, BANKCTL                      *
      * OUTPUT   : ARPOUT, ARPCORPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, ARDTE, ARRISK, ARPRC    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-08-13 RJT  ORIGINAL                                       *
      * 1999-05-26 MKL  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2001-12-09 LMB  PERFORMANCE - VOID READ AHEAD CHG1630          *
      * 2004-09-20 CRP  VOID ROUNDING - HALF UP PRB3217                *
      * 2004-09-11 SKN  VOID CALCULATION CORRECTED REQ9380             *
      * 2005-06-09 RJT  OUTSTAND CALCULATION CORRECTED CHG6516         *
      * 2006-04-25 GAF  ISSUED NOTICE TEXT PER LEGAL PRB9805           *
      * 2014-02-09 JLM  STOP CALCULATION CORRECTED CHG2510             *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ARPCONV.
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
           SELECT REPORT-FILE ASSIGN TO "ARPCORPT"
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
      * WS-OUTSTAND-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-OUTSTAND-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000452000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000675000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020000000000099000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021000000000732000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021050000000477500'.
       01  WS-OUTSTAND-GRID-TBL REDEFINES WS-OUTSTAND-GRID-VALUES.
           05  WS-OUTSTAND-GRID-ROW OCCURS 5 TIMES.
               10  WS-OUTSTAND-GRID-LOW    PIC 9(09)V99.
               10  WS-OUTSTAND-GRID-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-FEE-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FEE-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000777500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000283500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001100000000175500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011100000000758000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011200000000790500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013700000000856000'.
       01  WS-FEE-SCHED-TBL REDEFINES WS-FEE-SCHED-VALUES.
           05  WS-FEE-SCHED-ROW OCCURS 6 TIMES.
               10  WS-FEE-SCHED-LOW        PIC 9(09)V99.
               10  WS-FEE-SCHED-RATE       PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-PAID-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PAID-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000712000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000577000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000344000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000600000000018500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003100000000095500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013100000000877500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013600000000800000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014600000000764000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014850000000542000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015850000000247000'.
       01  WS-PAID-GRID-TBL REDEFINES WS-PAID-GRID-VALUES.
           05  WS-PAID-GRID-ROW OCCURS 10 TIMES.
               10  WS-PAID-GRID-LOW        PIC 9(09)V99.
               10  WS-PAID-GRID-RATE       PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-FEE-SCHEDX - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FEE-SCHEDX-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000332500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000547000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002750000000109000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002800000000891000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003050000000855500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005550000000812000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015550000000355000'.
       01  WS-FEE-SCHEDX-TBL REDEFINES WS-FEE-SCHEDX-VALUES.
           05  WS-FEE-SCHEDX-ROW OCCURS 7 TIMES.
               10  WS-FEE-SCHEDX-LOW       PIC 9(09)V99.
               10  WS-FEE-SCHEDX-RATE      PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  ARPCONV  ACCOUNT RECONCILEMENT'.
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
       COPY ARDTEP.
       COPY ARRISKP.
       COPY ARPRCP.

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
               PERFORM 3000-ACCUM-CYCLE
               PERFORM 3010-DESCRIBE-CHANNEL
               PERFORM 3020-EDIT-CUST-ID
               PERFORM 3030-ACCRUE-OUTSTAND
               PERFORM 3040-PRORATE-PAID
               PERFORM 3050-AGE-OPEN
               PERFORM 3060-LIMIT-STOP
               PERFORM 3070-SPLIT-OUTSTAND
               PERFORM 3080-ASSESS-PREFERRED
               PERFORM 3090-SPLIT-ISSUED
               PERFORM 3100-FLAG-VOID
               PERFORM 3110-EDIT-REF-NO
               PERFORM 3120-DESCRIBE-CHANNEL
               PERFORM 3130-ACCRUE-FEE
               PERFORM 3140-LIMIT-ITEM
               PERFORM 3150-COUNT-CYCLE
               PERFORM 3160-PRORATE-OUTSTAND
               PERFORM 3170-LIMIT-VOID
               PERFORM 3180-DESCRIBE-REGION
               PERFORM 3190-LIMIT-VOID
               PERFORM 3200-SPLIT-ITEM
               PERFORM 3210-ACCRUE-FEE
               PERFORM 3220-SELECT-DISCOUNT
               PERFORM 3230-AGE-EFFECTIVE
               PERFORM 3240-SPLIT-FEE
               PERFORM 3250-ACCRUE-FEE
               PERFORM 3260-DESCRIBE-SOURCE
               PERFORM 3270-SPLIT-VOID
               PERFORM 3280-ASSESS-HOLD
               PERFORM 3290-COUNT-EXCEPT
               PERFORM 3300-FLAG-VOID
               PERFORM 3310-ACCRUE-ISSUED
               PERFORM 3320-SELECT-FEE-PCT
               PERFORM 3330-FLAG-ITEM
               PERFORM 3340-AGE-LAST-STMT
               PERFORM 3350-SPLIT-ISSUED
               PERFORM 3360-COUNT-ITEM
               PERFORM 3370-LIMIT-OUTSTAND
               PERFORM 3380-SELECT-FEE-PCT
               PERFORM 3390-SET-BRANCH
               PERFORM 3400-DESCRIBE-TRAN
               PERFORM 3410-AGE-EFFECTIVE
               PERFORM 3420-LIMIT-ISSUED
               PERFORM 3430-EDIT-REF-NO
               PERFORM 3440-LIMIT-FEE
               PERFORM 3450-ACCUM-SOURCE
               PERFORM 3460-SELECT-PENALTY-PCT
               PERFORM 3470-CHECK-LAST-PAY
               PERFORM 3480-ACCUM-CYCLE
               PERFORM 3490-ACCUM-REGION
               PERFORM 3500-PRORATE-OUTSTAND
               PERFORM 3510-CHECK-EFFECTIVE
               PERFORM 3520-AGE-LAST-PAY
               PERFORM 3530-SELECT-DISCOUNT
               PERFORM 3540-EDIT-CONTACT
               PERFORM 3550-DESCRIBE-TRAN
               PERFORM 3560-DESCRIBE-CYCLE
               PERFORM 3570-ACCRUE-OUTSTAND
               PERFORM 3580-AGE-LAST-STMT
               PERFORM 3590-COUNT-SEQ
               PERFORM 3600-SPLIT-FEE
               PERFORM 3610-CHECK-LAST-ACT
               PERFORM 3620-ACCUM-CHANNEL
               PERFORM 3630-EDIT-NAME
               PERFORM 3640-COUNT-PAYMENT
               PERFORM 3650-COUNT-SEQ
               PERFORM 3660-EDIT-NAME
               PERFORM 3670-ASSESS-OVERRIDE
               PERFORM 3680-CHECK-LAST-MAINT
               PERFORM 3690-SELECT-FEE-PCT
               PERFORM 3700-AGE-LAST-ACT
               PERFORM 3710-FLAG-ITEM
               PERFORM 3720-PRORATE-VOID
               PERFORM 3730-SET-CHANNEL
               PERFORM 3740-PRORATE-VOID
               PERFORM 3750-AGE-LAST-STMT
               PERFORM 3760-SELECT-PENALTY-PCT
               PERFORM 3770-EDIT-REJECT
               PERFORM 3780-SPLIT-OUTSTAND
               PERFORM 3790-ACCRUE-FEE
               PERFORM 3800-SELECT-PENALTY-PCT
               PERFORM 3810-SET-REGION
               PERFORM 3820-AGE-OPEN
               PERFORM 3830-ASSESS-NOTICE-SENT
               PERFORM 3840-DESCRIBE-CHANNEL
               PERFORM 3850-SELECT-DISCOUNT
               PERFORM 3860-ACCRUE-ISSUED
               PERFORM 3870-DESCRIBE-CHANNEL
               PERFORM 3880-AGE-LAST-MAINT
               PERFORM 3890-AGE-EFFECTIVE
               PERFORM 3900-ACCRUE-VOID
               PERFORM 3910-AGE-LAST-PAY
               PERFORM 3920-COUNT-PAYMENT
               PERFORM 3930-SELECT-FEE-PCT
               PERFORM 3940-ASSESS-REVERSAL
               PERFORM 3950-ASSESS-REVERSAL
               PERFORM 3960-FLAG-PAID
               PERFORM 3970-AGE-LAST-ACT
               PERFORM 3980-COUNT-CYCLE
               PERFORM 3990-PRORATE-ISSUED
               PERFORM 4000-CHECK-LAST-ACT
               PERFORM 4010-CHECK-NEXT-DUE
               PERFORM 4020-FLAG-ISSUED
               PERFORM 4030-COUNT-CYCLE
               PERFORM 7020-CALL-ARDTE
               PERFORM 7030-CALL-ARRISK
               PERFORM 7000-CALL-ARPRC
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
      * ACCUMULATE WORK AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3000-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARPM-CYCLE-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARPM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3010-DESCRIBE-CHANNEL.
           EVALUATE ARPM-CHANNEL-CD
               WHEN 'ME'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'SW'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'UA'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'WB'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E447 CUST ID REQUIRED
      * E385 FEE AMT NEGATIVE
      * E450 FEE AMT OVER 250000
      * E049 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3020-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPM-CUST-ID = SPACES
                   MOVE 'E447' TO WS-REJECT-CODE
               WHEN ARPT-FEE-AMT < 0
                   MOVE 'E385' TO WS-REJECT-CODE
               WHEN ARPT-FEE-AMT > 250000.00
                   MOVE 'E450' TO WS-REJECT-CODE
               WHEN ARPM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E049' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON OUTSTAND AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3030-ACCRUE-OUTSTAND.
           IF ARPM-OUTSTAND-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPM-OUTSTAND-AMT * ARPM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO ARPT-OUTSTAND-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE OUTSTAND AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PAID AMT.
      *----------------------------------------------------------------*
       3040-PRORATE-PAID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPM-PAID-AMT ROUNDED =
                   ARPT-OUTSTAND-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ARPM-PAID-AMT
           END-IF
           ADD ARPM-PAID-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3050-AGE-OPEN.
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
                   ADD ARPM-ISSUED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD ARPM-ISSUED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD ARPM-ISSUED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ARPM-ISSUED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ARPM-ISSUED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STOP YTD IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3060-LIMIT-STOP.
           IF ARPM-STOP-YTD < 25.00
               MOVE 25.00 TO ARPM-STOP-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARPM-STOP-YTD > 500.00
               MOVE 500.00 TO ARPM-STOP-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT OUTSTAND YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3070-SPLIT-OUTSTAND.
           COMPUTE WS-WORK-AMT = ARPM-OUTSTAND-YTD / 4
           COMPUTE WS-WORK-AMT-2 = ARPM-OUTSTAND-YTD - (WS-WORK-AMT * 4)
           COMPUTE ARPM-OUTSTAND-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FEE FEE ON THE VOID AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3080-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN ARPM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO ARPM-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARPT-REVERSAL-FLAG = 'Y'
                   COMPUTE ARPM-FEE-AMT ROUNDED = ARPT-VOID-AMT *
                       ARPM-DISCOUNT / 200
               WHEN ARPT-TRAN-CD = 'IS'
                   MOVE 15.00 TO ARPM-FEE-AMT
               WHEN ARPT-TRAN-CD = 'PD'
                   MOVE 10.00 TO ARPM-FEE-AMT
               WHEN OTHER
                   COMPUTE ARPM-FEE-AMT ROUNDED = ARPT-VOID-AMT *
                       ARPM-DISCOUNT / 100
           END-EVALUATE
           IF ARPM-FEE-AMT > 0 AND ARPM-FEE-AMT < 10.00
               MOVE 10.00 TO ARPM-FEE-AMT
           END-IF
           IF ARPM-FEE-AMT > 25.00
               MOVE 25.00 TO ARPM-FEE-AMT
           END-IF
           ADD ARPM-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT ISSUED YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3090-SPLIT-ISSUED.
           COMPUTE WS-WORK-AMT = ARPM-ISSUED-YTD / 3
           COMPUTE WS-WORK-AMT-2 = ARPM-ISSUED-YTD - (WS-WORK-AMT * 3)
           COMPUTE ARPM-VOID-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * VOID YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3100-FLAG-VOID.
           IF ARPM-VOID-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ARPM-VOID-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E792 REF NO REQUIRED
      * E924 STOP YTD NEGATIVE
      * E911 STOP YTD OVER 50000
      * E802 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3110-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPM-REF-NO = SPACES
                   MOVE 'E792' TO WS-REJECT-CODE
               WHEN ARPM-STOP-YTD < 0
                   MOVE 'E924' TO WS-REJECT-CODE
               WHEN ARPM-STOP-YTD > 50000.00
                   MOVE 'E911' TO WS-REJECT-CODE
               WHEN ARPM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E802' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3120-DESCRIBE-CHANNEL.
           EVALUATE ARPT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3130-ACCRUE-FEE.
           IF ARPT-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPT-FEE-AMT * ARPM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3140-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 0.00
               MOVE 0.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 10000.00
               MOVE 10000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3150-COUNT-CYCLE.
           IF ARPM-CYCLE-CNT < 9999999
               ADD 1 TO ARPM-CYCLE-CNT
           END-IF
           IF ARPM-CYCLE-CNT NOT < 12
               MOVE 'Y' TO ARPM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO OUTSTAND AMT.
      *----------------------------------------------------------------*
       3160-PRORATE-OUTSTAND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPT-OUTSTAND-AMT ROUNDED =
                   ARPT-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ARPT-OUTSTAND-AMT
           END-IF
           ADD ARPT-OUTSTAND-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * VOID AMT IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3170-LIMIT-VOID.
           IF ARPT-VOID-AMT < 25.00
               MOVE 25.00 TO ARPT-VOID-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARPT-VOID-AMT > 1000.00
               MOVE 1000.00 TO ARPT-VOID-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3180-DESCRIBE-REGION.
           EVALUATE ARPM-REGION-CD
               WHEN 'AD'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'BU'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'GM'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'SW'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'WD'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'XV'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * VOID AMT IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3190-LIMIT-VOID.
           IF ARPT-VOID-AMT < 25.00
               MOVE 25.00 TO ARPT-VOID-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARPT-VOID-AMT > 500.00
               MOVE 500.00 TO ARPT-VOID-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3200-SPLIT-ITEM.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 2)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3210-ACCRUE-FEE.
           IF ARPT-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPT-FEE-AMT * ARPM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO ARPM-STOP-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAID GRID TIER FOR THE OUTSTAND AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3220-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-PAID-GRID-RATE(1) TO ARPM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF ARPM-OUTSTAND-AMT NOT < WS-PAID-GRID-LOW(WS-TX)
                   MOVE WS-PAID-GRID-RATE(WS-TX) TO ARPM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3230-AGE-EFFECTIVE.
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
                   ADD ARPT-VOID-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ARPT-VOID-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD ARPT-VOID-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ARPT-VOID-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ARPT-VOID-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3240-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = ARPT-FEE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = ARPT-FEE-AMT - (WS-WORK-AMT * 4)
           COMPUTE ARPM-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3250-ACCRUE-FEE.
           IF ARPM-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPM-FEE-AMT * ARPM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO ARPM-STOP-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3260-DESCRIBE-SOURCE.
           EVALUATE ARPM-SOURCE-CD
               WHEN 'GW'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'MB'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'MH'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'UG'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'UR'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT VOID AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3270-SPLIT-VOID.
           COMPUTE WS-WORK-AMT = ARPM-VOID-AMT / 3
           COMPUTE WS-WORK-AMT-2 = ARPM-VOID-AMT - (WS-WORK-AMT * 3)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAID FEE ON THE ISSUED YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3280-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN ARPM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO ARPM-PAID-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARPM-OVERRIDE-FLAG = 'Y'
                   COMPUTE ARPM-PAID-AMT ROUNDED = ARPM-ISSUED-YTD *
                       ARPM-DISCOUNT / 200
               WHEN ARPM-RECON-TYPE-CD = 'EEH'
                   MOVE 7.00 TO ARPM-PAID-AMT
               WHEN ARPM-RECON-TYPE-CD = 'KBS'
                   MOVE 15.00 TO ARPM-PAID-AMT
               WHEN OTHER
                   COMPUTE ARPM-PAID-AMT ROUNDED = ARPM-ISSUED-YTD *
                       ARPM-DISCOUNT / 100
           END-EVALUATE
           IF ARPM-PAID-AMT > 0 AND ARPM-PAID-AMT < 5.00
               MOVE 5.00 TO ARPM-PAID-AMT
           END-IF
           IF ARPM-PAID-AMT > 100.00
               MOVE 100.00 TO ARPM-PAID-AMT
           END-IF
           ADD ARPM-PAID-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3290-COUNT-EXCEPT.
           IF ARPM-EXCEPT-CNT < 9999999
               ADD 1 TO ARPM-EXCEPT-CNT
           END-IF
           IF ARPM-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO ARPM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * VOID YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3300-FLAG-VOID.
           IF ARPM-VOID-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ARPM-VOID-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ISSUED AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3310-ACCRUE-ISSUED.
           IF ARPT-ISSUED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPT-ISSUED-AMT * ARPM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO ARPM-VOID-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE SCHEDX TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3320-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-SCHEDX-RATE(1) TO ARPM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF WS-WORK-AMT-2 NOT < WS-FEE-SCHEDX-LOW(WS-TX)
                   MOVE WS-FEE-SCHEDX-RATE(WS-TX) TO ARPM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3330-FLAG-ITEM.
           IF WS-WORK-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3340-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF ARPM-LAST-STMT-DATE > 0 AND ARPM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ARPM-ISSUED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD ARPM-ISSUED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD ARPM-ISSUED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ARPM-ISSUED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ARPM-ISSUED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3350-SPLIT-ISSUED.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 2)
           COMPUTE ARPM-ISSUED-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3360-COUNT-ITEM.
           IF ARPM-ITEM-CNT < 9999999
               ADD 1 TO ARPM-ITEM-CNT
           END-IF
           IF ARPM-ITEM-CNT NOT < 5
               MOVE 'Y' TO ARPM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * OUTSTAND AMT IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3370-LIMIT-OUTSTAND.
           IF ARPM-OUTSTAND-AMT < 25.00
               MOVE 25.00 TO ARPM-OUTSTAND-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARPM-OUTSTAND-AMT > 50000.00
               MOVE 50000.00 TO ARPM-OUTSTAND-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE SCHED TIER FOR THE STOP AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3380-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-SCHED-RATE(1) TO ARPM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF ARPM-STOP-AMT NOT < WS-FEE-SCHED-LOW(WS-TX)
                   MOVE WS-FEE-SCHED-RATE(WS-TX) TO ARPM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * FN -> GM WHEN LAST STMT DATE IS PAST
      * GM -> GV WHEN LAST STMT DATE IS PAST
      * GV -> HK WHEN LAST STMT DATE IS PAST
      * HK -> VX WHEN LAST STMT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3390-SET-BRANCH.
           EVALUATE ARPM-BRANCH-CD
               WHEN 'FN'
                   IF ARPM-LAST-STMT-DATE > 0 AND ARPM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GM' TO ARPM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GM'
                   IF ARPM-LAST-STMT-DATE > 0 AND ARPM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GV' TO ARPM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GV'
                   IF ARPM-LAST-STMT-DATE > 0 AND ARPM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HK' TO ARPM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HK'
                   IF ARPM-LAST-STMT-DATE > 0 AND ARPM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VX' TO ARPM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3400-DESCRIBE-TRAN.
           EVALUATE ARPT-TRAN-CD
               WHEN 'IS'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'PD'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'SP'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'VD'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'FE'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3410-AGE-EFFECTIVE.
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
                   ADD ARPM-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD ARPM-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD ARPM-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ARPM-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ARPM-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ISSUED YTD IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3420-LIMIT-ISSUED.
           IF ARPM-ISSUED-YTD < 0.00
               MOVE 0.00 TO ARPM-ISSUED-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARPM-ISSUED-YTD > 5000.00
               MOVE 5000.00 TO ARPM-ISSUED-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E488 REF NO REQUIRED
      * E877 FEE AMT NEGATIVE
      * E551 FEE AMT OVER 250000
      * E711 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3430-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPM-REF-NO = SPACES
                   MOVE 'E488' TO WS-REJECT-CODE
               WHEN ARPM-FEE-AMT < 0
                   MOVE 'E877' TO WS-REJECT-CODE
               WHEN ARPM-FEE-AMT > 250000.00
                   MOVE 'E551' TO WS-REJECT-CODE
               WHEN ARPM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E711' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3440-LIMIT-FEE.
           IF ARPM-FEE-AMT < 10.00
               MOVE 10.00 TO ARPM-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ARPM-FEE-AMT > 500.00
               MOVE 500.00 TO ARPM-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ISSUED AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3450-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARPM-SOURCE-CD
                   ADD ARPT-ISSUED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARPM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ARPT-ISSUED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE SCHED TIER FOR THE VOID AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3460-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-SCHED-RATE(1) TO ARPM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF ARPM-VOID-AMT NOT < WS-FEE-SCHED-LOW(WS-TX)
                   MOVE WS-FEE-SCHED-RATE(WS-TX) TO ARPM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 10 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3470-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF ARPM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO ARPM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3480-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARPM-CYCLE-CD
                   ADD ARPM-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARPM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ARPM-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE VOID YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3490-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARPM-REGION-CD
                   ADD ARPM-VOID-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARPM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ARPM-VOID-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE OUTSTAND AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3500-PRORATE-OUTSTAND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPT-OUTSTAND-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3510-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF ARPM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO ARPM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3520-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF ARPM-LAST-PAY-DATE > 0 AND ARPM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ARPM-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD ARPM-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD ARPM-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ARPM-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ARPM-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE FEE SCHED TIER FOR THE ISSUED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3530-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-SCHED-RATE(1) TO ARPM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF ARPM-ISSUED-AMT NOT < WS-FEE-SCHED-LOW(WS-TX)
                   MOVE WS-FEE-SCHED-RATE(WS-TX) TO ARPM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E686 CONTACT REQUIRED
      * E531 OUTSTAND AMT NEGATIVE
      * E624 OUTSTAND AMT OVER 1000000
      * E144 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3540-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPM-CONTACT = SPACES
                   MOVE 'E686' TO WS-REJECT-CODE
               WHEN ARPM-OUTSTAND-AMT < 0
                   MOVE 'E531' TO WS-REJECT-CODE
               WHEN ARPM-OUTSTAND-AMT > 1000000.00
                   MOVE 'E624' TO WS-REJECT-CODE
               WHEN ARPM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E144' TO WS-REJECT-CODE
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
       3550-DESCRIBE-TRAN.
           EVALUATE ARPT-TRAN-CD
               WHEN 'IS'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'PD'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'SP'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'VD'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'FE'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3560-DESCRIBE-CYCLE.
           EVALUATE ARPM-CYCLE-CD
               WHEN 'CP'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'MR'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'PY'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'SP'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'TP'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON OUTSTAND AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3570-ACCRUE-OUTSTAND.
           IF ARPM-OUTSTAND-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPM-OUTSTAND-AMT * ARPM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3580-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF ARPM-LAST-STMT-DATE > 0 AND ARPM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-LAST-STMT-DATE TO DTU-DATE-1
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
               WHEN WS-DAYS NOT > 180
                   ADD ARPM-OUTSTAND-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ARPM-OUTSTAND-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3590-COUNT-SEQ.
           IF ARPT-SEQ < 9999999
               ADD 1 TO ARPT-SEQ
           END-IF
           IF ARPT-SEQ NOT < 6
               MOVE 'Y' TO ARPM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3600-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = ARPM-FEE-YTD / 4
           COMPUTE WS-WORK-AMT-2 = ARPM-FEE-YTD - (WS-WORK-AMT * 4)
           COMPUTE ARPM-ISSUED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 10 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3610-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF ARPM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO ARPT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE VOID AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3620-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ARPM-CHANNEL-CD
                   ADD ARPT-VOID-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ARPM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ARPT-VOID-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E598 NAME REQUIRED
      * E444 ISSUED YTD NEGATIVE
      * E675 ISSUED YTD OVER 250000
      * E376 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3630-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPM-NAME = SPACES
                   MOVE 'E598' TO WS-REJECT-CODE
               WHEN ARPM-ISSUED-YTD < 0
                   MOVE 'E444' TO WS-REJECT-CODE
               WHEN ARPM-ISSUED-YTD > 250000.00
                   MOVE 'E675' TO WS-REJECT-CODE
               WHEN ARPM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E376' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3640-COUNT-PAYMENT.
           IF ARPM-PAYMENT-CNT < 9999999
               ADD 1 TO ARPM-PAYMENT-CNT
           END-IF
           IF ARPM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO ARPM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3650-COUNT-SEQ.
           IF ARPT-SEQ < 9999999
               ADD 1 TO ARPT-SEQ
           END-IF
           IF ARPT-SEQ NOT < 3
               MOVE 'Y' TO ARPM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E412 NAME REQUIRED
      * E847 WORK AMT 2 NEGATIVE
      * E987 WORK AMT 2 OVER 5000
      * E060 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3660-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPM-NAME = SPACES
                   MOVE 'E412' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E847' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 5000.00
                   MOVE 'E987' TO WS-REJECT-CODE
               WHEN ARPM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E060' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE OUTSTAND YTD FEE ON THE STOP AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3670-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN ARPM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO ARPM-OUTSTAND-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARPM-HOLD-FLAG = 'Y'
                   COMPUTE ARPM-OUTSTAND-YTD ROUNDED = ARPM-STOP-AMT *
                       ARPM-PENALTY-PCT / 200
               WHEN ARPM-SOURCE-CD = 'GW'
                   MOVE 20.00 TO ARPM-OUTSTAND-YTD
               WHEN ARPM-SOURCE-CD = 'MB'
                   MOVE 5.00 TO ARPM-OUTSTAND-YTD
               WHEN OTHER
                   COMPUTE ARPM-OUTSTAND-YTD ROUNDED = ARPM-STOP-AMT *
                       ARPM-PENALTY-PCT / 100
           END-EVALUATE
           IF ARPM-OUTSTAND-YTD > 0 AND ARPM-OUTSTAND-YTD < 5.00
               MOVE 5.00 TO ARPM-OUTSTAND-YTD
           END-IF
           IF ARPM-OUTSTAND-YTD > 35.00
               MOVE 35.00 TO ARPM-OUTSTAND-YTD
           END-IF
           ADD ARPM-OUTSTAND-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 45 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3680-CHECK-LAST-MAINT.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO ARPT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE OUTSTAND GRID TIER FOR THE OUTSTAND AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3690-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-OUTSTAND-GRID-RATE(1) TO ARPM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF ARPT-OUTSTAND-AMT NOT < WS-OUTSTAND-GRID-LOW(WS-TX)
                   MOVE WS-OUTSTAND-GRID-RATE(WS-TX) TO ARPM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3700-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF ARPM-LAST-ACT-DATE > 0 AND ARPM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3710-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO VOID AMT.
      *----------------------------------------------------------------*
       3720-PRORATE-VOID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPM-VOID-AMT ROUNDED =
                   ARPT-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ARPM-VOID-AMT
           END-IF
           ADD ARPM-VOID-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * ME -> SW WHEN EFF DATE IS PAST
      * SW -> UA WHEN EFF DATE IS PAST
      * UA -> WB WHEN EFF DATE IS PAST
      * WB -> ME WHEN EFF DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3730-SET-CHANNEL.
           EVALUATE ARPM-CHANNEL-CD
               WHEN 'ME'
                   IF ARPT-EFF-DATE > 0 AND ARPT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SW' TO ARPM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SW'
                   IF ARPT-EFF-DATE > 0 AND ARPT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UA' TO ARPM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UA'
                   IF ARPT-EFF-DATE > 0 AND ARPT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WB' TO ARPM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WB'
                   IF ARPT-EFF-DATE > 0 AND ARPT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ME' TO ARPM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE OUTSTAND AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO VOID YTD.
      *----------------------------------------------------------------*
       3740-PRORATE-VOID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPM-VOID-YTD ROUNDED =
                   ARPT-OUTSTAND-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ARPM-VOID-YTD
           END-IF
           ADD ARPM-VOID-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3750-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF ARPM-LAST-STMT-DATE > 0 AND ARPM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ARPM-PAID-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD ARPM-PAID-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD ARPM-PAID-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD ARPM-PAID-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ARPM-PAID-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE OUTSTAND GRID TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3760-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-OUTSTAND-GRID-RATE(1) TO ARPM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF WS-WORK-AMT-2 NOT < WS-OUTSTAND-GRID-LOW(WS-TX)
                   MOVE WS-OUTSTAND-GRID-RATE(WS-TX) TO ARPM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E618 REJECT CD REQUIRED
      * E914 FEE AMT NEGATIVE
      * E088 FEE AMT OVER 250000
      * E052 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3770-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ARPT-REJECT-CD = SPACES
                   MOVE 'E618' TO WS-REJECT-CODE
               WHEN ARPM-FEE-AMT < 0
                   MOVE 'E914' TO WS-REJECT-CODE
               WHEN ARPM-FEE-AMT > 250000.00
                   MOVE 'E088' TO WS-REJECT-CODE
               WHEN ARPM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E052' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT OUTSTAND AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3780-SPLIT-OUTSTAND.
           COMPUTE WS-WORK-AMT = ARPM-OUTSTAND-AMT / 3
           COMPUTE WS-WORK-AMT-2 = ARPM-OUTSTAND-AMT - (WS-WORK-AMT * 3)
           COMPUTE ARPM-OUTSTAND-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3790-ACCRUE-FEE.
           IF ARPM-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPM-FEE-AMT * ARPM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO ARPM-OUTSTAND-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAID GRID TIER FOR THE PAID YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3800-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAID-GRID-RATE(1) TO ARPM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF ARPM-PAID-YTD NOT < WS-PAID-GRID-LOW(WS-TX)
                   MOVE WS-PAID-GRID-RATE(WS-TX) TO ARPM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * AD -> BU WHEN LAST PAY DATE IS PAST
      * BU -> GM WHEN LAST PAY DATE IS PAST
      * GM -> SW WHEN LAST PAY DATE IS PAST
      * SW -> WD WHEN LAST PAY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3810-SET-REGION.
           EVALUATE ARPM-REGION-CD
               WHEN 'AD'
                   IF ARPM-LAST-PAY-DATE > 0 AND ARPM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BU' TO ARPM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BU'
                   IF ARPM-LAST-PAY-DATE > 0 AND ARPM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GM' TO ARPM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GM'
                   IF ARPM-LAST-PAY-DATE > 0 AND ARPM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SW' TO ARPM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SW'
                   IF ARPM-LAST-PAY-DATE > 0 AND ARPM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WD' TO ARPM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3820-AGE-OPEN.
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
                   ADD ARPT-OUTSTAND-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ARPT-OUTSTAND-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD ARPT-OUTSTAND-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ARPT-OUTSTAND-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ARPT-OUTSTAND-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE FEE YTD FEE ON THE ISSUED AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3830-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN ARPM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO ARPM-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARPM-PREFERRED-FLAG = 'Y'
                   COMPUTE ARPM-FEE-YTD ROUNDED = ARPM-ISSUED-AMT *
                       ARPM-DISCOUNT / 200
               WHEN ARPM-RECON-TYPE-CD = 'EEH'
                   MOVE 3.00 TO ARPM-FEE-YTD
               WHEN ARPM-RECON-TYPE-CD = 'KBS'
                   MOVE 20.00 TO ARPM-FEE-YTD
               WHEN OTHER
                   COMPUTE ARPM-FEE-YTD ROUNDED = ARPM-ISSUED-AMT *
                       ARPM-DISCOUNT / 100
           END-EVALUATE
           IF ARPM-FEE-YTD > 0 AND ARPM-FEE-YTD < 5.00
               MOVE 5.00 TO ARPM-FEE-YTD
           END-IF
           IF ARPM-FEE-YTD > 35.00
               MOVE 35.00 TO ARPM-FEE-YTD
           END-IF
           ADD ARPM-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3840-DESCRIBE-CHANNEL.
           EVALUATE ARPT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE OUTSTAND GRID TIER FOR THE FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3850-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-OUTSTAND-GRID-RATE(1) TO ARPM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF ARPM-FEE-YTD NOT < WS-OUTSTAND-GRID-LOW(WS-TX)
                   MOVE WS-OUTSTAND-GRID-RATE(WS-TX) TO ARPM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ISSUED AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3860-ACCRUE-ISSUED.
           IF ARPT-ISSUED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPT-ISSUED-AMT * ARPM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO ARPM-STOP-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3870-DESCRIBE-CHANNEL.
           EVALUATE ARPM-CHANNEL-CD
               WHEN 'ME'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'SW'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'UA'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'WB'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3880-AGE-LAST-MAINT.
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
                   ADD ARPM-ISSUED-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ARPM-ISSUED-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD ARPM-ISSUED-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD ARPM-ISSUED-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ARPM-ISSUED-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3890-AGE-EFFECTIVE.
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
                   ADD ARPT-OUTSTAND-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD ARPT-OUTSTAND-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD ARPT-OUTSTAND-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ARPT-OUTSTAND-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ARPT-OUTSTAND-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VOID AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3900-ACCRUE-VOID.
           IF ARPT-VOID-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ARPT-VOID-AMT * ARPM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO ARPM-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3910-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF ARPM-LAST-PAY-DATE > 0 AND ARPM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ARPT-ISSUED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD ARPT-ISSUED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD ARPT-ISSUED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD ARPT-ISSUED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ARPT-ISSUED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3920-COUNT-PAYMENT.
           IF ARPM-PAYMENT-CNT < 9999999
               ADD 1 TO ARPM-PAYMENT-CNT
           END-IF
           IF ARPM-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO ARPM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE OUTSTAND GRID TIER FOR THE OUTSTAND AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3930-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-OUTSTAND-GRID-RATE(1) TO ARPM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF ARPT-OUTSTAND-AMT NOT < WS-OUTSTAND-GRID-LOW(WS-TX)
                   MOVE WS-OUTSTAND-GRID-RATE(WS-TX) TO ARPM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ARPM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ARPM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ISSUED FEE ON THE STOP AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3940-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN ARPT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO ARPM-ISSUED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARPM-EXEMPT-FLAG = 'Y'
                   COMPUTE ARPM-ISSUED-AMT ROUNDED = ARPM-STOP-AMT *
                       ARPM-PENALTY-PCT / 200
               WHEN ARPM-SOURCE-CD = 'GW'
                   MOVE 15.00 TO ARPM-ISSUED-AMT
               WHEN ARPM-SOURCE-CD = 'MB'
                   MOVE 7.00 TO ARPM-ISSUED-AMT
               WHEN OTHER
                   COMPUTE ARPM-ISSUED-AMT ROUNDED = ARPM-STOP-AMT *
                       ARPM-PENALTY-PCT / 100
           END-EVALUATE
           IF ARPM-ISSUED-AMT > 0 AND ARPM-ISSUED-AMT < 5.00
               MOVE 5.00 TO ARPM-ISSUED-AMT
           END-IF
           IF ARPM-ISSUED-AMT > 35.00
               MOVE 35.00 TO ARPM-ISSUED-AMT
           END-IF
           ADD ARPM-ISSUED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE VOID FEE ON THE STOP YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3950-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN ARPT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO ARPT-VOID-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ARPM-EXEMPT-FLAG = 'Y'
                   COMPUTE ARPT-VOID-AMT ROUNDED = ARPM-STOP-YTD *
                       ARPM-PENALTY-PCT / 200
               WHEN ARPM-CYCLE-CD = 'CP'
                   MOVE 15.00 TO ARPT-VOID-AMT
               WHEN ARPM-CYCLE-CD = 'MR'
                   MOVE 20.00 TO ARPT-VOID-AMT
               WHEN OTHER
                   COMPUTE ARPT-VOID-AMT ROUNDED = ARPM-STOP-YTD *
                       ARPM-PENALTY-PCT / 100
           END-EVALUATE
           IF ARPT-VOID-AMT > 0 AND ARPT-VOID-AMT < 2.00
               MOVE 2.00 TO ARPT-VOID-AMT
           END-IF
           IF ARPT-VOID-AMT > 100.00
               MOVE 100.00 TO ARPT-VOID-AMT
           END-IF
           ADD ARPT-VOID-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PAID AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3960-FLAG-PAID.
           IF ARPM-PAID-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ARPM-PAID-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3970-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF ARPM-LAST-ACT-DATE > 0 AND ARPM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ARPT-ISSUED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD ARPT-ISSUED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD ARPT-ISSUED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ARPT-ISSUED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ARPT-ISSUED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3980-COUNT-CYCLE.
           IF ARPM-CYCLE-CNT < 9999999
               ADD 1 TO ARPM-CYCLE-CNT
           END-IF
           IF ARPM-CYCLE-CNT NOT < 6
               MOVE 'Y' TO ARPM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE OUTSTAND AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ISSUED AMT.
      *----------------------------------------------------------------*
       3990-PRORATE-ISSUED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ARPT-ISSUED-AMT ROUNDED =
                   ARPT-OUTSTAND-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ARPT-ISSUED-AMT
           END-IF
           ADD ARPT-ISSUED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 60 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4000-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF ARPM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ARPM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO ARPM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4010-CHECK-NEXT-DUE.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO ARPM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ISSUED AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4020-FLAG-ISSUED.
           IF ARPM-ISSUED-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ARPM-ISSUED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       4030-COUNT-CYCLE.
           IF ARPM-CYCLE-CNT < 9999999
               ADD 1 TO ARPM-CYCLE-CNT
           END-IF
           IF ARPM-CYCLE-CNT NOT < 6
               MOVE 'Y' TO ARPM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE ARP FIELDS TO ARDTE AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7020-CALL-ARDTE.
           INITIALIZE ARDTE-PARMS
           MOVE ARPM-FEE-YTD
             TO ARDTE-FEE-YTD
           MOVE ARPM-ISSUED-YTD
             TO ARDTE-ISSUED-YTD
           MOVE ARPM-PAID-YTD
             TO ARDTE-PAID-YTD
           MOVE ARPM-VOID-YTD
             TO ARDTE-VOID-YTD
           MOVE ARPM-FEE-AMT
             TO ARDTE-FEE-AMT
           MOVE ARPM-VOID-AMT
             TO ARDTE-VOID-AMT
           MOVE ARPM-CLOSE-DATE
             TO ARDTE-CLOSE-DATE
           MOVE ARPM-LAST-MAINT-DATE
             TO ARDTE-LAST-MAINT-DATE
           MOVE ARPM-LAST-STMT-DATE
             TO ARDTE-LAST-STMT-DATE
           MOVE ARPM-LAST-PAY-DATE
             TO ARDTE-LAST-PAY-DATE
           MOVE ARPM-SOURCE-CD
             TO ARDTE-SOURCE-CD
           MOVE ARPM-RECON-TYPE-CD
             TO ARDTE-RECON-TYPE-CD
           MOVE ARPM-REGION-CD
             TO ARDTE-REGION-CD
           MOVE ARPM-CYCLE-CD
             TO ARDTE-CYCLE-CD
           MOVE ARPM-EXEMPT-FLAG
             TO ARDTE-EXEMPT-FLAG
           MOVE ARPM-PREFERRED-FLAG
             TO ARDTE-PREFERRED-FLAG
           MOVE ARPM-STOP-FLAG
             TO ARDTE-STOP-FLAG
           MOVE ARPM-ITEM-CNT
             TO ARDTE-ITEM-CNT
           MOVE ARPM-CYCLE-CNT
             TO ARDTE-CYCLE-CNT
           MOVE ARPM-DISCOUNT
             TO ARDTE-DISCOUNT
           MOVE ARPM-PENALTY-PCT
             TO ARDTE-PENALTY-PCT
           MOVE ARPM-CUST-ID
             TO ARDTE-CUST-ID
           MOVE ARPM-NAME
             TO ARDTE-NAME
           CALL 'ARDTE' USING ARDTE-PARMS
           EVALUATE ARDTE-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE ARDTE-FEE-YTD
                     TO ARPM-FEE-YTD
                   MOVE ARDTE-EXEMPT-FLAG
                     TO ARPM-EXEMPT-FLAG
                   MOVE ARDTE-PREFERRED-FLAG
                     TO ARPM-PREFERRED-FLAG
                   MOVE ARDTE-STOP-FLAG
                     TO ARPM-STOP-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RDTE' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-ARDTE' TO ABND-PARAGRAPH
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

      *----------------------------------------------------------------*
      * PASS THE ARP FIELDS TO ARPRC AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7000-CALL-ARPRC.
           INITIALIZE ARPRC-PARMS
           MOVE ARPM-OUTSTAND-YTD
             TO ARPRC-OUTSTAND-YTD
           MOVE ARPM-ISSUED-AMT
             TO ARPRC-ISSUED-AMT
           MOVE ARPM-ISSUED-YTD
             TO ARPRC-ISSUED-YTD
           MOVE ARPM-FEE-YTD
             TO ARPRC-FEE-YTD
           MOVE ARPM-OUTSTAND-AMT
             TO ARPRC-OUTSTAND-AMT
           MOVE ARPM-STOP-YTD
             TO ARPRC-STOP-YTD
           MOVE ARPM-OPEN-DATE
             TO ARPRC-OPEN-DATE
           MOVE ARPM-NEXT-DUE-DATE
             TO ARPRC-NEXT-DUE-DATE
           MOVE ARPM-NEXT-REVIEW-DATE
             TO ARPRC-NEXT-REVIEW-DATE
           MOVE ARPM-LAST-ACT-DATE
             TO ARPRC-LAST-ACT-DATE
           MOVE ARPM-RECON-TYPE-CD
             TO ARPRC-RECON-TYPE-CD
           MOVE ARPM-BRANCH-CD
             TO ARPRC-BRANCH-CD
           MOVE ARPM-CHANNEL-CD
             TO ARPRC-CHANNEL-CD
           MOVE ARPM-SOURCE-CD
             TO ARPRC-SOURCE-CD
           MOVE ARPM-HOLD-FLAG
             TO ARPRC-HOLD-FLAG
           MOVE ARPM-STOP-FLAG
             TO ARPRC-STOP-FLAG
           MOVE ARPM-PREFERRED-FLAG
             TO ARPRC-PREFERRED-FLAG
           MOVE ARPM-PAYMENT-CNT
             TO ARPRC-PAYMENT-CNT
           MOVE ARPM-EXCEPT-CNT
             TO ARPRC-EXCEPT-CNT
           MOVE ARPM-DISCOUNT
             TO ARPRC-DISCOUNT
           MOVE ARPM-PENALTY-PCT
             TO ARPRC-PENALTY-PCT
           MOVE ARPM-NAME
             TO ARPRC-NAME
           MOVE ARPM-ADDR
             TO ARPRC-ADDR
           CALL 'ARPRC' USING ARPRC-PARMS
           EVALUATE ARPRC-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE ARPRC-OUTSTAND-YTD
                     TO ARPM-OUTSTAND-YTD
                   MOVE ARPRC-ISSUED-YTD
                     TO ARPM-ISSUED-YTD
                   MOVE ARPRC-FEE-YTD
                     TO ARPM-FEE-YTD
                   MOVE ARPRC-HOLD-FLAG
                     TO ARPM-HOLD-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RPRC' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-ARPRC' TO ABND-PARAGRAPH
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
           DISPLAY 'ARPCONV READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'ARPCONV' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
