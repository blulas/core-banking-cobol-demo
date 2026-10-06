      *================================================================*
      * PROGRAM  : PPYEDIT                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : POSITIVE PAY - INPUT EDIT. READS THE PPY            *
      *            TRANSACTIONS, UPDATES THE PPY MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. CORPORATE POSITIVE PAY ISSUE    *
      *            FILE MATCHING AND EXCEPTIONS                        *
      * INPUT    : PPYMST (KSDS), PPYTRN, BANKCTL                      *
      * OUTPUT   : PPYOUT, PPYEDRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, PPLIM, PPFEE, PPSTAT    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-08-12 CRP  ORIGINAL                                       *
      * 1992-10-02 DPS  VOID NOTICE TEXT PER LEGAL CHG5170             *
      * 1993-05-02 DPS  VOID STATUS CODES ADDED PRB8427                *
      * 1993-01-03 BHS  AUDIT FINDING - PAID CONTROL TOTALS CHG6676    *
      * 1994-10-22 SKN  AUDIT FINDING - VOID CONTROL TOTALS CHG1584    *
      * 1996-05-11 SKN  ISSUED NOTICE TEXT PER LEGAL REQ6096           *
      * 1996-06-13 CRP  VOID FEE SCHEDULE UPDATE PRB2005               *
      * 1997-01-25 SKN  PAID LIMIT REVISED INC8070                     *
      * 1997-07-02 EWS  PAID NOTICE TEXT PER LEGAL INC8608             *
      * 1998-12-12 JLM  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2002-11-28 SKN  STALE STATUS CODES ADDED REQ7339               *
      * 2003-05-27 SKN  PERFORMANCE - ISSUED READ AHEAD PRB1437        *
      * 2003-01-28 GAF  ISSUED FEE SCHEDULE UPDATE CHG7760             *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PPYEDIT.
       AUTHOR. D P SHAW.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "PPYMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS PPYM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "PPYTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "PPYOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "PPYEDRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY PPYMST.
       FD  TRAN-FILE.
       COPY PPYTRN.
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
                                           VALUE 25.000000.
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
      * WS-ISSUED-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-ISSUED-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000162500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000652000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000750000000444000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000800000000206000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000900000000292000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000226000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001500000000088000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001600000000012000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006600000000820500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006700000000306000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006800000000381500'.
       01  WS-ISSUED-GRID-TBL REDEFINES WS-ISSUED-GRID-VALUES.
           05  WS-ISSUED-GRID-ROW OCCURS 11 TIMES.
               10  WS-ISSUED-GRID-LOW      PIC 9(09)V99.
               10  WS-ISSUED-GRID-RATE     PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-VOID-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-VOID-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000504500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000061500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000014500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005500000000639500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010500000000042000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020500000000496000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023000000000384000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023500000000658000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00033500000000505000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00034000000000558000'.
       01  WS-VOID-SCHED-TBL REDEFINES WS-VOID-SCHED-VALUES.
           05  WS-VOID-SCHED-ROW OCCURS 10 TIMES.
               10  WS-VOID-SCHED-LOW       PIC 9(09)V99.
               10  WS-VOID-SCHED-RATE      PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-ISSUED-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-ISSUED-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000117000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000340500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005050000000178500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006050000000259000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006550000000098000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006600000000719000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011600000000441000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012100000000559000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022100000000479000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022150000000084500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022400000000202000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00024900000000372500'.
       01  WS-ISSUED-TIER-TBL REDEFINES WS-ISSUED-TIER-VALUES.
           05  WS-ISSUED-TIER-ROW OCCURS 12 TIMES.
               10  WS-ISSUED-TIER-LOW      PIC 9(09)V99.
               10  WS-ISSUED-TIER-RATE     PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-PAID-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PAID-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000283500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000723000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001050000000179000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001150000000375000'.
       01  WS-PAID-TIER-TBL REDEFINES WS-PAID-TIER-VALUES.
           05  WS-PAID-TIER-ROW OCCURS 4 TIMES.
               10  WS-PAID-TIER-LOW        PIC 9(09)V99.
               10  WS-PAID-TIER-RATE       PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  PPYEDIT  POSITIVE PAY'.
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
       COPY PPLIMP.
       COPY PPFEEP.
       COPY PPSTATP.

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
               MOVE 'PPYMST' TO ABND-FILE-NAME
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
           MOVE PPYT-KEY TO PPYM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-DESCRIBE-REGION
               PERFORM 3010-SELECT-MARGIN
               PERFORM 3020-EDIT-REJECT
               PERFORM 3030-SELECT-RATE
               PERFORM 3040-SPLIT-STALE
               PERFORM 3050-AGE-LAST-PAY
               PERFORM 3060-SELECT-MARGIN
               PERFORM 3070-SET-REASON
               PERFORM 3080-EDIT-OPERATOR
               PERFORM 3090-FLAG-PAID
               PERFORM 3100-AGE-NEXT-DUE
               PERFORM 3110-EDIT-CONTACT
               PERFORM 3120-SET-STATUS
               PERFORM 3130-EDIT-CONTACT
               PERFORM 3140-ASSESS-STOP
               PERFORM 3150-AGE-LAST-PAY
               PERFORM 3160-COUNT-CYCLE
               PERFORM 3170-PRORATE-ITEM
               PERFORM 3180-SELECT-MARGIN
               PERFORM 3190-LIMIT-STALE
               PERFORM 3200-FLAG-PAID
               PERFORM 3210-ASSESS-OVERRIDE
               PERFORM 3220-LIMIT-STALE
               PERFORM 3230-LIMIT-EXCEPT
               PERFORM 3240-LIMIT-PAID
               PERFORM 3250-CHECK-LAST-PAY
               PERFORM 3260-EDIT-CUST-ID
               PERFORM 3270-CHECK-NEXT-REVIEW
               PERFORM 3280-SELECT-MARGIN
               PERFORM 3290-SELECT-MARGIN
               PERFORM 3300-EDIT-CONTACT
               PERFORM 3310-COUNT-PAYMENT
               PERFORM 3320-DESCRIBE-REGION
               PERFORM 3330-ACCUM-DECISION
               PERFORM 3340-PRORATE-STALE
               PERFORM 3350-ASSESS-STOP
               PERFORM 3360-ACCUM-CYCLE
               PERFORM 3370-AGE-CLOSE
               PERFORM 3380-FLAG-PAID
               PERFORM 3390-CHECK-EFFECTIVE
               PERFORM 3400-CHECK-EFF
               PERFORM 3410-LIMIT-PAID
               PERFORM 3420-SPLIT-STALE
               PERFORM 3430-SET-CHANNEL
               PERFORM 3440-ACCRUE-ISSUED
               PERFORM 3450-FLAG-ITEM
               PERFORM 3460-LIMIT-VOID
               PERFORM 3470-COUNT-EXCEPT
               PERFORM 3480-COUNT-SEQ
               PERFORM 3490-SET-CYCLE
               PERFORM 3500-COUNT-PAYMENT
               PERFORM 3510-DESCRIBE-REGION
               PERFORM 3520-SET-STATUS
               PERFORM 3530-SPLIT-EXCEPT
               PERFORM 3540-ASSESS-EXEMPT
               PERFORM 3550-SELECT-DISCOUNT
               PERFORM 3560-SELECT-MARGIN
               PERFORM 3570-ACCRUE-PAID
               PERFORM 3580-ACCRUE-EXCEPT
               PERFORM 3590-ACCUM-DECISION
               PERFORM 3600-AGE-NEXT-DUE
               PERFORM 3610-ACCRUE-PAID
               PERFORM 3620-PRORATE-STALE
               PERFORM 3630-AGE-OPEN
               PERFORM 3640-COUNT-PAYMENT
               PERFORM 3650-ACCUM-TRAN
               PERFORM 3660-CHECK-LAST-PAY
               PERFORM 3670-FLAG-STALE
               PERFORM 3680-CHECK-LAST-MAINT
               PERFORM 3690-ACCUM-STATUS
               PERFORM 3700-FLAG-EXCEPT
               PERFORM 3710-SET-STATUS
               PERFORM 3720-EDIT-NAME
               PERFORM 3730-COUNT-SEQ
               PERFORM 3740-SELECT-RATE
               PERFORM 3750-ACCUM-DECISION
               PERFORM 3760-FLAG-EXCEPT
               PERFORM 3770-PRORATE-PAID
               PERFORM 3780-EDIT-OPERATOR
               PERFORM 3790-ACCUM-CHANNEL
               PERFORM 3800-COUNT-SEQ
               PERFORM 3810-ACCRUE-VOID
               PERFORM 3820-SPLIT-STALE
               PERFORM 3830-COUNT-CYCLE
               PERFORM 3840-CHECK-EFF
               PERFORM 3850-CHECK-NEXT-REVIEW
               PERFORM 3860-CHECK-EXPIRY
               PERFORM 3870-AGE-EFFECTIVE
               PERFORM 3880-ASSESS-AUTO-RENEW
               PERFORM 3890-DESCRIBE-REGION
               PERFORM 3900-DESCRIBE-REGION
               PERFORM 3910-ASSESS-EXEMPT
               PERFORM 3920-COUNT-CYCLE
               PERFORM 3930-AGE-EFF
               PERFORM 3940-SELECT-DISCOUNT
               PERFORM 3950-CHECK-LAST-ACT
               PERFORM 3960-FLAG-PAID
               PERFORM 3970-SET-DECISION
               PERFORM 3980-DESCRIBE-DECISION
               PERFORM 3990-FLAG-ISSUED
               PERFORM 4000-FLAG-EXCEPT
               PERFORM 4010-AGE-NEXT-DUE
               PERFORM 4020-ACCRUE-ISSUED
               PERFORM 4030-LIMIT-VOID
               PERFORM 4040-PRORATE-ITEM
               PERFORM 4050-DESCRIBE-CHANNEL
               PERFORM 4060-SET-CHANNEL
               PERFORM 4070-AGE-POST
               PERFORM 4080-PRORATE-VOID
               PERFORM 4090-DESCRIBE-MATCH-RULE
               PERFORM 4100-COUNT-PAYMENT
               PERFORM 4110-DESCRIBE-DECISION
               PERFORM 4120-SPLIT-VOID
               PERFORM 4130-ACCUM-MATCH-RULE
               PERFORM 4140-ASSESS-OVERRIDE
               PERFORM 4150-DESCRIBE-REASON
               PERFORM 4160-COUNT-EXCEPT
               PERFORM 7000-CALL-PPLIM
               PERFORM 7040-CALL-PPFEE
               PERFORM 7030-CALL-PPSTAT
               REWRITE PPY-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'PPYMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM PPY-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3000-DESCRIBE-REGION.
           EVALUATE PPYM-REGION-CD
               WHEN 'A'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'B'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'D'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'H'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'N'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'Y'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE PAID TIER TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3010-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PAID-TIER-RATE(1) TO PPYM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF WS-WORK-AMT NOT < WS-PAID-TIER-LOW(WS-TX)
                   MOVE WS-PAID-TIER-RATE(WS-TX) TO PPYM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPYM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPYM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E543 REJECT CD REQUIRED
      * E657 EXCEPT AMT NEGATIVE
      * E767 EXCEPT AMT OVER 10000
      * E328 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3020-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPYT-REJECT-CD = SPACES
                   MOVE 'E543' TO WS-REJECT-CODE
               WHEN PPYM-EXCEPT-AMT < 0
                   MOVE 'E657' TO WS-REJECT-CODE
               WHEN PPYM-EXCEPT-AMT > 10000.00
                   MOVE 'E767' TO WS-REJECT-CODE
               WHEN PPYM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E328' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE VOID SCHED TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3030-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-VOID-SCHED-RATE(1) TO PPYM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF WS-WORK-AMT-2 NOT < WS-VOID-SCHED-LOW(WS-TX)
                   MOVE WS-VOID-SCHED-RATE(WS-TX) TO PPYM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPYM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPYM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT STALE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3040-SPLIT-STALE.
           COMPUTE WS-WORK-AMT = PPYT-STALE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = PPYT-STALE-AMT - (WS-WORK-AMT * 3)
           COMPUTE PPYM-EXCEPT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3050-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF PPYM-LAST-PAY-DATE > 0 AND PPYM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPYM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PPYM-EXCEPT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PPYM-EXCEPT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD PPYM-EXCEPT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PPYM-EXCEPT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPYM-EXCEPT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE ISSUED TIER TIER FOR THE ISSUED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3060-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-ISSUED-TIER-RATE(1) TO PPYM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF PPYM-ISSUED-YTD NOT < WS-ISSUED-TIER-LOW(WS-TX)
                   MOVE WS-ISSUED-TIER-RATE(WS-TX) TO PPYM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPYM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPYM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * B -> M WHEN EXPIRY DATE IS PAST
      * M -> N WHEN EXPIRY DATE IS PAST
      * N -> U WHEN EXPIRY DATE IS PAST
      * U -> V WHEN EXPIRY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3070-SET-REASON.
           EVALUATE PPYM-REASON-CD
               WHEN 'B'
                   IF PPYM-EXPIRY-DATE > 0 AND PPYM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO PPYM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF PPYM-EXPIRY-DATE > 0 AND PPYM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'N' TO PPYM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'N'
                   IF PPYM-EXPIRY-DATE > 0 AND PPYM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO PPYM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF PPYM-EXPIRY-DATE > 0 AND PPYM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'V' TO PPYM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E035 OPERATOR REQUIRED
      * E594 STALE AMT NEGATIVE
      * E455 STALE AMT OVER 99999
      * E014 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3080-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPYT-OPERATOR = SPACES
                   MOVE 'E035' TO WS-REJECT-CODE
               WHEN PPYM-STALE-AMT < 0
                   MOVE 'E594' TO WS-REJECT-CODE
               WHEN PPYM-STALE-AMT > 99999.00
                   MOVE 'E455' TO WS-REJECT-CODE
               WHEN PPYT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E014' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAID AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3090-FLAG-PAID.
           IF PPYM-PAID-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PPYM-PAID-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3100-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF PPYM-NEXT-DUE-DATE > 0 AND PPYM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPYM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PPYT-VOID-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PPYT-VOID-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD PPYT-VOID-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PPYT-VOID-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPYT-VOID-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E145 CONTACT REQUIRED
      * E258 PAID AMT NEGATIVE
      * E135 PAID AMT OVER 50000
      * E024 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3110-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPYM-CONTACT = SPACES
                   MOVE 'E145' TO WS-REJECT-CODE
               WHEN PPYM-PAID-AMT < 0
                   MOVE 'E258' TO WS-REJECT-CODE
               WHEN PPYM-PAID-AMT > 50000.00
                   MOVE 'E135' TO WS-REJECT-CODE
               WHEN PPYM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E024' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * DR -> TK WHEN CLOSE DATE IS PAST
      * TK -> WX WHEN CLOSE DATE IS PAST
      * WX -> DR WHEN CLOSE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3120-SET-STATUS.
           EVALUATE PPYM-STATUS-CD
               WHEN 'DR'
                   IF PPYM-CLOSE-DATE > 0 AND PPYM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TK' TO PPYM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TK'
                   IF PPYM-CLOSE-DATE > 0 AND PPYM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WX' TO PPYM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WX'
                   IF PPYM-CLOSE-DATE > 0 AND PPYM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DR' TO PPYM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E106 CONTACT REQUIRED
      * E758 VOID AMT NEGATIVE
      * E442 VOID AMT OVER 10000
      * E231 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3130-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPYM-CONTACT = SPACES
                   MOVE 'E106' TO WS-REJECT-CODE
               WHEN PPYT-VOID-AMT < 0
                   MOVE 'E758' TO WS-REJECT-CODE
               WHEN PPYT-VOID-AMT > 10000.00
                   MOVE 'E442' TO WS-REJECT-CODE
               WHEN PPYM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E231' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ISSUED FEE ON THE PAID AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3140-ASSESS-STOP.
           EVALUATE TRUE
               WHEN PPYM-STOP-FLAG = 'Y'
                   MOVE ZERO TO PPYM-ISSUED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE PPYM-ISSUED-AMT ROUNDED = PPYM-PAID-AMT *
                       PPYM-RATE / 200
               WHEN PPYT-TRAN-CD = 'IS'
                   MOVE 10.00 TO PPYM-ISSUED-AMT
               WHEN PPYT-TRAN-CD = 'PD'
                   MOVE 15.00 TO PPYM-ISSUED-AMT
               WHEN OTHER
                   COMPUTE PPYM-ISSUED-AMT ROUNDED = PPYM-PAID-AMT *
                       PPYM-RATE / 100
           END-EVALUATE
           IF PPYM-ISSUED-AMT > 0 AND PPYM-ISSUED-AMT < 10.00
               MOVE 10.00 TO PPYM-ISSUED-AMT
           END-IF
           IF PPYM-ISSUED-AMT > 150.00
               MOVE 150.00 TO PPYM-ISSUED-AMT
           END-IF
           ADD PPYM-ISSUED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3150-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF PPYM-LAST-PAY-DATE > 0 AND PPYM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPYM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PPYT-ISSUED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD PPYT-ISSUED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD PPYT-ISSUED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD PPYT-ISSUED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPYT-ISSUED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3160-COUNT-CYCLE.
           IF PPYM-CYCLE-CNT < 9999999
               ADD 1 TO PPYM-CYCLE-CNT
           END-IF
           IF PPYM-CYCLE-CNT NOT < 12
               MOVE 'Y' TO PPYM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3170-PRORATE-ITEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE ISSUED TIER TIER FOR THE STALE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3180-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-ISSUED-TIER-RATE(1) TO PPYM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF PPYT-STALE-AMT NOT < WS-ISSUED-TIER-LOW(WS-TX)
                   MOVE WS-ISSUED-TIER-RATE(WS-TX) TO PPYM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPYM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPYM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STALE AMT IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3190-LIMIT-STALE.
           IF PPYM-STALE-AMT < 50.00
               MOVE 50.00 TO PPYM-STALE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PPYM-STALE-AMT > 2500.00
               MOVE 2500.00 TO PPYM-STALE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAID AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3200-FLAG-PAID.
           IF PPYT-PAID-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PPYT-PAID-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE EXCEPT YTD FEE ON THE ISSUED YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3210-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN PPYM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO PPYM-EXCEPT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYM-EXEMPT-FLAG = 'Y'
                   COMPUTE PPYM-EXCEPT-YTD ROUNDED = PPYM-ISSUED-YTD *
                       PPYM-RATE / 200
               WHEN PPYM-REASON-CD = 'B'
                   MOVE 20.00 TO PPYM-EXCEPT-YTD
               WHEN PPYM-REASON-CD = 'M'
                   MOVE 15.00 TO PPYM-EXCEPT-YTD
               WHEN OTHER
                   COMPUTE PPYM-EXCEPT-YTD ROUNDED = PPYM-ISSUED-YTD *
                       PPYM-RATE / 100
           END-EVALUATE
           IF PPYM-EXCEPT-YTD > 0 AND PPYM-EXCEPT-YTD < 10.00
               MOVE 10.00 TO PPYM-EXCEPT-YTD
           END-IF
           IF PPYM-EXCEPT-YTD > 35.00
               MOVE 35.00 TO PPYM-EXCEPT-YTD
           END-IF
           ADD PPYM-EXCEPT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STALE AMT IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3220-LIMIT-STALE.
           IF PPYM-STALE-AMT < 100.00
               MOVE 100.00 TO PPYM-STALE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PPYM-STALE-AMT > 500.00
               MOVE 500.00 TO PPYM-STALE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT YTD IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3230-LIMIT-EXCEPT.
           IF PPYM-EXCEPT-YTD < 50.00
               MOVE 50.00 TO PPYM-EXCEPT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PPYM-EXCEPT-YTD > 5000.00
               MOVE 5000.00 TO PPYM-EXCEPT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAID AMT IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3240-LIMIT-PAID.
           IF PPYM-PAID-AMT < 50.00
               MOVE 50.00 TO PPYM-PAID-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PPYM-PAID-AMT > 5000.00
               MOVE 5000.00 TO PPYM-PAID-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3250-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF PPYM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPYM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO PPYM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E779 CUST ID REQUIRED
      * E099 WORK AMT NEGATIVE
      * E294 WORK AMT OVER 5000
      * E571 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3260-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPYM-CUST-ID = SPACES
                   MOVE 'E779' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E099' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 5000.00
                   MOVE 'E294' TO WS-REJECT-CODE
               WHEN PPYM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E571' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 120 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3270-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF PPYM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPYM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO PPYM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ISSUED GRID TIER FOR THE EXCEPT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3280-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-ISSUED-GRID-RATE(1) TO PPYM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF PPYM-EXCEPT-AMT NOT < WS-ISSUED-GRID-LOW(WS-TX)
                   MOVE WS-ISSUED-GRID-RATE(WS-TX) TO PPYM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPYM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPYM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE VOID SCHED TIER FOR THE EXCEPT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3290-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-VOID-SCHED-RATE(1) TO PPYM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF PPYM-EXCEPT-AMT NOT < WS-VOID-SCHED-LOW(WS-TX)
                   MOVE WS-VOID-SCHED-RATE(WS-TX) TO PPYM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPYM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPYM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E661 CONTACT REQUIRED
      * E012 VOID AMT NEGATIVE
      * E853 VOID AMT OVER 10000
      * E311 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3300-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPYM-CONTACT = SPACES
                   MOVE 'E661' TO WS-REJECT-CODE
               WHEN PPYT-VOID-AMT < 0
                   MOVE 'E012' TO WS-REJECT-CODE
               WHEN PPYT-VOID-AMT > 10000.00
                   MOVE 'E853' TO WS-REJECT-CODE
               WHEN PPYM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E311' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3310-COUNT-PAYMENT.
           IF PPYM-PAYMENT-CNT < 9999999
               ADD 1 TO PPYM-PAYMENT-CNT
           END-IF
           IF PPYM-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO PPYM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3320-DESCRIBE-REGION.
           EVALUATE PPYM-REGION-CD
               WHEN 'A'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'B'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'D'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'H'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'N'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'Y'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY DECISION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3330-ACCUM-DECISION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPYM-DECISION-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPYM-DECISION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ISSUED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO STALE YTD.
      *----------------------------------------------------------------*
       3340-PRORATE-STALE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPYM-STALE-YTD ROUNDED =
                   PPYM-ISSUED-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PPYM-STALE-YTD
           END-IF
           ADD PPYM-STALE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE STALE YTD FEE ON THE EXCEPT AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3350-ASSESS-STOP.
           EVALUATE TRUE
               WHEN PPYM-STOP-FLAG = 'Y'
                   MOVE ZERO TO PPYM-STALE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE PPYM-STALE-YTD ROUNDED = PPYM-EXCEPT-AMT *
                       PPYM-MARGIN / 200
               WHEN PPYM-REGION-CD = 'A'
                   MOVE 3.00 TO PPYM-STALE-YTD
               WHEN PPYM-REGION-CD = 'B'
                   MOVE 5.00 TO PPYM-STALE-YTD
               WHEN OTHER
                   COMPUTE PPYM-STALE-YTD ROUNDED = PPYM-EXCEPT-AMT *
                       PPYM-MARGIN / 100
           END-EVALUATE
           IF PPYM-STALE-YTD > 0 AND PPYM-STALE-YTD < 2.00
               MOVE 2.00 TO PPYM-STALE-YTD
           END-IF
           IF PPYM-STALE-YTD > 35.00
               MOVE 35.00 TO PPYM-STALE-YTD
           END-IF
           ADD PPYM-STALE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE STALE AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3360-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPYM-CYCLE-CD
                   ADD PPYM-STALE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPYM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PPYM-STALE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3370-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF PPYM-CLOSE-DATE > 0 AND PPYM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPYM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PPYM-ISSUED-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD PPYM-ISSUED-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD PPYM-ISSUED-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD PPYM-ISSUED-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPYM-ISSUED-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAID AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3380-FLAG-PAID.
           IF PPYT-PAID-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PPYT-PAID-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3390-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF PPYM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPYM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO PPYM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 180 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3400-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF PPYT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPYT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO PPYT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAID YTD IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3410-LIMIT-PAID.
           IF PPYM-PAID-YTD < 0.00
               MOVE 0.00 TO PPYM-PAID-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PPYM-PAID-YTD > 1000.00
               MOVE 1000.00 TO PPYM-PAID-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT STALE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3420-SPLIT-STALE.
           COMPUTE WS-WORK-AMT = PPYM-STALE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = PPYM-STALE-AMT - (WS-WORK-AMT * 12)
           COMPUTE PPYM-PAID-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN EFFECTIVE DATE IS PAST
      * BAT -> MAL WHEN EFFECTIVE DATE IS PAST
      * MAL -> TEL WHEN EFFECTIVE DATE IS PAST
      * TEL -> ONL WHEN EFFECTIVE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3430-SET-CHANNEL.
           EVALUATE PPYT-CHANNEL-CD
               WHEN 'BRN'
                   IF PPYM-EFFECTIVE-DATE > 0 AND PPYM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO PPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF PPYM-EFFECTIVE-DATE > 0 AND PPYM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO PPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF PPYM-EFFECTIVE-DATE > 0 AND PPYM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO PPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF PPYM-EFFECTIVE-DATE > 0 AND PPYM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO PPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ISSUED AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3440-ACCRUE-ISSUED.
           IF PPYT-ISSUED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPYT-ISSUED-AMT * PPYM-RATE / 100 / 365
               ADD WS-WORK-AMT TO PPYM-EXCEPT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3450-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * VOID YTD IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3460-LIMIT-VOID.
           IF PPYM-VOID-YTD < 100.00
               MOVE 100.00 TO PPYM-VOID-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PPYM-VOID-YTD > 500.00
               MOVE 500.00 TO PPYM-VOID-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3470-COUNT-EXCEPT.
           IF PPYM-EXCEPT-CNT < 9999999
               ADD 1 TO PPYM-EXCEPT-CNT
           END-IF
           IF PPYM-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO PPYM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3480-COUNT-SEQ.
           IF PPYT-SEQ < 9999999
               ADD 1 TO PPYT-SEQ
           END-IF
           IF PPYT-SEQ NOT < 5
               MOVE 'Y' TO PPYM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * A -> B WHEN EFFECTIVE DATE IS PAST
      * B -> C WHEN EFFECTIVE DATE IS PAST
      * C -> R WHEN EFFECTIVE DATE IS PAST
      * R -> T WHEN EFFECTIVE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3490-SET-CYCLE.
           EVALUATE PPYM-CYCLE-CD
               WHEN 'A'
                   IF PPYM-EFFECTIVE-DATE > 0 AND PPYM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'B' TO PPYM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'B'
                   IF PPYM-EFFECTIVE-DATE > 0 AND PPYM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'C' TO PPYM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'C'
                   IF PPYM-EFFECTIVE-DATE > 0 AND PPYM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'R' TO PPYM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF PPYM-EFFECTIVE-DATE > 0 AND PPYM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO PPYM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3500-COUNT-PAYMENT.
           IF PPYM-PAYMENT-CNT < 9999999
               ADD 1 TO PPYM-PAYMENT-CNT
           END-IF
           IF PPYM-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO PPYM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3510-DESCRIBE-REGION.
           EVALUATE PPYM-REGION-CD
               WHEN 'A'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'B'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'D'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'H'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'N'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'Y'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * DR -> TK WHEN CLOSE DATE IS PAST
      * TK -> WX WHEN CLOSE DATE IS PAST
      * WX -> DR WHEN CLOSE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3520-SET-STATUS.
           EVALUATE PPYM-STATUS-CD
               WHEN 'DR'
                   IF PPYM-CLOSE-DATE > 0 AND PPYM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TK' TO PPYM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TK'
                   IF PPYM-CLOSE-DATE > 0 AND PPYM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WX' TO PPYM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WX'
                   IF PPYM-CLOSE-DATE > 0 AND PPYM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DR' TO PPYM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT EXCEPT AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3530-SPLIT-EXCEPT.
           COMPUTE WS-WORK-AMT = PPYM-EXCEPT-AMT / 4
           COMPUTE WS-WORK-AMT-2 = PPYM-EXCEPT-AMT - (WS-WORK-AMT * 4)
           COMPUTE PPYT-STALE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAID FEE ON THE ISSUED AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3540-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN PPYM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO PPYM-PAID-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYM-STOP-FLAG = 'Y'
                   COMPUTE PPYM-PAID-AMT ROUNDED = PPYT-ISSUED-AMT *
                       PPYM-MARGIN / 200
               WHEN PPYM-DECISION-CD = 'AND'
                   MOVE 10.00 TO PPYM-PAID-AMT
               WHEN PPYM-DECISION-CD = 'BER'
                   MOVE 7.00 TO PPYM-PAID-AMT
               WHEN OTHER
                   COMPUTE PPYM-PAID-AMT ROUNDED = PPYT-ISSUED-AMT *
                       PPYM-MARGIN / 100
           END-EVALUATE
           IF PPYM-PAID-AMT > 0 AND PPYM-PAID-AMT < 2.00
               MOVE 2.00 TO PPYM-PAID-AMT
           END-IF
           IF PPYM-PAID-AMT > 75.00
               MOVE 75.00 TO PPYM-PAID-AMT
           END-IF
           ADD PPYM-PAID-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE PAID TIER TIER FOR THE VOID YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3550-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-PAID-TIER-RATE(1) TO PPYM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF PPYM-VOID-YTD NOT < WS-PAID-TIER-LOW(WS-TX)
                   MOVE WS-PAID-TIER-RATE(WS-TX) TO PPYM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPYM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPYM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAID TIER TIER FOR THE PAID YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3560-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PAID-TIER-RATE(1) TO PPYM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF PPYM-PAID-YTD NOT < WS-PAID-TIER-LOW(WS-TX)
                   MOVE WS-PAID-TIER-RATE(WS-TX) TO PPYM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPYM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPYM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAID AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3570-ACCRUE-PAID.
           IF PPYT-PAID-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPYT-PAID-AMT * PPYM-RATE / 100 / 365
               ADD WS-WORK-AMT TO PPYM-VOID-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON EXCEPT YTD AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3580-ACCRUE-EXCEPT.
           IF PPYM-EXCEPT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPYM-EXCEPT-YTD * PPYM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ISSUED AMT BY DECISION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3590-ACCUM-DECISION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPYM-DECISION-CD
                   ADD PPYT-ISSUED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPYM-DECISION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PPYT-ISSUED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3600-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF PPYM-NEXT-DUE-DATE > 0 AND PPYM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPYM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PPYT-PAID-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PPYT-PAID-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD PPYT-PAID-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD PPYT-PAID-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPYT-PAID-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAID AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3610-ACCRUE-PAID.
           IF PPYT-PAID-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPYT-PAID-AMT * PPYM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO PPYM-EXCEPT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE VOID AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO STALE YTD.
      *----------------------------------------------------------------*
       3620-PRORATE-STALE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPYM-STALE-YTD ROUNDED =
                   PPYM-VOID-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PPYM-STALE-YTD
           END-IF
           ADD PPYM-STALE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-45 AND OVER 45 DAYS.
      *----------------------------------------------------------------*
       3630-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF PPYM-OPEN-DATE > 0 AND PPYM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPYM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PPYM-VOID-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PPYM-VOID-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD PPYM-VOID-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 45
                   ADD PPYM-VOID-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPYM-VOID-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3640-COUNT-PAYMENT.
           IF PPYM-PAYMENT-CNT < 9999999
               ADD 1 TO PPYM-PAYMENT-CNT
           END-IF
           IF PPYM-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO PPYT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE VOID YTD BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3650-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPYT-TRAN-CD
                   ADD PPYM-VOID-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPYT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PPYM-VOID-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 365 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3660-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF PPYM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPYM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO PPYM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STALE AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3670-FLAG-STALE.
           IF PPYM-STALE-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PPYM-STALE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 90 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3680-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF PPYM-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPYM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO PPYM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE VOID YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3690-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPYM-STATUS-CD
                   ADD PPYM-VOID-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPYM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PPYM-VOID-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3700-FLAG-EXCEPT.
           IF PPYM-EXCEPT-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PPYM-EXCEPT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * DR -> TK WHEN NEXT REVIEW DATE IS PAST
      * TK -> WX WHEN NEXT REVIEW DATE IS PAST
      * WX -> DR WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3710-SET-STATUS.
           EVALUATE PPYM-STATUS-CD
               WHEN 'DR'
                   IF PPYM-NEXT-REVIEW-DATE > 0 AND
                       PPYM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'TK' TO PPYM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TK'
                   IF PPYM-NEXT-REVIEW-DATE > 0 AND
                       PPYM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'WX' TO PPYM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WX'
                   IF PPYM-NEXT-REVIEW-DATE > 0 AND
                       PPYM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'DR' TO PPYM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E067 NAME REQUIRED
      * E283 PAID AMT NEGATIVE
      * E394 PAID AMT OVER 10000
      * E870 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3720-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPYM-NAME = SPACES
                   MOVE 'E067' TO WS-REJECT-CODE
               WHEN PPYT-PAID-AMT < 0
                   MOVE 'E283' TO WS-REJECT-CODE
               WHEN PPYT-PAID-AMT > 10000.00
                   MOVE 'E394' TO WS-REJECT-CODE
               WHEN PPYM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E870' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3730-COUNT-SEQ.
           IF PPYT-SEQ < 9999999
               ADD 1 TO PPYT-SEQ
           END-IF
           IF PPYT-SEQ NOT < 12
               MOVE 'Y' TO PPYM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE VOID SCHED TIER FOR THE EXCEPT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3740-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-VOID-SCHED-RATE(1) TO PPYM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF PPYM-EXCEPT-AMT NOT < WS-VOID-SCHED-LOW(WS-TX)
                   MOVE WS-VOID-SCHED-RATE(WS-TX) TO PPYM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPYM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPYM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY DECISION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3750-ACCUM-DECISION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPYM-DECISION-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPYM-DECISION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3760-FLAG-EXCEPT.
           IF PPYM-EXCEPT-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PPYM-EXCEPT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAID AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3770-PRORATE-PAID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPYT-PAID-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E606 OPERATOR REQUIRED
      * E268 WORK AMT NEGATIVE
      * E518 WORK AMT OVER 1000000
      * E788 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3780-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPYT-OPERATOR = SPACES
                   MOVE 'E606' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E268' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 1000000.00
                   MOVE 'E518' TO WS-REJECT-CODE
               WHEN PPYM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E788' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE VOID YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3790-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPYT-CHANNEL-CD
                   ADD PPYM-VOID-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPYT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PPYM-VOID-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3800-COUNT-SEQ.
           IF PPYT-SEQ < 9999999
               ADD 1 TO PPYT-SEQ
           END-IF
           IF PPYT-SEQ NOT < 5
               MOVE 'Y' TO PPYM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VOID AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3810-ACCRUE-VOID.
           IF PPYM-VOID-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPYM-VOID-AMT * PPYM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO PPYM-PAID-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT STALE YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3820-SPLIT-STALE.
           COMPUTE WS-WORK-AMT = PPYM-STALE-YTD / 3
           COMPUTE WS-WORK-AMT-2 = PPYM-STALE-YTD - (WS-WORK-AMT * 3)
           COMPUTE PPYM-PAID-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3830-COUNT-CYCLE.
           IF PPYM-CYCLE-CNT < 9999999
               ADD 1 TO PPYM-CYCLE-CNT
           END-IF
           IF PPYM-CYCLE-CNT NOT < 5
               MOVE 'Y' TO PPYM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 45 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3840-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF PPYT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPYT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO PPYM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 10 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3850-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF PPYM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPYM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO PPYT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 45 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3860-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF PPYM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPYM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO PPYM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3870-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF PPYM-EFFECTIVE-DATE > 0 AND PPYM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPYM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PPYM-VOID-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD PPYM-VOID-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD PPYM-VOID-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD PPYM-VOID-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPYM-VOID-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE PAID FEE ON THE WORK AMT 2.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3880-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN PPYM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO PPYM-PAID-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYM-STOP-FLAG = 'Y'
                   COMPUTE PPYM-PAID-AMT ROUNDED = WS-WORK-AMT-2 *
                       PPYM-DISCOUNT / 200
               WHEN PPYM-MATCH-RULE-CD = 'A'
                   MOVE 15.00 TO PPYM-PAID-AMT
               WHEN PPYM-MATCH-RULE-CD = 'B'
                   MOVE 15.00 TO PPYM-PAID-AMT
               WHEN OTHER
                   COMPUTE PPYM-PAID-AMT ROUNDED = WS-WORK-AMT-2 *
                       PPYM-DISCOUNT / 100
           END-EVALUATE
           IF PPYM-PAID-AMT > 0 AND PPYM-PAID-AMT < 2.00
               MOVE 2.00 TO PPYM-PAID-AMT
           END-IF
           IF PPYM-PAID-AMT > 250.00
               MOVE 250.00 TO PPYM-PAID-AMT
           END-IF
           ADD PPYM-PAID-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3890-DESCRIBE-REGION.
           EVALUATE PPYM-REGION-CD
               WHEN 'A'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'B'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'D'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'H'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'N'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'Y'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3900-DESCRIBE-REGION.
           EVALUATE PPYM-REGION-CD
               WHEN 'A'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'B'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'D'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'H'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'N'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'Y'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE ISSUED FEE ON THE VOID YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3910-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN PPYM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO PPYM-ISSUED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYM-HOLD-FLAG = 'Y'
                   COMPUTE PPYM-ISSUED-AMT ROUNDED = PPYM-VOID-YTD *
                       PPYM-DISCOUNT / 200
               WHEN PPYM-REASON-CD = 'B'
                   MOVE 5.00 TO PPYM-ISSUED-AMT
               WHEN PPYM-REASON-CD = 'M'
                   MOVE 5.00 TO PPYM-ISSUED-AMT
               WHEN OTHER
                   COMPUTE PPYM-ISSUED-AMT ROUNDED = PPYM-VOID-YTD *
                       PPYM-DISCOUNT / 100
           END-EVALUATE
           IF PPYM-ISSUED-AMT > 0 AND PPYM-ISSUED-AMT < 10.00
               MOVE 10.00 TO PPYM-ISSUED-AMT
           END-IF
           IF PPYM-ISSUED-AMT > 25.00
               MOVE 25.00 TO PPYM-ISSUED-AMT
           END-IF
           ADD PPYM-ISSUED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3920-COUNT-CYCLE.
           IF PPYM-CYCLE-CNT < 9999999
               ADD 1 TO PPYM-CYCLE-CNT
           END-IF
           IF PPYM-CYCLE-CNT NOT < 10
               MOVE 'Y' TO PPYM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3930-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF PPYT-EFF-DATE > 0 AND PPYT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPYT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE ISSUED TIER TIER FOR THE VOID AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3940-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-ISSUED-TIER-RATE(1) TO PPYM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF PPYT-VOID-AMT NOT < WS-ISSUED-TIER-LOW(WS-TX)
                   MOVE WS-ISSUED-TIER-RATE(WS-TX) TO PPYM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPYM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPYM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 365 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3950-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF PPYM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPYM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO PPYM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAID YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3960-FLAG-PAID.
           IF PPYM-PAID-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PPYM-PAID-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DECISION CD:
      * AND -> BER WHEN LAST ACT DATE IS PAST
      * BER -> CZW WHEN LAST ACT DATE IS PAST
      * CZW -> SBV WHEN LAST ACT DATE IS PAST
      * SBV -> ZNG WHEN LAST ACT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3970-SET-DECISION.
           EVALUATE PPYM-DECISION-CD
               WHEN 'AND'
                   IF PPYM-LAST-ACT-DATE > 0 AND PPYM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BER' TO PPYM-DECISION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BER'
                   IF PPYM-LAST-ACT-DATE > 0 AND PPYM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CZW' TO PPYM-DECISION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CZW'
                   IF PPYM-LAST-ACT-DATE > 0 AND PPYM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SBV' TO PPYM-DECISION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SBV'
                   IF PPYM-LAST-ACT-DATE > 0 AND PPYM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZNG' TO PPYM-DECISION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE DECISION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3980-DESCRIBE-DECISION.
           EVALUATE PPYM-DECISION-CD
               WHEN 'AND'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'BER'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'CZW'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'SBV'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'ZNG'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'ZPS'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ISSUED AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3990-FLAG-ISSUED.
           IF PPYM-ISSUED-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PPYM-ISSUED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4000-FLAG-EXCEPT.
           IF PPYM-EXCEPT-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PPYM-EXCEPT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       4010-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF PPYM-NEXT-DUE-DATE > 0 AND PPYM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPYM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PPYM-VOID-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD PPYM-VOID-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD PPYM-VOID-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD PPYM-VOID-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPYM-VOID-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ISSUED YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4020-ACCRUE-ISSUED.
           IF PPYM-ISSUED-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPYM-ISSUED-YTD * PPYM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO PPYT-ISSUED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * VOID AMT IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4030-LIMIT-VOID.
           IF PPYT-VOID-AMT < 0.00
               MOVE 0.00 TO PPYT-VOID-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PPYT-VOID-AMT > 500.00
               MOVE 500.00 TO PPYT-VOID-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       4040-PRORATE-ITEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4050-DESCRIBE-CHANNEL.
           EVALUATE PPYT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN EFF DATE IS PAST
      * BAT -> MAL WHEN EFF DATE IS PAST
      * MAL -> TEL WHEN EFF DATE IS PAST
      * TEL -> ONL WHEN EFF DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4060-SET-CHANNEL.
           EVALUATE PPYT-CHANNEL-CD
               WHEN 'BRN'
                   IF PPYT-EFF-DATE > 0 AND PPYT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO PPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF PPYT-EFF-DATE > 0 AND PPYT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO PPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF PPYT-EFF-DATE > 0 AND PPYT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO PPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF PPYT-EFF-DATE > 0 AND PPYT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO PPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4070-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF PPYT-POST-DATE > 0 AND PPYT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPYT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PPYM-STALE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PPYM-STALE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD PPYM-STALE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PPYM-STALE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPYM-STALE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE EXCEPT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO VOID AMT.
      *----------------------------------------------------------------*
       4080-PRORATE-VOID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPYT-VOID-AMT ROUNDED =
                   PPYM-EXCEPT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PPYT-VOID-AMT
           END-IF
           ADD PPYT-VOID-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE MATCH RULE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4090-DESCRIBE-MATCH-RULE.
           EVALUATE PPYM-MATCH-RULE-CD
               WHEN 'A'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'B'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'F'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'M'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'S'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'V'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4100-COUNT-PAYMENT.
           IF PPYM-PAYMENT-CNT < 9999999
               ADD 1 TO PPYM-PAYMENT-CNT
           END-IF
           IF PPYM-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO PPYT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE DECISION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4110-DESCRIBE-DECISION.
           EVALUATE PPYM-DECISION-CD
               WHEN 'AND'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'BER'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'CZW'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'SBV'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'ZNG'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'ZPS'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT VOID YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4120-SPLIT-VOID.
           COMPUTE WS-WORK-AMT = PPYM-VOID-YTD / 3
           COMPUTE WS-WORK-AMT-2 = PPYM-VOID-YTD - (WS-WORK-AMT * 3)
           COMPUTE PPYM-STALE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE VOID YTD BY MATCH RULE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4130-ACCUM-MATCH-RULE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPYM-MATCH-RULE-CD
                   ADD PPYM-VOID-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPYM-MATCH-RULE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PPYM-VOID-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE VOID YTD FEE ON THE STALE AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       4140-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN PPYM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO PPYM-VOID-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYT-REVERSAL-FLAG = 'Y'
                   COMPUTE PPYM-VOID-YTD ROUNDED = PPYT-STALE-AMT *
                       PPYM-DISCOUNT / 200
               WHEN PPYM-REGION-CD = 'A'
                   MOVE 7.00 TO PPYM-VOID-YTD
               WHEN PPYM-REGION-CD = 'B'
                   MOVE 15.00 TO PPYM-VOID-YTD
               WHEN OTHER
                   COMPUTE PPYM-VOID-YTD ROUNDED = PPYT-STALE-AMT *
                       PPYM-DISCOUNT / 100
           END-EVALUATE
           IF PPYM-VOID-YTD > 0 AND PPYM-VOID-YTD < 1.00
               MOVE 1.00 TO PPYM-VOID-YTD
           END-IF
           IF PPYM-VOID-YTD > 150.00
               MOVE 150.00 TO PPYM-VOID-YTD
           END-IF
           ADD PPYM-VOID-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4150-DESCRIBE-REASON.
           EVALUATE PPYM-REASON-CD
               WHEN 'B'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'M'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'N'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'U'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'V'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'Z'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4160-COUNT-EXCEPT.
           IF PPYM-EXCEPT-CNT < 9999999
               ADD 1 TO PPYM-EXCEPT-CNT
           END-IF
           IF PPYM-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO PPYT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE PPY FIELDS TO PPLIM AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7000-CALL-PPLIM.
           INITIALIZE PPLIM-PARMS
           MOVE PPYM-STALE-YTD
             TO PPLIM-STALE-YTD
           MOVE PPYM-EXCEPT-YTD
             TO PPLIM-EXCEPT-YTD
           MOVE PPYM-VOID-YTD
             TO PPLIM-VOID-YTD
           MOVE PPYM-PAID-YTD
             TO PPLIM-PAID-YTD
           MOVE PPYM-ISSUED-YTD
             TO PPLIM-ISSUED-YTD
           MOVE PPYM-PAID-AMT
             TO PPLIM-PAID-AMT
           MOVE PPYM-NEXT-REVIEW-DATE
             TO PPLIM-NEXT-REVIEW-DATE
           MOVE PPYM-CLOSE-DATE
             TO PPLIM-CLOSE-DATE
           MOVE PPYM-OPEN-DATE
             TO PPLIM-OPEN-DATE
           MOVE PPYM-EXPIRY-DATE
             TO PPLIM-EXPIRY-DATE
           MOVE PPYM-MATCH-RULE-CD
             TO PPLIM-MATCH-RULE-CD
           MOVE PPYM-DECISION-CD
             TO PPLIM-DECISION-CD
           MOVE PPYM-REASON-CD
             TO PPLIM-REASON-CD
           MOVE PPYM-REGION-CD
             TO PPLIM-REGION-CD
           MOVE PPYM-NOTICE-SENT-FLAG
             TO PPLIM-NOTICE-SENT-FLAG
           MOVE PPYM-AUTO-RENEW-FLAG
             TO PPLIM-AUTO-RENEW-FLAG
           MOVE PPYM-EXEMPT-FLAG
             TO PPLIM-EXEMPT-FLAG
           MOVE PPYM-CYCLE-CNT
             TO PPLIM-CYCLE-CNT
           MOVE PPYM-NOTICE-CNT
             TO PPLIM-NOTICE-CNT
           MOVE PPYM-RATE
             TO PPLIM-RATE
           MOVE PPYM-DISCOUNT
             TO PPLIM-DISCOUNT
           MOVE PPYM-NAME
             TO PPLIM-NAME
           MOVE PPYM-REF-NO
             TO PPLIM-REF-NO
           CALL 'PPLIM' USING PPLIM-PARMS
           EVALUATE PPLIM-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE PPLIM-STALE-YTD
                     TO PPYM-STALE-YTD
                   MOVE PPLIM-EXCEPT-YTD
                     TO PPYM-EXCEPT-YTD
                   MOVE PPLIM-VOID-YTD
                     TO PPYM-VOID-YTD
                   MOVE PPLIM-PAID-YTD
                     TO PPYM-PAID-YTD
                   MOVE PPLIM-EXEMPT-FLAG
                     TO PPYM-EXEMPT-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RLIM' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-PPLIM' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE PPY FIELDS TO PPFEE AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7040-CALL-PPFEE.
           INITIALIZE PPFEE-PARMS
           MOVE PPYM-STALE-YTD
             TO PPFEE-STALE-YTD
           MOVE PPYM-PAID-YTD
             TO PPFEE-PAID-YTD
           MOVE PPYM-EXCEPT-AMT
             TO PPFEE-EXCEPT-AMT
           MOVE PPYM-VOID-YTD
             TO PPFEE-VOID-YTD
           MOVE PPYM-VOID-AMT
             TO PPFEE-VOID-AMT
           MOVE PPYM-PAID-AMT
             TO PPFEE-PAID-AMT
           MOVE PPYM-NEXT-REVIEW-DATE
             TO PPFEE-NEXT-REVIEW-DATE
           MOVE PPYM-LAST-PAY-DATE
             TO PPFEE-LAST-PAY-DATE
           MOVE PPYM-CLOSE-DATE
             TO PPFEE-CLOSE-DATE
           MOVE PPYM-OPEN-DATE
             TO PPFEE-OPEN-DATE
           MOVE PPYM-MATCH-RULE-CD
             TO PPFEE-MATCH-RULE-CD
           MOVE PPYM-REGION-CD
             TO PPFEE-REGION-CD
           MOVE PPYM-STATUS-CD
             TO PPFEE-STATUS-CD
           MOVE PPYM-REASON-CD
             TO PPFEE-REASON-CD
           MOVE PPYM-NOTICE-SENT-FLAG
             TO PPFEE-NOTICE-SENT-FLAG
           MOVE PPYM-EXEMPT-FLAG
             TO PPFEE-EXEMPT-FLAG
           MOVE PPYM-OVERRIDE-FLAG
             TO PPFEE-OVERRIDE-FLAG
           MOVE PPYM-PAYMENT-CNT
             TO PPFEE-PAYMENT-CNT
           MOVE PPYM-NOTICE-CNT
             TO PPFEE-NOTICE-CNT
           MOVE PPYM-DISCOUNT
             TO PPFEE-DISCOUNT
           MOVE PPYM-MARGIN
             TO PPFEE-MARGIN
           MOVE PPYM-CONTACT
             TO PPFEE-CONTACT
           MOVE PPYM-REF-NO
             TO PPFEE-REF-NO
           CALL 'PPFEE' USING PPFEE-PARMS
           EVALUATE PPFEE-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE PPFEE-EXCEPT-AMT
                     TO PPYM-EXCEPT-AMT
                   MOVE PPFEE-PAID-AMT
                     TO PPYM-PAID-AMT
                   MOVE PPFEE-NOTICE-SENT-FLAG
                     TO PPYM-NOTICE-SENT-FLAG
                   MOVE PPFEE-EXEMPT-FLAG
                     TO PPYM-EXEMPT-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RFEE' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7040-CALL-PPFEE' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE PPY FIELDS TO PPSTAT AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7030-CALL-PPSTAT.
           INITIALIZE PPSTAT-PARMS
           MOVE PPYM-EXCEPT-YTD
             TO PPSTAT-EXCEPT-YTD
           MOVE PPYM-VOID-AMT
             TO PPSTAT-VOID-AMT
           MOVE PPYM-PAID-YTD
             TO PPSTAT-PAID-YTD
           MOVE PPYM-EXCEPT-AMT
             TO PPSTAT-EXCEPT-AMT
           MOVE PPYM-PAID-AMT
             TO PPSTAT-PAID-AMT
           MOVE PPYM-STALE-YTD
             TO PPSTAT-STALE-YTD
           MOVE PPYM-CLOSE-DATE
             TO PPSTAT-CLOSE-DATE
           MOVE PPYM-EFFECTIVE-DATE
             TO PPSTAT-EFFECTIVE-DATE
           MOVE PPYM-LAST-PAY-DATE
             TO PPSTAT-LAST-PAY-DATE
           MOVE PPYM-EXPIRY-DATE
             TO PPSTAT-EXPIRY-DATE
           MOVE PPYM-REGION-CD
             TO PPSTAT-REGION-CD
           MOVE PPYM-REASON-CD
             TO PPSTAT-REASON-CD
           MOVE PPYM-MATCH-RULE-CD
             TO PPSTAT-MATCH-RULE-CD
           MOVE PPYM-CYCLE-CD
             TO PPSTAT-CYCLE-CD
           MOVE PPYM-OVERRIDE-FLAG
             TO PPSTAT-OVERRIDE-FLAG
           MOVE PPYM-EXEMPT-FLAG
             TO PPSTAT-EXEMPT-FLAG
           MOVE PPYM-HOLD-FLAG
             TO PPSTAT-HOLD-FLAG
           MOVE PPYM-NOTICE-CNT
             TO PPSTAT-NOTICE-CNT
           MOVE PPYM-PAYMENT-CNT
             TO PPSTAT-PAYMENT-CNT
           MOVE PPYM-DISCOUNT
             TO PPSTAT-DISCOUNT
           MOVE PPYM-MARGIN
             TO PPSTAT-MARGIN
           MOVE PPYM-CONTACT
             TO PPSTAT-CONTACT
           MOVE PPYM-REF-NO
             TO PPSTAT-REF-NO
           CALL 'PPSTAT' USING PPSTAT-PARMS
           EVALUATE PPSTAT-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE PPSTAT-PAID-YTD
                     TO PPYM-PAID-YTD
                   MOVE PPSTAT-EXCEPT-AMT
                     TO PPYM-EXCEPT-AMT
                   MOVE PPSTAT-PAID-AMT
                     TO PPYM-PAID-AMT
                   MOVE PPSTAT-OVERRIDE-FLAG
                     TO PPYM-OVERRIDE-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RTAT' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7030-CALL-PPSTAT' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE PPYT-KEY TO RD-KEY
           MOVE PPYT-TRAN-CD TO RD-CODE
           MOVE PPYT-VOID-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE PPYT-KEY TO RD-KEY
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
           DISPLAY 'PPYEDIT READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'PPYEDIT' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
