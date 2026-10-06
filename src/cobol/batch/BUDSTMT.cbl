      *================================================================*
      * PROGRAM  : BUDSTMT                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : BUMP-UP CERTIFICATES - STATEMENT PREPARATION. READS *
      *            THE BCD TRANSACTIONS, UPDATES THE BCD MASTER AND    *
      *            PRODUCES THE CONTROL REPORT. BUMP-UP AND STEP-RATE  *
      *            CERTIFICATE RATE ELECTION AND ACCRUAL               *
      * INPUT    : BCDMST (KSDS), BCDTRN, BANKCTL                      *
      * OUTPUT   : BCDOUT, BUDSTRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, BUTIER                  *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1992-08-22 LMB  ORIGINAL                                       *
      * 1997-12-13 PQR  PERFORMANCE - BUMP READ AHEAD INC8869          *
      * 1998-03-16 GAF  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-07-01 TWH  INTEREST NOTICE TEXT PER LEGAL REQ3850         *
      * 1999-10-02 EWS  PERFORMANCE - PRINCIPAL READ AHEAD PRB8325     *
      * 2002-02-20 PQR  PENALTY CALCULATION CORRECTED REQ3618          *
      * 2003-12-09 TWH  PRINCIPAL TABLE EXPANDED PRB6316               *
      * 2004-05-09 RJT  INTEREST ROUNDING - HALF UP REQ9108            *
      * 2007-02-25 EWS  PRINCIPAL RERUN PROTECTION REQ6955             *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BUDSTMT.
       AUTHOR. J L MARSH.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "BCDMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS BCDM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "BCDTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "BCDOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "BUDSTRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY BCDMST.
       FD  TRAN-FILE.
       COPY BCDTRN.
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
                                           VALUE 8.000000.
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
      * WS-BUMP-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-BUMP-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000702500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000063000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000130000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015000000000168000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015050000000156000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015100000000880500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016100000000475000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016600000000222500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016850000000353000'.
       01  WS-BUMP-BAND-TBL REDEFINES WS-BUMP-BAND-VALUES.
           05  WS-BUMP-BAND-ROW OCCURS 9 TIMES.
               10  WS-BUMP-BAND-LOW        PIC 9(09)V99.
               10  WS-BUMP-BAND-RATE       PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-BUMP-BANDX - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-BUMP-BANDX-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000221000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000374000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000200000000070000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002700000000680500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012700000000900500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012950000000828000'.
       01  WS-BUMP-BANDX-TBL REDEFINES WS-BUMP-BANDX-VALUES.
           05  WS-BUMP-BANDX-ROW OCCURS 6 TIMES.
               10  WS-BUMP-BANDX-LOW       PIC 9(09)V99.
               10  WS-BUMP-BANDX-RATE      PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  BUDSTMT  BUMP-UP CERTIFICATES'.
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
       COPY BUTIERP.

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
               MOVE 'BCDMST' TO ABND-FILE-NAME
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
           MOVE BCDT-KEY TO BCDM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-AGE-NEXT-DUE
               PERFORM 3010-ACCUM-REGION
               PERFORM 3020-COUNT-CYCLE
               PERFORM 3030-DESCRIBE-STEP-PLAN
               PERFORM 3040-LIMIT-PRINCIPAL
               PERFORM 3050-DESCRIBE-SOURCE
               PERFORM 3060-CHECK-CLOSE
               PERFORM 3070-DESCRIBE-SOURCE
               PERFORM 3080-COUNT-EXCEPT
               PERFORM 3090-SELECT-PENALTY-PCT
               PERFORM 3100-COUNT-CYCLE
               PERFORM 3110-PRORATE-ADDON
               PERFORM 3120-ACCUM-STEP-PLAN
               PERFORM 3130-ASSESS-STOP
               PERFORM 3140-AGE-EFFECTIVE
               PERFORM 3150-SET-CHANNEL
               PERFORM 3160-AGE-LAST-STMT
               PERFORM 3170-CHECK-MATURITY
               PERFORM 3180-LIMIT-PENALTY
               PERFORM 3190-DESCRIBE-CHANNEL
               PERFORM 3200-FLAG-PENALTY
               PERFORM 3210-DESCRIBE-TRAN
               PERFORM 3220-SPLIT-BUMP
               PERFORM 3230-COUNT-EXCEPT
               PERFORM 3240-EDIT-CUST-ID
               PERFORM 3250-SPLIT-BUMP
               PERFORM 3260-CHECK-NEXT-REVIEW
               PERFORM 3270-ACCRUE-INTEREST
               PERFORM 3280-PRORATE-PRINCIPAL
               PERFORM 3290-CHECK-EFFECTIVE
               PERFORM 3300-SELECT-MARGIN
               PERFORM 3310-PRORATE-PRINCIPAL
               PERFORM 3320-PRORATE-BUMP
               PERFORM 3330-ACCUM-TRAN
               PERFORM 3340-DESCRIBE-CYCLE
               PERFORM 3350-CHECK-CLOSE
               PERFORM 3360-LIMIT-BUMP
               PERFORM 3370-FLAG-PENALTY
               PERFORM 3380-ACCRUE-PRINCIPAL
               PERFORM 3390-DESCRIBE-REGION
               PERFORM 3400-CHECK-EFFECTIVE
               PERFORM 3410-COUNT-ITEM
               PERFORM 3420-SPLIT-BUMP
               PERFORM 3430-AGE-EFF
               PERFORM 3440-SELECT-PENALTY-PCT
               PERFORM 3450-COUNT-SEQ
               PERFORM 3460-LIMIT-ITEM
               PERFORM 3470-ACCRUE-ADDON
               PERFORM 3480-PRORATE-INTEREST
               PERFORM 3490-ASSESS-STOP
               PERFORM 3500-DESCRIBE-CHANNEL
               PERFORM 3510-EDIT-CUST-ID
               PERFORM 3520-COUNT-ITEM
               PERFORM 3530-SPLIT-BUMP
               PERFORM 3540-ASSESS-REVIEW
               PERFORM 3550-SPLIT-PRINCIPAL
               PERFORM 3560-ASSESS-STOP
               PERFORM 3570-SET-CYCLE
               PERFORM 3580-ACCRUE-PENALTY
               PERFORM 3590-LIMIT-INTEREST
               PERFORM 3600-AGE-POST
               PERFORM 3610-COUNT-EXCEPT
               PERFORM 3620-DESCRIBE-CHANNEL
               PERFORM 3630-EDIT-REF-NO
               PERFORM 3640-ACCUM-CYCLE
               PERFORM 3650-PRORATE-INTEREST
               PERFORM 3660-ACCRUE-PENALTY
               PERFORM 3670-SET-STEP-PLAN
               PERFORM 3680-ASSESS-SUSPEND
               PERFORM 3690-SPLIT-PENALTY
               PERFORM 3700-LIMIT-PENALTY
               PERFORM 3710-AGE-EFF
               PERFORM 3720-ASSESS-REVERSAL
               PERFORM 3730-FLAG-PENALTY
               PERFORM 3740-SELECT-RATE
               PERFORM 3750-SET-CHANNEL
               PERFORM 3760-SPLIT-ADDON
               PERFORM 3770-FLAG-PRINCIPAL
               PERFORM 3780-SELECT-RATE
               PERFORM 3790-ACCUM-CHANNEL
               PERFORM 3800-SPLIT-BUMP
               PERFORM 3810-AGE-LAST-MAINT
               PERFORM 3820-DESCRIBE-REGION
               PERFORM 3830-SPLIT-BUMP
               PERFORM 3840-SET-SOURCE
               PERFORM 3850-COUNT-ITEM
               PERFORM 3860-AGE-LAST-STMT
               PERFORM 3870-ASSESS-REVIEW
               PERFORM 3880-ACCUM-REGION
               PERFORM 3890-AGE-MATURITY
               PERFORM 3900-EDIT-DESC
               PERFORM 3910-ACCUM-STEP-PLAN
               PERFORM 3920-CHECK-CLOSE
               PERFORM 3930-SET-SOURCE
               PERFORM 3940-EDIT-REF-NO
               PERFORM 3950-AGE-LAST-STMT
               PERFORM 3960-COUNT-EXCEPT
               PERFORM 3970-SELECT-RATE
               PERFORM 3980-SET-SOURCE
               PERFORM 3990-COUNT-ITEM
               PERFORM 4000-PRORATE-BUMP
               PERFORM 4010-LIMIT-ADDON
               PERFORM 4020-SELECT-PENALTY-PCT
               PERFORM 4030-DESCRIBE-REGION
               PERFORM 4040-CHECK-EFF
               PERFORM 4050-CHECK-NEXT-DUE
               PERFORM 4060-PRORATE-PENALTY
               PERFORM 4070-FLAG-BUMP
               PERFORM 4080-LIMIT-ADDON
               PERFORM 4090-SELECT-RATE
               PERFORM 4100-COUNT-RETRY
               PERFORM 4110-DESCRIBE-REGION
               PERFORM 4120-ACCUM-STEP-PLAN
               PERFORM 4130-DESCRIBE-CYCLE
               PERFORM 4140-ACCUM-SOURCE
               PERFORM 4150-SET-CHANNEL
               PERFORM 4160-FLAG-PENALTY
               PERFORM 4170-ACCUM-STEP-PLAN
               PERFORM 4180-DESCRIBE-ELECTION
               PERFORM 4190-EDIT-CUST-ID
               PERFORM 4200-SPLIT-PRINCIPAL
               PERFORM 4210-COUNT-ITEM
               PERFORM 4220-ACCRUE-PRINCIPAL
               PERFORM 4230-ASSESS-STOP
               PERFORM 4240-ACCUM-CHANNEL
               PERFORM 4250-CHECK-CLOSE
               PERFORM 4260-CHECK-EFFECTIVE
               PERFORM 4270-FLAG-PRINCIPAL
               PERFORM 4280-EDIT-NAME
               PERFORM 4290-ACCRUE-ADDON
               PERFORM 4300-ASSESS-OVERRIDE
               PERFORM 4310-CHECK-MATURITY
               PERFORM 4320-ASSESS-EXEMPT
               PERFORM 4330-PRORATE-BUMP
               PERFORM 4340-DESCRIBE-SOURCE
               PERFORM 4350-FLAG-BUMP
               PERFORM 4360-SELECT-RATE
               PERFORM 4370-ACCRUE-PENALTY
               PERFORM 4380-ACCUM-CHANNEL
               PERFORM 4390-PRORATE-ADDON
               PERFORM 4400-LIMIT-BUMP
               PERFORM 4410-ASSESS-REVIEW
               PERFORM 4420-ACCUM-CHANNEL
               PERFORM 4430-FLAG-INTEREST
               PERFORM 4440-AGE-LAST-STMT
               PERFORM 4450-CHECK-LAST-STMT
               PERFORM 4460-SET-STEP-PLAN
               PERFORM 4470-SPLIT-PRINCIPAL
               PERFORM 4480-SET-ELECTION
               PERFORM 4490-SELECT-MARGIN
               PERFORM 4500-SELECT-PENALTY-PCT
               PERFORM 4510-CHECK-NEXT-REVIEW
               PERFORM 4520-ASSESS-HOLD
               PERFORM 4530-SPLIT-ADDON
               PERFORM 4540-DESCRIBE-REGION
               PERFORM 4550-COUNT-EXCEPT
               PERFORM 4560-COUNT-RETRY
               PERFORM 4570-COUNT-EXCEPT
               PERFORM 4580-ACCRUE-ADDON
               PERFORM 4590-EDIT-ADDR
               PERFORM 4600-FLAG-INTEREST
               PERFORM 4610-PRORATE-PENALTY
               PERFORM 4620-DESCRIBE-TRAN
               PERFORM 4630-CHECK-LAST-PAY
               PERFORM 4640-AGE-LAST-MAINT
               PERFORM 4650-ACCRUE-MARGIN
               PERFORM 4660-ASSESS-REVERSAL
               PERFORM 7040-CALL-BUTIER
               REWRITE BCD-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'BCDMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM BCD-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3000-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF BCDM-NEXT-DUE-DATE > 0 AND BCDM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BCDM-BUMP-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD BCDM-BUMP-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD BCDM-BUMP-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD BCDM-BUMP-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BCDM-BUMP-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE PRINCIPAL YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3010-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BCDM-REGION-CD
                   ADD BCDM-PRINCIPAL-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BCDM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BCDM-PRINCIPAL-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3020-COUNT-CYCLE.
           IF BCDM-CYCLE-CNT < 9999999
               ADD 1 TO BCDM-CYCLE-CNT
           END-IF
           IF BCDM-CYCLE-CNT NOT < 12
               MOVE 'Y' TO BCDM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STEP PLAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3030-DESCRIBE-STEP-PLAN.
           EVALUATE BCDM-STEP-PLAN-CD
               WHEN 'AYX'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'DGU'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'GWJ'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'RJL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'UPM'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'VCX'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'YPY'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRINCIPAL YTD IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3040-LIMIT-PRINCIPAL.
           IF BCDM-PRINCIPAL-YTD < 50.00
               MOVE 50.00 TO BCDM-PRINCIPAL-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BCDM-PRINCIPAL-YTD > 1000.00
               MOVE 1000.00 TO BCDM-PRINCIPAL-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3050-DESCRIBE-SOURCE.
           EVALUATE BCDM-SOURCE-CD
               WHEN 'AG'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'BF'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'HF'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'HR'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'UF'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'YD'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3060-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF BCDM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO BCDM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3070-DESCRIBE-SOURCE.
           EVALUATE BCDM-SOURCE-CD
               WHEN 'AG'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'BF'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'HF'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'HR'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'UF'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'YD'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3080-COUNT-EXCEPT.
           IF BCDM-EXCEPT-CNT < 9999999
               ADD 1 TO BCDM-EXCEPT-CNT
           END-IF
           IF BCDM-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO BCDM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE BUMP BANDX TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3090-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-BUMP-BANDX-RATE(1) TO BCDM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF WS-WORK-AMT NOT < WS-BUMP-BANDX-LOW(WS-TX)
                   MOVE WS-BUMP-BANDX-RATE(WS-TX) TO BCDM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BCDM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BCDM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3100-COUNT-CYCLE.
           IF BCDM-CYCLE-CNT < 9999999
               ADD 1 TO BCDM-CYCLE-CNT
           END-IF
           IF BCDM-CYCLE-CNT NOT < 10
               MOVE 'Y' TO BCDM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PENALTY AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ADDON AMT.
      *----------------------------------------------------------------*
       3110-PRORATE-ADDON.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BCDM-ADDON-AMT ROUNDED =
                   BCDM-PENALTY-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BCDM-ADDON-AMT
           END-IF
           ADD BCDM-ADDON-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE PENALTY AMT BY STEP PLAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3120-ACCUM-STEP-PLAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BCDM-STEP-PLAN-CD
                   ADD BCDT-PENALTY-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BCDM-STEP-PLAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BCDT-PENALTY-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ADDON FEE ON THE BUMP AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3130-ASSESS-STOP.
           EVALUATE TRUE
               WHEN BCDM-STOP-FLAG = 'Y'
                   MOVE ZERO TO BCDT-ADDON-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BCDT-REVERSAL-FLAG = 'Y'
                   COMPUTE BCDT-ADDON-AMT ROUNDED = BCDM-BUMP-AMT *
                       BCDM-PENALTY-PCT / 200
               WHEN BCDM-STEP-PLAN-CD = 'AYX'
                   MOVE 10.00 TO BCDT-ADDON-AMT
               WHEN BCDM-STEP-PLAN-CD = 'DGU'
                   MOVE 20.00 TO BCDT-ADDON-AMT
               WHEN OTHER
                   COMPUTE BCDT-ADDON-AMT ROUNDED = BCDM-BUMP-AMT *
                       BCDM-PENALTY-PCT / 100
           END-EVALUATE
           IF BCDT-ADDON-AMT > 0 AND BCDT-ADDON-AMT < 2.00
               MOVE 2.00 TO BCDT-ADDON-AMT
           END-IF
           IF BCDT-ADDON-AMT > 250.00
               MOVE 250.00 TO BCDT-ADDON-AMT
           END-IF
           ADD BCDT-ADDON-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3140-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF BCDM-EFFECTIVE-DATE > 0 AND BCDM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * EK -> JE WHEN EFF DATE IS PAST
      * JE -> PN WHEN EFF DATE IS PAST
      * PN -> WV WHEN EFF DATE IS PAST
      * WV -> EK WHEN EFF DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3150-SET-CHANNEL.
           EVALUATE BCDM-CHANNEL-CD
               WHEN 'EK'
                   IF BCDT-EFF-DATE > 0 AND BCDT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JE' TO BCDM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JE'
                   IF BCDT-EFF-DATE > 0 AND BCDT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PN' TO BCDM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PN'
                   IF BCDT-EFF-DATE > 0 AND BCDT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WV' TO BCDM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WV'
                   IF BCDT-EFF-DATE > 0 AND BCDT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EK' TO BCDM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3160-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF BCDM-LAST-STMT-DATE > 0 AND BCDM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BCDM-INTEREST-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD BCDM-INTEREST-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD BCDM-INTEREST-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD BCDM-INTEREST-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BCDM-INTEREST-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 90 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3170-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF BCDM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO BCDM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PENALTY AMT IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3180-LIMIT-PENALTY.
           IF BCDT-PENALTY-AMT < 0.00
               MOVE 0.00 TO BCDT-PENALTY-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BCDT-PENALTY-AMT > 500.00
               MOVE 500.00 TO BCDT-PENALTY-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3190-DESCRIBE-CHANNEL.
           EVALUATE BCDM-CHANNEL-CD
               WHEN 'EK'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'JE'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'PN'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'WV'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PENALTY AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3200-FLAG-PENALTY.
           IF BCDT-PENALTY-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE BCDT-PENALTY-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3210-DESCRIBE-TRAN.
           EVALUATE BCDT-TRAN-CD
               WHEN 'BU'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'AD'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'WD'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'MT'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT BUMP AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3220-SPLIT-BUMP.
           COMPUTE WS-WORK-AMT = BCDM-BUMP-AMT / 2
           COMPUTE WS-WORK-AMT-2 = BCDM-BUMP-AMT - (WS-WORK-AMT * 2)
           COMPUTE BCDM-BUMP-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3230-COUNT-EXCEPT.
           IF BCDM-EXCEPT-CNT < 9999999
               ADD 1 TO BCDM-EXCEPT-CNT
           END-IF
           IF BCDM-EXCEPT-CNT NOT < 12
               MOVE 'Y' TO BCDT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E605 CUST ID REQUIRED
      * E796 BUMP AMT NEGATIVE
      * E155 BUMP AMT OVER 1000000
      * E350 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3240-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BCDM-CUST-ID = SPACES
                   MOVE 'E605' TO WS-REJECT-CODE
               WHEN BCDM-BUMP-AMT < 0
                   MOVE 'E796' TO WS-REJECT-CODE
               WHEN BCDM-BUMP-AMT > 1000000.00
                   MOVE 'E155' TO WS-REJECT-CODE
               WHEN BCDT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E350' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT BUMP AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3250-SPLIT-BUMP.
           COMPUTE WS-WORK-AMT = BCDT-BUMP-AMT / 2
           COMPUTE WS-WORK-AMT-2 = BCDT-BUMP-AMT - (WS-WORK-AMT * 2)
           COMPUTE BCDM-ADDON-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 15 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3260-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF BCDM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO BCDM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INTEREST AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3270-ACCRUE-INTEREST.
           IF BCDM-INTEREST-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BCDM-INTEREST-AMT * BCDM-RATE / 100 / 365
               ADD WS-WORK-AMT TO BCDM-PRINCIPAL-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ADDON AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PRINCIPAL AMT.
      *----------------------------------------------------------------*
       3280-PRORATE-PRINCIPAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BCDM-PRINCIPAL-AMT ROUNDED =
                   BCDT-ADDON-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BCDM-PRINCIPAL-AMT
           END-IF
           ADD BCDM-PRINCIPAL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3290-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF BCDM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO BCDM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE BUMP BANDX TIER FOR THE PRINCIPAL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3300-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-BUMP-BANDX-RATE(1) TO BCDM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF BCDM-PRINCIPAL-AMT NOT < WS-BUMP-BANDX-LOW(WS-TX)
                   MOVE WS-BUMP-BANDX-RATE(WS-TX) TO BCDM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BCDM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BCDM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PRINCIPAL YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3310-PRORATE-PRINCIPAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BCDM-PRINCIPAL-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE BUMP AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3320-PRORATE-BUMP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   BCDM-BUMP-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE ADDON YTD BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3330-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BCDT-TRAN-CD
                   ADD BCDM-ADDON-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BCDT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BCDM-ADDON-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3340-DESCRIBE-CYCLE.
           EVALUATE BCDM-CYCLE-CD
               WHEN 'A'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'S'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'T'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'U'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'X'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'Y'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3350-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF BCDM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO BCDT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * BUMP AMT IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3360-LIMIT-BUMP.
           IF BCDT-BUMP-AMT < 0.00
               MOVE 0.00 TO BCDT-BUMP-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BCDT-BUMP-AMT > 500.00
               MOVE 500.00 TO BCDT-BUMP-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PENALTY YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3370-FLAG-PENALTY.
           IF BCDM-PENALTY-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE BCDM-PENALTY-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PRINCIPAL AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3380-ACCRUE-PRINCIPAL.
           IF BCDM-PRINCIPAL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BCDM-PRINCIPAL-AMT * BCDM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO BCDM-PENALTY-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3390-DESCRIBE-REGION.
           EVALUATE BCDM-REGION-CD
               WHEN 'BGJ'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'LFX'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'MLA'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'ULA'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3400-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF BCDM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO BCDT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3410-COUNT-ITEM.
           IF BCDM-ITEM-CNT < 9999999
               ADD 1 TO BCDM-ITEM-CNT
           END-IF
           IF BCDM-ITEM-CNT NOT < 6
               MOVE 'Y' TO BCDT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT BUMP YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3420-SPLIT-BUMP.
           COMPUTE WS-WORK-AMT = BCDM-BUMP-YTD / 2
           COMPUTE WS-WORK-AMT-2 = BCDM-BUMP-YTD - (WS-WORK-AMT * 2)
           COMPUTE BCDM-PENALTY-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3430-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF BCDT-EFF-DATE > 0 AND BCDT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE BUMP BANDX TIER FOR THE PENALTY YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3440-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-BUMP-BANDX-RATE(1) TO BCDM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF BCDM-PENALTY-YTD NOT < WS-BUMP-BANDX-LOW(WS-TX)
                   MOVE WS-BUMP-BANDX-RATE(WS-TX) TO BCDM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BCDM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BCDM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3450-COUNT-SEQ.
           IF BCDT-SEQ < 9999999
               ADD 1 TO BCDT-SEQ
           END-IF
           IF BCDT-SEQ NOT < 5
               MOVE 'Y' TO BCDM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3460-LIMIT-ITEM.
           IF WS-WORK-AMT < 100.00
               MOVE 100.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 5000.00
               MOVE 5000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ADDON YTD AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3470-ACCRUE-ADDON.
           IF BCDM-ADDON-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BCDM-ADDON-YTD * BCDM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO BCDM-PENALTY-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PENALTY AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO INTEREST AMT.
      *----------------------------------------------------------------*
       3480-PRORATE-INTEREST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BCDM-INTEREST-AMT ROUNDED =
                   BCDM-PENALTY-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BCDM-INTEREST-AMT
           END-IF
           ADD BCDM-INTEREST-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE PENALTY YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3490-ASSESS-STOP.
           EVALUATE TRUE
               WHEN BCDM-STOP-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BCDM-HOLD-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = BCDM-PENALTY-YTD *
                       BCDM-PENALTY-PCT / 200
               WHEN BCDM-REGION-CD = 'BGJ'
                   MOVE 5.00 TO WS-WORK-AMT
               WHEN BCDM-REGION-CD = 'LFX'
                   MOVE 10.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = BCDM-PENALTY-YTD *
                       BCDM-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 5.00
               MOVE 5.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 50.00
               MOVE 50.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3500-DESCRIBE-CHANNEL.
           EVALUATE BCDM-CHANNEL-CD
               WHEN 'EK'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'JE'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'PN'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'WV'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E476 CUST ID REQUIRED
      * E363 BUMP YTD NEGATIVE
      * E243 BUMP YTD OVER 5000
      * E928 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3510-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BCDM-CUST-ID = SPACES
                   MOVE 'E476' TO WS-REJECT-CODE
               WHEN BCDM-BUMP-YTD < 0
                   MOVE 'E363' TO WS-REJECT-CODE
               WHEN BCDM-BUMP-YTD > 5000.00
                   MOVE 'E243' TO WS-REJECT-CODE
               WHEN BCDM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E928' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3520-COUNT-ITEM.
           IF BCDM-ITEM-CNT < 9999999
               ADD 1 TO BCDM-ITEM-CNT
           END-IF
           IF BCDM-ITEM-CNT NOT < 10
               MOVE 'Y' TO BCDM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT BUMP YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3530-SPLIT-BUMP.
           COMPUTE WS-WORK-AMT = BCDM-BUMP-YTD / 12
           COMPUTE WS-WORK-AMT-2 = BCDM-BUMP-YTD - (WS-WORK-AMT * 12)
           COMPUTE BCDM-PRINCIPAL-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PRINCIPAL FEE ON THE ADDON YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3540-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN BCDM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO BCDM-PRINCIPAL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BCDM-HOLD-FLAG = 'Y'
                   COMPUTE BCDM-PRINCIPAL-AMT ROUNDED = BCDM-ADDON-YTD *
                       BCDM-MARGIN / 200
               WHEN BCDM-ELECTION-CD = 'L'
                   MOVE 12.00 TO BCDM-PRINCIPAL-AMT
               WHEN BCDM-ELECTION-CD = 'M'
                   MOVE 5.00 TO BCDM-PRINCIPAL-AMT
               WHEN OTHER
                   COMPUTE BCDM-PRINCIPAL-AMT ROUNDED = BCDM-ADDON-YTD *
                       BCDM-MARGIN / 100
           END-EVALUATE
           IF BCDM-PRINCIPAL-AMT > 0 AND BCDM-PRINCIPAL-AMT < 1.00
               MOVE 1.00 TO BCDM-PRINCIPAL-AMT
           END-IF
           IF BCDM-PRINCIPAL-AMT > 50.00
               MOVE 50.00 TO BCDM-PRINCIPAL-AMT
           END-IF
           ADD BCDM-PRINCIPAL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT PRINCIPAL YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3550-SPLIT-PRINCIPAL.
           COMPUTE WS-WORK-AMT = BCDM-PRINCIPAL-YTD / 4
           COMPUTE WS-WORK-AMT-2 = BCDM-PRINCIPAL-YTD - (WS-WORK-AMT *
               4)
           COMPUTE BCDM-PRINCIPAL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE BUMP YTD FEE ON THE BUMP AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3560-ASSESS-STOP.
           EVALUATE TRUE
               WHEN BCDM-STOP-FLAG = 'Y'
                   MOVE ZERO TO BCDM-BUMP-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BCDM-EXEMPT-FLAG = 'Y'
                   COMPUTE BCDM-BUMP-YTD ROUNDED = BCDT-BUMP-AMT *
                       BCDM-PENALTY-PCT / 200
               WHEN BCDM-CHANNEL-CD = 'EK'
                   MOVE 12.00 TO BCDM-BUMP-YTD
               WHEN BCDM-CHANNEL-CD = 'JE'
                   MOVE 12.00 TO BCDM-BUMP-YTD
               WHEN OTHER
                   COMPUTE BCDM-BUMP-YTD ROUNDED = BCDT-BUMP-AMT *
                       BCDM-PENALTY-PCT / 100
           END-EVALUATE
           IF BCDM-BUMP-YTD > 0 AND BCDM-BUMP-YTD < 1.00
               MOVE 1.00 TO BCDM-BUMP-YTD
           END-IF
           IF BCDM-BUMP-YTD > 150.00
               MOVE 150.00 TO BCDM-BUMP-YTD
           END-IF
           ADD BCDM-BUMP-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * A -> S WHEN EXPIRY DATE IS PAST
      * S -> T WHEN EXPIRY DATE IS PAST
      * T -> U WHEN EXPIRY DATE IS PAST
      * U -> X WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3570-SET-CYCLE.
           EVALUATE BCDM-CYCLE-CD
               WHEN 'A'
                   IF BCDM-EXPIRY-DATE > 0 AND BCDM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO BCDM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF BCDM-EXPIRY-DATE > 0 AND BCDM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO BCDM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF BCDM-EXPIRY-DATE > 0 AND BCDM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO BCDM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF BCDM-EXPIRY-DATE > 0 AND BCDM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'X' TO BCDM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PENALTY YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3580-ACCRUE-PENALTY.
           IF BCDM-PENALTY-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BCDM-PENALTY-YTD * BCDM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO BCDM-BUMP-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INTEREST AMT IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3590-LIMIT-INTEREST.
           IF BCDM-INTEREST-AMT < 25.00
               MOVE 25.00 TO BCDM-INTEREST-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BCDM-INTEREST-AMT > 10000.00
               MOVE 10000.00 TO BCDM-INTEREST-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3600-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF BCDT-POST-DATE > 0 AND BCDT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BCDM-PRINCIPAL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD BCDM-PRINCIPAL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD BCDM-PRINCIPAL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD BCDM-PRINCIPAL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BCDM-PRINCIPAL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3610-COUNT-EXCEPT.
           IF BCDM-EXCEPT-CNT < 9999999
               ADD 1 TO BCDM-EXCEPT-CNT
           END-IF
           IF BCDM-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO BCDM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3620-DESCRIBE-CHANNEL.
           EVALUATE BCDT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E395 REF NO REQUIRED
      * E248 WORK AMT 2 NEGATIVE
      * E201 WORK AMT 2 OVER 250000
      * E927 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3630-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BCDM-REF-NO = SPACES
                   MOVE 'E395' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E248' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 250000.00
                   MOVE 'E201' TO WS-REJECT-CODE
               WHEN BCDM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E927' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ADDON AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3640-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BCDM-CYCLE-CD
                   ADD BCDM-ADDON-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BCDM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BCDM-ADDON-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ADDON YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO INTEREST YTD.
      *----------------------------------------------------------------*
       3650-PRORATE-INTEREST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BCDM-INTEREST-YTD ROUNDED =
                   BCDM-ADDON-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BCDM-INTEREST-YTD
           END-IF
           ADD BCDM-INTEREST-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PENALTY YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3660-ACCRUE-PENALTY.
           IF BCDM-PENALTY-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BCDM-PENALTY-YTD * BCDM-RATE / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STEP PLAN CD:
      * AYX -> DGU WHEN NEXT DUE DATE IS PAST
      * DGU -> GWJ WHEN NEXT DUE DATE IS PAST
      * GWJ -> RJL WHEN NEXT DUE DATE IS PAST
      * RJL -> UPM WHEN NEXT DUE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3670-SET-STEP-PLAN.
           EVALUATE BCDM-STEP-PLAN-CD
               WHEN 'AYX'
                   IF BCDM-NEXT-DUE-DATE > 0 AND BCDM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DGU' TO BCDM-STEP-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DGU'
                   IF BCDM-NEXT-DUE-DATE > 0 AND BCDM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GWJ' TO BCDM-STEP-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GWJ'
                   IF BCDM-NEXT-DUE-DATE > 0 AND BCDM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RJL' TO BCDM-STEP-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RJL'
                   IF BCDM-NEXT-DUE-DATE > 0 AND BCDM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UPM' TO BCDM-STEP-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE INTEREST FEE ON THE PRINCIPAL YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3680-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN BCDM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO BCDM-INTEREST-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BCDT-REVERSAL-FLAG = 'Y'
                   COMPUTE BCDM-INTEREST-AMT ROUNDED =
                       BCDM-PRINCIPAL-YTD * BCDM-MARGIN / 200
               WHEN BCDM-STEP-PLAN-CD = 'AYX'
                   MOVE 7.00 TO BCDM-INTEREST-AMT
               WHEN BCDM-STEP-PLAN-CD = 'DGU'
                   MOVE 20.00 TO BCDM-INTEREST-AMT
               WHEN OTHER
                   COMPUTE BCDM-INTEREST-AMT ROUNDED =
                       BCDM-PRINCIPAL-YTD * BCDM-MARGIN / 100
           END-EVALUATE
           IF BCDM-INTEREST-AMT > 0 AND BCDM-INTEREST-AMT < 10.00
               MOVE 10.00 TO BCDM-INTEREST-AMT
           END-IF
           IF BCDM-INTEREST-AMT > 35.00
               MOVE 35.00 TO BCDM-INTEREST-AMT
           END-IF
           ADD BCDM-INTEREST-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT PENALTY AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3690-SPLIT-PENALTY.
           COMPUTE WS-WORK-AMT = BCDM-PENALTY-AMT / 4
           COMPUTE WS-WORK-AMT-2 = BCDM-PENALTY-AMT - (WS-WORK-AMT * 4)
           COMPUTE BCDM-PRINCIPAL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PENALTY AMT IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3700-LIMIT-PENALTY.
           IF BCDM-PENALTY-AMT < 25.00
               MOVE 25.00 TO BCDM-PENALTY-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BCDM-PENALTY-AMT > 1000.00
               MOVE 1000.00 TO BCDM-PENALTY-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3710-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF BCDT-EFF-DATE > 0 AND BCDT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BCDM-BUMP-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD BCDM-BUMP-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD BCDM-BUMP-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD BCDM-BUMP-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BCDM-BUMP-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE PENALTY FEE ON THE PRINCIPAL YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3720-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN BCDT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO BCDM-PENALTY-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BCDM-OVERRIDE-FLAG = 'Y'
                   COMPUTE BCDM-PENALTY-AMT ROUNDED = BCDM-PRINCIPAL-YTD
                       * BCDM-MARGIN / 200
               WHEN BCDM-SOURCE-CD = 'AG'
                   MOVE 5.00 TO BCDM-PENALTY-AMT
               WHEN BCDM-SOURCE-CD = 'BF'
                   MOVE 12.00 TO BCDM-PENALTY-AMT
               WHEN OTHER
                   COMPUTE BCDM-PENALTY-AMT ROUNDED = BCDM-PRINCIPAL-YTD
                       * BCDM-MARGIN / 100
           END-EVALUATE
           IF BCDM-PENALTY-AMT > 0 AND BCDM-PENALTY-AMT < 5.00
               MOVE 5.00 TO BCDM-PENALTY-AMT
           END-IF
           IF BCDM-PENALTY-AMT > 35.00
               MOVE 35.00 TO BCDM-PENALTY-AMT
           END-IF
           ADD BCDM-PENALTY-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PENALTY YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3730-FLAG-PENALTY.
           IF BCDM-PENALTY-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE BCDM-PENALTY-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE BUMP BAND TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3740-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-BUMP-BAND-RATE(1) TO BCDM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF WS-WORK-AMT NOT < WS-BUMP-BAND-LOW(WS-TX)
                   MOVE WS-BUMP-BAND-RATE(WS-TX) TO BCDM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BCDM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BCDM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * EK -> JE WHEN MATURITY DATE IS PAST
      * JE -> PN WHEN MATURITY DATE IS PAST
      * PN -> WV WHEN MATURITY DATE IS PAST
      * WV -> EK WHEN MATURITY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3750-SET-CHANNEL.
           EVALUATE BCDM-CHANNEL-CD
               WHEN 'EK'
                   IF BCDM-MATURITY-DATE > 0 AND BCDM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JE' TO BCDM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JE'
                   IF BCDM-MATURITY-DATE > 0 AND BCDM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PN' TO BCDM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PN'
                   IF BCDM-MATURITY-DATE > 0 AND BCDM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WV' TO BCDM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WV'
                   IF BCDM-MATURITY-DATE > 0 AND BCDM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EK' TO BCDM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT ADDON AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3760-SPLIT-ADDON.
           COMPUTE WS-WORK-AMT = BCDM-ADDON-AMT / 3
           COMPUTE WS-WORK-AMT-2 = BCDM-ADDON-AMT - (WS-WORK-AMT * 3)
           COMPUTE BCDM-BUMP-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRINCIPAL YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3770-FLAG-PRINCIPAL.
           IF BCDM-PRINCIPAL-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE BCDM-PRINCIPAL-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE BUMP BAND TIER FOR THE INTEREST YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3780-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-BUMP-BAND-RATE(1) TO BCDM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF BCDM-INTEREST-YTD NOT < WS-BUMP-BAND-LOW(WS-TX)
                   MOVE WS-BUMP-BAND-RATE(WS-TX) TO BCDM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BCDM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BCDM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE INTEREST AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3790-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BCDM-CHANNEL-CD
                   ADD BCDT-INTEREST-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BCDM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BCDT-INTEREST-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT BUMP AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3800-SPLIT-BUMP.
           COMPUTE WS-WORK-AMT = BCDM-BUMP-AMT / 2
           COMPUTE WS-WORK-AMT-2 = BCDM-BUMP-AMT - (WS-WORK-AMT * 2)
           COMPUTE BCDM-INTEREST-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3810-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF BCDM-LAST-MAINT-DATE > 0 AND BCDM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BCDM-ADDON-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD BCDM-ADDON-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD BCDM-ADDON-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD BCDM-ADDON-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BCDM-ADDON-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3820-DESCRIBE-REGION.
           EVALUATE BCDM-REGION-CD
               WHEN 'BGJ'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'LFX'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'MLA'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ULA'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT BUMP AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3830-SPLIT-BUMP.
           COMPUTE WS-WORK-AMT = BCDM-BUMP-AMT / 2
           COMPUTE WS-WORK-AMT-2 = BCDM-BUMP-AMT - (WS-WORK-AMT * 2)
           COMPUTE BCDT-PENALTY-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * AG -> BF WHEN EFF DATE IS PAST
      * BF -> HF WHEN EFF DATE IS PAST
      * HF -> HR WHEN EFF DATE IS PAST
      * HR -> UF WHEN EFF DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3840-SET-SOURCE.
           EVALUATE BCDM-SOURCE-CD
               WHEN 'AG'
                   IF BCDT-EFF-DATE > 0 AND BCDT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BF' TO BCDM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BF'
                   IF BCDT-EFF-DATE > 0 AND BCDT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HF' TO BCDM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HF'
                   IF BCDT-EFF-DATE > 0 AND BCDT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HR' TO BCDM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HR'
                   IF BCDT-EFF-DATE > 0 AND BCDT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UF' TO BCDM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3850-COUNT-ITEM.
           IF BCDM-ITEM-CNT < 9999999
               ADD 1 TO BCDM-ITEM-CNT
           END-IF
           IF BCDM-ITEM-CNT NOT < 12
               MOVE 'Y' TO BCDT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3860-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF BCDM-LAST-STMT-DATE > 0 AND BCDM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BCDT-INTEREST-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD BCDT-INTEREST-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD BCDT-INTEREST-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD BCDT-INTEREST-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BCDT-INTEREST-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE BUMP YTD FEE ON THE PENALTY AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3870-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN BCDM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO BCDM-BUMP-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BCDM-SUSPEND-FLAG = 'Y'
                   COMPUTE BCDM-BUMP-YTD ROUNDED = BCDT-PENALTY-AMT *
                       BCDM-MARGIN / 200
               WHEN BCDM-ELECTION-CD = 'L'
                   MOVE 20.00 TO BCDM-BUMP-YTD
               WHEN BCDM-ELECTION-CD = 'M'
                   MOVE 20.00 TO BCDM-BUMP-YTD
               WHEN OTHER
                   COMPUTE BCDM-BUMP-YTD ROUNDED = BCDT-PENALTY-AMT *
                       BCDM-MARGIN / 100
           END-EVALUATE
           IF BCDM-BUMP-YTD > 0 AND BCDM-BUMP-YTD < 5.00
               MOVE 5.00 TO BCDM-BUMP-YTD
           END-IF
           IF BCDM-BUMP-YTD > 25.00
               MOVE 25.00 TO BCDM-BUMP-YTD
           END-IF
           ADD BCDM-BUMP-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE BUMP YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3880-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BCDM-REGION-CD
                   ADD BCDM-BUMP-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BCDM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BCDM-BUMP-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3890-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF BCDM-MATURITY-DATE > 0 AND BCDM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BCDT-PENALTY-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD BCDT-PENALTY-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD BCDT-PENALTY-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD BCDT-PENALTY-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BCDT-PENALTY-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E598 DESC REQUIRED
      * E137 PRINCIPAL AMT NEGATIVE
      * E514 PRINCIPAL AMT OVER 5000
      * E872 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3900-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BCDM-DESC = SPACES
                   MOVE 'E598' TO WS-REJECT-CODE
               WHEN BCDM-PRINCIPAL-AMT < 0
                   MOVE 'E137' TO WS-REJECT-CODE
               WHEN BCDM-PRINCIPAL-AMT > 5000.00
                   MOVE 'E514' TO WS-REJECT-CODE
               WHEN BCDM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E872' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ADDON AMT BY STEP PLAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3910-ACCUM-STEP-PLAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BCDM-STEP-PLAN-CD
                   ADD BCDM-ADDON-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BCDM-STEP-PLAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BCDM-ADDON-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3920-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF BCDM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO BCDM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * AG -> BF WHEN EXPIRY DATE IS PAST
      * BF -> HF WHEN EXPIRY DATE IS PAST
      * HF -> HR WHEN EXPIRY DATE IS PAST
      * HR -> UF WHEN EXPIRY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3930-SET-SOURCE.
           EVALUATE BCDM-SOURCE-CD
               WHEN 'AG'
                   IF BCDM-EXPIRY-DATE > 0 AND BCDM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BF' TO BCDM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BF'
                   IF BCDM-EXPIRY-DATE > 0 AND BCDM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HF' TO BCDM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HF'
                   IF BCDM-EXPIRY-DATE > 0 AND BCDM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HR' TO BCDM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HR'
                   IF BCDM-EXPIRY-DATE > 0 AND BCDM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UF' TO BCDM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E615 REF NO REQUIRED
      * E797 PRINCIPAL AMT NEGATIVE
      * E799 PRINCIPAL AMT OVER 25000
      * E469 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3940-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BCDM-REF-NO = SPACES
                   MOVE 'E615' TO WS-REJECT-CODE
               WHEN BCDM-PRINCIPAL-AMT < 0
                   MOVE 'E797' TO WS-REJECT-CODE
               WHEN BCDM-PRINCIPAL-AMT > 25000.00
                   MOVE 'E799' TO WS-REJECT-CODE
               WHEN BCDM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E469' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3950-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF BCDM-LAST-STMT-DATE > 0 AND BCDM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BCDM-INTEREST-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD BCDM-INTEREST-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD BCDM-INTEREST-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD BCDM-INTEREST-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BCDM-INTEREST-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3960-COUNT-EXCEPT.
           IF BCDM-EXCEPT-CNT < 9999999
               ADD 1 TO BCDM-EXCEPT-CNT
           END-IF
           IF BCDM-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO BCDM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE BUMP BANDX TIER FOR THE ADDON AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3970-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-BUMP-BANDX-RATE(1) TO BCDM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF BCDM-ADDON-AMT NOT < WS-BUMP-BANDX-LOW(WS-TX)
                   MOVE WS-BUMP-BANDX-RATE(WS-TX) TO BCDM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BCDM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BCDM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * AG -> BF WHEN MATURITY DATE IS PAST
      * BF -> HF WHEN MATURITY DATE IS PAST
      * HF -> HR WHEN MATURITY DATE IS PAST
      * HR -> UF WHEN MATURITY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3980-SET-SOURCE.
           EVALUATE BCDM-SOURCE-CD
               WHEN 'AG'
                   IF BCDM-MATURITY-DATE > 0 AND BCDM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BF' TO BCDM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BF'
                   IF BCDM-MATURITY-DATE > 0 AND BCDM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HF' TO BCDM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HF'
                   IF BCDM-MATURITY-DATE > 0 AND BCDM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HR' TO BCDM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HR'
                   IF BCDM-MATURITY-DATE > 0 AND BCDM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UF' TO BCDM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3990-COUNT-ITEM.
           IF BCDM-ITEM-CNT < 9999999
               ADD 1 TO BCDM-ITEM-CNT
           END-IF
           IF BCDM-ITEM-CNT NOT < 6
               MOVE 'Y' TO BCDM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE BUMP AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO BUMP YTD.
      *----------------------------------------------------------------*
       4000-PRORATE-BUMP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BCDM-BUMP-YTD ROUNDED =
                   BCDM-BUMP-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BCDM-BUMP-YTD
           END-IF
           ADD BCDM-BUMP-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ADDON YTD IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4010-LIMIT-ADDON.
           IF BCDM-ADDON-YTD < 10.00
               MOVE 10.00 TO BCDM-ADDON-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BCDM-ADDON-YTD > 500.00
               MOVE 500.00 TO BCDM-ADDON-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE BUMP BANDX TIER FOR THE INTEREST AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4020-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-BUMP-BANDX-RATE(1) TO BCDM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF BCDM-INTEREST-AMT NOT < WS-BUMP-BANDX-LOW(WS-TX)
                   MOVE WS-BUMP-BANDX-RATE(WS-TX) TO BCDM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BCDM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BCDM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4030-DESCRIBE-REGION.
           EVALUATE BCDM-REGION-CD
               WHEN 'BGJ'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'LFX'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'MLA'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'ULA'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 120 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4040-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF BCDT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO BCDM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4050-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF BCDM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO BCDM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE BUMP AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PENALTY YTD.
      *----------------------------------------------------------------*
       4060-PRORATE-PENALTY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BCDM-PENALTY-YTD ROUNDED =
                   BCDM-BUMP-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BCDM-PENALTY-YTD
           END-IF
           ADD BCDM-PENALTY-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * BUMP AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4070-FLAG-BUMP.
           IF BCDT-BUMP-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE BCDT-BUMP-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ADDON AMT IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4080-LIMIT-ADDON.
           IF BCDM-ADDON-AMT < 50.00
               MOVE 50.00 TO BCDM-ADDON-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BCDM-ADDON-AMT > 10000.00
               MOVE 10000.00 TO BCDM-ADDON-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE BUMP BAND TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4090-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-BUMP-BAND-RATE(1) TO BCDM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF WS-WORK-AMT-2 NOT < WS-BUMP-BAND-LOW(WS-TX)
                   MOVE WS-BUMP-BAND-RATE(WS-TX) TO BCDM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BCDM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BCDM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4100-COUNT-RETRY.
           IF BCDM-RETRY-CNT < 9999999
               ADD 1 TO BCDM-RETRY-CNT
           END-IF
           IF BCDM-RETRY-CNT NOT < 10
               MOVE 'Y' TO BCDM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4110-DESCRIBE-REGION.
           EVALUATE BCDM-REGION-CD
               WHEN 'BGJ'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'LFX'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'MLA'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'ULA'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE PRINCIPAL AMT BY STEP PLAN CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       4120-ACCUM-STEP-PLAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BCDM-STEP-PLAN-CD
                   ADD BCDM-PRINCIPAL-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BCDM-STEP-PLAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BCDM-PRINCIPAL-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4130-DESCRIBE-CYCLE.
           EVALUATE BCDM-CYCLE-CD
               WHEN 'A'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'S'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'T'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'U'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'X'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'Y'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE ADDON AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4140-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BCDM-SOURCE-CD
                   ADD BCDM-ADDON-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BCDM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BCDM-ADDON-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * EK -> JE WHEN LAST PAY DATE IS PAST
      * JE -> PN WHEN LAST PAY DATE IS PAST
      * PN -> WV WHEN LAST PAY DATE IS PAST
      * WV -> EK WHEN LAST PAY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4150-SET-CHANNEL.
           EVALUATE BCDM-CHANNEL-CD
               WHEN 'EK'
                   IF BCDM-LAST-PAY-DATE > 0 AND BCDM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JE' TO BCDM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JE'
                   IF BCDM-LAST-PAY-DATE > 0 AND BCDM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PN' TO BCDM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PN'
                   IF BCDM-LAST-PAY-DATE > 0 AND BCDM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WV' TO BCDM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WV'
                   IF BCDM-LAST-PAY-DATE > 0 AND BCDM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EK' TO BCDM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PENALTY AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4160-FLAG-PENALTY.
           IF BCDT-PENALTY-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE BCDT-PENALTY-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PENALTY AMT BY STEP PLAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4170-ACCUM-STEP-PLAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BCDM-STEP-PLAN-CD
                   ADD BCDT-PENALTY-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BCDM-STEP-PLAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BCDT-PENALTY-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE ELECTION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4180-DESCRIBE-ELECTION.
           EVALUATE BCDM-ELECTION-CD
               WHEN 'L'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'M'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'N'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'R'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'V'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E736 CUST ID REQUIRED
      * E286 PENALTY YTD NEGATIVE
      * E877 PENALTY YTD OVER 25000
      * E262 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       4190-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BCDM-CUST-ID = SPACES
                   MOVE 'E736' TO WS-REJECT-CODE
               WHEN BCDM-PENALTY-YTD < 0
                   MOVE 'E286' TO WS-REJECT-CODE
               WHEN BCDM-PENALTY-YTD > 25000.00
                   MOVE 'E877' TO WS-REJECT-CODE
               WHEN BCDM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E262' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PRINCIPAL YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4200-SPLIT-PRINCIPAL.
           COMPUTE WS-WORK-AMT = BCDM-PRINCIPAL-YTD / 4
           COMPUTE WS-WORK-AMT-2 = BCDM-PRINCIPAL-YTD - (WS-WORK-AMT *
               4)
           COMPUTE BCDT-BUMP-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       4210-COUNT-ITEM.
           IF BCDM-ITEM-CNT < 9999999
               ADD 1 TO BCDM-ITEM-CNT
           END-IF
           IF BCDM-ITEM-CNT NOT < 10
               MOVE 'Y' TO BCDM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PRINCIPAL AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4220-ACCRUE-PRINCIPAL.
           IF BCDM-PRINCIPAL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BCDM-PRINCIPAL-AMT * BCDM-RATE / 100 / 365
               ADD WS-WORK-AMT TO BCDM-BUMP-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PRINCIPAL YTD FEE ON THE PENALTY YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4230-ASSESS-STOP.
           EVALUATE TRUE
               WHEN BCDM-STOP-FLAG = 'Y'
                   MOVE ZERO TO BCDM-PRINCIPAL-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BCDM-OVERRIDE-FLAG = 'Y'
                   COMPUTE BCDM-PRINCIPAL-YTD ROUNDED = BCDM-PENALTY-YTD
                       * BCDM-PENALTY-PCT / 200
               WHEN BCDT-TRAN-CD = 'BU'
                   MOVE 5.00 TO BCDM-PRINCIPAL-YTD
               WHEN BCDT-TRAN-CD = 'AD'
                   MOVE 10.00 TO BCDM-PRINCIPAL-YTD
               WHEN OTHER
                   COMPUTE BCDM-PRINCIPAL-YTD ROUNDED = BCDM-PENALTY-YTD
                       * BCDM-PENALTY-PCT / 100
           END-EVALUATE
           IF BCDM-PRINCIPAL-YTD > 0 AND BCDM-PRINCIPAL-YTD < 1.00
               MOVE 1.00 TO BCDM-PRINCIPAL-YTD
           END-IF
           IF BCDM-PRINCIPAL-YTD > 50.00
               MOVE 50.00 TO BCDM-PRINCIPAL-YTD
           END-IF
           ADD BCDM-PRINCIPAL-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE PENALTY YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4240-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BCDT-CHANNEL-CD
                   ADD BCDM-PENALTY-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BCDT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BCDM-PENALTY-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4250-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF BCDM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO BCDM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4260-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF BCDM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO BCDM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRINCIPAL AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4270-FLAG-PRINCIPAL.
           IF BCDM-PRINCIPAL-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE BCDM-PRINCIPAL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E814 NAME REQUIRED
      * E685 INTEREST AMT NEGATIVE
      * E061 INTEREST AMT OVER 25000
      * E467 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       4280-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BCDM-NAME = SPACES
                   MOVE 'E814' TO WS-REJECT-CODE
               WHEN BCDM-INTEREST-AMT < 0
                   MOVE 'E685' TO WS-REJECT-CODE
               WHEN BCDM-INTEREST-AMT > 25000.00
                   MOVE 'E061' TO WS-REJECT-CODE
               WHEN BCDM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E467' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ADDON AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4290-ACCRUE-ADDON.
           IF BCDT-ADDON-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BCDT-ADDON-AMT * BCDM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO BCDM-PRINCIPAL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE INTEREST YTD FEE ON THE PRINCIPAL AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4300-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN BCDM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO BCDM-INTEREST-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BCDM-EXEMPT-FLAG = 'Y'
                   COMPUTE BCDM-INTEREST-YTD ROUNDED =
                       BCDM-PRINCIPAL-AMT * BCDM-MARGIN / 200
               WHEN BCDM-ELECTION-CD = 'L'
                   MOVE 10.00 TO BCDM-INTEREST-YTD
               WHEN BCDM-ELECTION-CD = 'M'
                   MOVE 20.00 TO BCDM-INTEREST-YTD
               WHEN OTHER
                   COMPUTE BCDM-INTEREST-YTD ROUNDED =
                       BCDM-PRINCIPAL-AMT * BCDM-MARGIN / 100
           END-EVALUATE
           IF BCDM-INTEREST-YTD > 0 AND BCDM-INTEREST-YTD < 10.00
               MOVE 10.00 TO BCDM-INTEREST-YTD
           END-IF
           IF BCDM-INTEREST-YTD > 25.00
               MOVE 25.00 TO BCDM-INTEREST-YTD
           END-IF
           ADD BCDM-INTEREST-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 10 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4310-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF BCDM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO BCDM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE ADDON YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4320-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN BCDM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BCDM-OVERRIDE-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = BCDM-ADDON-YTD *
                       BCDM-RATE / 200
               WHEN BCDM-STEP-PLAN-CD = 'AYX'
                   MOVE 3.00 TO WS-WORK-AMT-2
               WHEN BCDM-STEP-PLAN-CD = 'DGU'
                   MOVE 3.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = BCDM-ADDON-YTD *
                       BCDM-RATE / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 1.00
               MOVE 1.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE PRINCIPAL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO BUMP YTD.
      *----------------------------------------------------------------*
       4330-PRORATE-BUMP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BCDM-BUMP-YTD ROUNDED =
                   BCDM-PRINCIPAL-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BCDM-BUMP-YTD
           END-IF
           ADD BCDM-BUMP-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4340-DESCRIBE-SOURCE.
           EVALUATE BCDM-SOURCE-CD
               WHEN 'AG'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'BF'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'HF'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'HR'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'UF'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'YD'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * BUMP AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4350-FLAG-BUMP.
           IF BCDM-BUMP-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE BCDM-BUMP-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE BUMP BAND TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4360-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-BUMP-BAND-RATE(1) TO BCDM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF WS-WORK-AMT-2 NOT < WS-BUMP-BAND-LOW(WS-TX)
                   MOVE WS-BUMP-BAND-RATE(WS-TX) TO BCDM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BCDM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BCDM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PENALTY YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4370-ACCRUE-PENALTY.
           IF BCDM-PENALTY-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BCDM-PENALTY-YTD * BCDM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO BCDM-ADDON-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE BUMP AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4380-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BCDT-CHANNEL-CD
                   ADD BCDT-BUMP-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BCDT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BCDT-BUMP-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE INTEREST AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ADDON YTD.
      *----------------------------------------------------------------*
       4390-PRORATE-ADDON.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BCDM-ADDON-YTD ROUNDED =
                   BCDM-INTEREST-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BCDM-ADDON-YTD
           END-IF
           ADD BCDM-ADDON-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * BUMP AMT IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4400-LIMIT-BUMP.
           IF BCDT-BUMP-AMT < 0.00
               MOVE 0.00 TO BCDT-BUMP-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BCDT-BUMP-AMT > 10000.00
               MOVE 10000.00 TO BCDT-BUMP-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE BUMP FEE ON THE PRINCIPAL YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4410-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN BCDM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO BCDT-BUMP-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BCDM-STOP-FLAG = 'Y'
                   COMPUTE BCDT-BUMP-AMT ROUNDED = BCDM-PRINCIPAL-YTD *
                       BCDM-PENALTY-PCT / 200
               WHEN BCDM-ELECTION-CD = 'L'
                   MOVE 12.00 TO BCDT-BUMP-AMT
               WHEN BCDM-ELECTION-CD = 'M'
                   MOVE 15.00 TO BCDT-BUMP-AMT
               WHEN OTHER
                   COMPUTE BCDT-BUMP-AMT ROUNDED = BCDM-PRINCIPAL-YTD *
                       BCDM-PENALTY-PCT / 100
           END-EVALUATE
           IF BCDT-BUMP-AMT > 0 AND BCDT-BUMP-AMT < 5.00
               MOVE 5.00 TO BCDT-BUMP-AMT
           END-IF
           IF BCDT-BUMP-AMT > 100.00
               MOVE 100.00 TO BCDT-BUMP-AMT
           END-IF
           ADD BCDT-BUMP-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE PENALTY AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4420-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BCDM-CHANNEL-CD
                   ADD BCDM-PENALTY-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BCDM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BCDM-PENALTY-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * INTEREST YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4430-FLAG-INTEREST.
           IF BCDM-INTEREST-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE BCDM-INTEREST-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4440-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF BCDM-LAST-STMT-DATE > 0 AND BCDM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BCDM-PRINCIPAL-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD BCDM-PRINCIPAL-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD BCDM-PRINCIPAL-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD BCDM-PRINCIPAL-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BCDM-PRINCIPAL-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 10 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4450-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF BCDM-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO BCDM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STEP PLAN CD:
      * AYX -> DGU WHEN CLOSE DATE IS PAST
      * DGU -> GWJ WHEN CLOSE DATE IS PAST
      * GWJ -> RJL WHEN CLOSE DATE IS PAST
      * RJL -> UPM WHEN CLOSE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4460-SET-STEP-PLAN.
           EVALUATE BCDM-STEP-PLAN-CD
               WHEN 'AYX'
                   IF BCDM-CLOSE-DATE > 0 AND BCDM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DGU' TO BCDM-STEP-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DGU'
                   IF BCDM-CLOSE-DATE > 0 AND BCDM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GWJ' TO BCDM-STEP-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GWJ'
                   IF BCDM-CLOSE-DATE > 0 AND BCDM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RJL' TO BCDM-STEP-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RJL'
                   IF BCDM-CLOSE-DATE > 0 AND BCDM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UPM' TO BCDM-STEP-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT PRINCIPAL AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4470-SPLIT-PRINCIPAL.
           COMPUTE WS-WORK-AMT = BCDM-PRINCIPAL-AMT / 2
           COMPUTE WS-WORK-AMT-2 = BCDM-PRINCIPAL-AMT - (WS-WORK-AMT *
               2)
           COMPUTE BCDT-INTEREST-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ELECTION CD:
      * L -> M WHEN NEXT REVIEW DATE IS PAST
      * M -> N WHEN NEXT REVIEW DATE IS PAST
      * N -> R WHEN NEXT REVIEW DATE IS PAST
      * R -> V WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4480-SET-ELECTION.
           EVALUATE BCDM-ELECTION-CD
               WHEN 'L'
                   IF BCDM-NEXT-REVIEW-DATE > 0 AND
                       BCDM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'M' TO BCDM-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF BCDM-NEXT-REVIEW-DATE > 0 AND
                       BCDM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'N' TO BCDM-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'N'
                   IF BCDM-NEXT-REVIEW-DATE > 0 AND
                       BCDM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'R' TO BCDM-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF BCDM-NEXT-REVIEW-DATE > 0 AND
                       BCDM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'V' TO BCDM-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE BUMP BANDX TIER FOR THE INTEREST YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4490-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-BUMP-BANDX-RATE(1) TO BCDM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF BCDM-INTEREST-YTD NOT < WS-BUMP-BANDX-LOW(WS-TX)
                   MOVE WS-BUMP-BANDX-RATE(WS-TX) TO BCDM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BCDM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BCDM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE BUMP BAND TIER FOR THE INTEREST AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4500-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-BUMP-BAND-RATE(1) TO BCDM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF BCDT-INTEREST-AMT NOT < WS-BUMP-BAND-LOW(WS-TX)
                   MOVE WS-BUMP-BAND-RATE(WS-TX) TO BCDM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BCDM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BCDM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 180 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4510-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF BCDM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO BCDM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PENALTY FEE ON THE BUMP YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       4520-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN BCDM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO BCDT-PENALTY-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BCDM-OVERRIDE-FLAG = 'Y'
                   COMPUTE BCDT-PENALTY-AMT ROUNDED = BCDM-BUMP-YTD *
                       BCDM-MARGIN / 200
               WHEN BCDM-ELECTION-CD = 'L'
                   MOVE 12.00 TO BCDT-PENALTY-AMT
               WHEN BCDM-ELECTION-CD = 'M'
                   MOVE 7.00 TO BCDT-PENALTY-AMT
               WHEN OTHER
                   COMPUTE BCDT-PENALTY-AMT ROUNDED = BCDM-BUMP-YTD *
                       BCDM-MARGIN / 100
           END-EVALUATE
           IF BCDT-PENALTY-AMT > 0 AND BCDT-PENALTY-AMT < 1.00
               MOVE 1.00 TO BCDT-PENALTY-AMT
           END-IF
           IF BCDT-PENALTY-AMT > 150.00
               MOVE 150.00 TO BCDT-PENALTY-AMT
           END-IF
           ADD BCDT-PENALTY-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT ADDON AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4530-SPLIT-ADDON.
           COMPUTE WS-WORK-AMT = BCDT-ADDON-AMT / 3
           COMPUTE WS-WORK-AMT-2 = BCDT-ADDON-AMT - (WS-WORK-AMT * 3)
           COMPUTE BCDM-BUMP-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4540-DESCRIBE-REGION.
           EVALUATE BCDM-REGION-CD
               WHEN 'BGJ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'LFX'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'MLA'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'ULA'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4550-COUNT-EXCEPT.
           IF BCDM-EXCEPT-CNT < 9999999
               ADD 1 TO BCDM-EXCEPT-CNT
           END-IF
           IF BCDM-EXCEPT-CNT NOT < 12
               MOVE 'Y' TO BCDM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4560-COUNT-RETRY.
           IF BCDM-RETRY-CNT < 9999999
               ADD 1 TO BCDM-RETRY-CNT
           END-IF
           IF BCDM-RETRY-CNT NOT < 3
               MOVE 'Y' TO BCDT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       4570-COUNT-EXCEPT.
           IF BCDM-EXCEPT-CNT < 9999999
               ADD 1 TO BCDM-EXCEPT-CNT
           END-IF
           IF BCDM-EXCEPT-CNT NOT < 12
               MOVE 'Y' TO BCDM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ADDON AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4580-ACCRUE-ADDON.
           IF BCDT-ADDON-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BCDT-ADDON-AMT * BCDM-RATE / 100 / 360
               ADD WS-WORK-AMT TO BCDT-PENALTY-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E196 ADDR REQUIRED
      * E584 ADDON AMT NEGATIVE
      * E556 ADDON AMT OVER 5000
      * E514 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       4590-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BCDM-ADDR = SPACES
                   MOVE 'E196' TO WS-REJECT-CODE
               WHEN BCDT-ADDON-AMT < 0
                   MOVE 'E584' TO WS-REJECT-CODE
               WHEN BCDT-ADDON-AMT > 5000.00
                   MOVE 'E556' TO WS-REJECT-CODE
               WHEN BCDM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E514' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * INTEREST AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4600-FLAG-INTEREST.
           IF BCDM-INTEREST-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE BCDM-INTEREST-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE BUMP AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PENALTY AMT.
      *----------------------------------------------------------------*
       4610-PRORATE-PENALTY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BCDM-PENALTY-AMT ROUNDED =
                   BCDT-BUMP-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BCDM-PENALTY-AMT
           END-IF
           ADD BCDM-PENALTY-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4620-DESCRIBE-TRAN.
           EVALUATE BCDT-TRAN-CD
               WHEN 'BU'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'AD'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'WD'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'MT'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4630-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF BCDM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO BCDM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4640-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF BCDM-LAST-MAINT-DATE > 0 AND BCDM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BCDT-BUMP-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD BCDT-BUMP-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD BCDT-BUMP-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD BCDT-BUMP-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BCDT-BUMP-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4650-ACCRUE-MARGIN.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * BCDM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO BCDT-ADDON-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE BUMP FEE ON THE ADDON AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4660-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN BCDT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO BCDT-BUMP-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BCDM-SUSPEND-FLAG = 'Y'
                   COMPUTE BCDT-BUMP-AMT ROUNDED = BCDT-ADDON-AMT *
                       BCDM-MARGIN / 200
               WHEN BCDM-REGION-CD = 'BGJ'
                   MOVE 7.00 TO BCDT-BUMP-AMT
               WHEN BCDM-REGION-CD = 'LFX'
                   MOVE 12.00 TO BCDT-BUMP-AMT
               WHEN OTHER
                   COMPUTE BCDT-BUMP-AMT ROUNDED = BCDT-ADDON-AMT *
                       BCDM-MARGIN / 100
           END-EVALUATE
           IF BCDT-BUMP-AMT > 0 AND BCDT-BUMP-AMT < 1.00
               MOVE 1.00 TO BCDT-BUMP-AMT
           END-IF
           IF BCDT-BUMP-AMT > 100.00
               MOVE 100.00 TO BCDT-BUMP-AMT
           END-IF
           ADD BCDT-BUMP-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PASS THE BCD FIELDS TO BUTIER AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7040-CALL-BUTIER.
           INITIALIZE BUTIER-PARMS
           MOVE BCDM-PENALTY-YTD
             TO BUTIER-PENALTY-YTD
           MOVE BCDM-ADDON-AMT
             TO BUTIER-ADDON-AMT
           MOVE BCDM-BUMP-AMT
             TO BUTIER-BUMP-AMT
           MOVE BCDM-PRINCIPAL-AMT
             TO BUTIER-PRINCIPAL-AMT
           MOVE BCDM-INTEREST-AMT
             TO BUTIER-INTEREST-AMT
           MOVE BCDM-PRINCIPAL-YTD
             TO BUTIER-PRINCIPAL-YTD
           MOVE BCDM-NEXT-DUE-DATE
             TO BUTIER-NEXT-DUE-DATE
           MOVE BCDM-CLOSE-DATE
             TO BUTIER-CLOSE-DATE
           MOVE BCDM-EFFECTIVE-DATE
             TO BUTIER-EFFECTIVE-DATE
           MOVE BCDM-MATURITY-DATE
             TO BUTIER-MATURITY-DATE
           MOVE BCDM-REGION-CD
             TO BUTIER-REGION-CD
           MOVE BCDM-CYCLE-CD
             TO BUTIER-CYCLE-CD
           MOVE BCDM-CHANNEL-CD
             TO BUTIER-CHANNEL-CD
           MOVE BCDM-SOURCE-CD
             TO BUTIER-SOURCE-CD
           MOVE BCDM-REVIEW-FLAG
             TO BUTIER-REVIEW-FLAG
           MOVE BCDM-EXEMPT-FLAG
             TO BUTIER-EXEMPT-FLAG
           MOVE BCDM-OVERRIDE-FLAG
             TO BUTIER-OVERRIDE-FLAG
           MOVE BCDM-EXCEPT-CNT
             TO BUTIER-EXCEPT-CNT
           MOVE BCDM-RETRY-CNT
             TO BUTIER-RETRY-CNT
           MOVE BCDM-MARGIN
             TO BUTIER-MARGIN
           MOVE BCDM-RATE
             TO BUTIER-RATE
           MOVE BCDM-CUST-ID
             TO BUTIER-CUST-ID
           MOVE BCDM-REF-NO
             TO BUTIER-REF-NO
           CALL 'BUTIER' USING BUTIER-PARMS
           EVALUATE BUTIER-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE BUTIER-PRINCIPAL-YTD
                     TO BCDM-PRINCIPAL-YTD
                   CONTINUE
               WHEN 08
                   MOVE 'RIER' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7040-CALL-BUTIER' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE BCDT-KEY TO RD-KEY
           MOVE BCDT-TRAN-CD TO RD-CODE
           MOVE BCDT-INTEREST-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE BCDT-KEY TO RD-KEY
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
           DISPLAY 'BUDSTMT READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'BUDSTMT' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
