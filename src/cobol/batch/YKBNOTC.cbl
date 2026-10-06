      *================================================================*
      * PROGRAM  : YKBNOTC                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : DATE WINDOW BRIDGE - CUSTOMER NOTICES. READS THE YDB*
      *            TRANSACTIONS, UPDATES THE YDB MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. TWO-DIGIT YEAR WINDOWING BRIDGE *
      *            FOR CENTURY DATE CONVERSION                         *
      * INPUT    : YDBMST (KSDS), YDBTRN, BANKCTL                      *
      * OUTPUT   : YDBOUT, YKBNORPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, YKLIM, YKWAIV, YKPRC    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-07-15 RJT  ORIGINAL                                       *
      * 1993-06-11 TWH  CONTROL EXCEPTIONS TO OPS QUEUE REQ3745        *
      * 1995-12-01 PQR  CONTROL TABLE EXPANDED CHG9947                 *
      * 1995-01-18 TWH  AUDIT FINDING - AMOUNT-2 CONTROL TOTALS CHG0534*
      * 1999-11-13 PQR  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-03-05 BHS  AMOUNT-1 EXCEPTIONS TO OPS QUEUE INC8396       *
      * 2006-05-21 JLM  AMOUNT-2 CALCULATION CORRECTED CHG8428         *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YKBNOTC.
       AUTHOR. J L MARSH.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "YDBMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS YDBM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "YDBTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "YDBOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "YKBNORPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY YDBMST.
       FD  TRAN-FILE.
       COPY YDBTRN.
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
      * WS-AMOUNT-2-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-AMOUNT-2-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000554000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000335000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003000000000150000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003050000000749000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003100000000554500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013100000000409000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018100000000248500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018600000000427000'.
       01  WS-AMOUNT-2-TIER-TBL REDEFINES WS-AMOUNT-2-TIER-VALUES.
           05  WS-AMOUNT-2-TIER-ROW OCCURS 8 TIMES.
               10  WS-AMOUNT-2-TIER-LOW    PIC 9(09)V99.
               10  WS-AMOUNT-2-TIER-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-AMOUNT-1-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-AMOUNT-1-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000874000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000278500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015000000000863500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020000000000880000'.
       01  WS-AMOUNT-1-SCHED-TBL REDEFINES WS-AMOUNT-1-SCHED-VALUES.
           05  WS-AMOUNT-1-SCHED-ROW OCCURS 4 TIMES.
               10  WS-AMOUNT-1-SCHED-LOW   PIC 9(09)V99.
               10  WS-AMOUNT-1-SCHED-RATE  PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-CONTROL-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CONTROL-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000618000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000898500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000036500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000600000000796500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003100000000597500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003200000000482000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005700000000419500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015700000000627000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018200000000541500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020700000000528500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020750000000546500'.
       01  WS-CONTROL-SCHED-TBL REDEFINES WS-CONTROL-SCHED-VALUES.
           05  WS-CONTROL-SCHED-ROW OCCURS 11 TIMES.
               10  WS-CONTROL-SCHED-LOW    PIC 9(09)V99.
               10  WS-CONTROL-SCHED-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-CONTROL-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CONTROL-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000859500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000865000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002600000000776500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003100000000698000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004100000000409000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004150000000672000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004250000000519000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014250000000514000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014300000000599000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016800000000563000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016900000000756000'.
       01  WS-CONTROL-TIER-TBL REDEFINES WS-CONTROL-TIER-VALUES.
           05  WS-CONTROL-TIER-ROW OCCURS 11 TIMES.
               10  WS-CONTROL-TIER-LOW     PIC 9(09)V99.
               10  WS-CONTROL-TIER-RATE    PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  YKBNOTC  DATE WINDOW BRIDGE'.
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
       COPY YKLIMP.
       COPY YKWAIVP.
       COPY YKPRCP.

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
               MOVE 'YDBMST' TO ABND-FILE-NAME
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
           MOVE YDBT-KEY TO YDBM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-COUNT-RETRY
               PERFORM 3010-LIMIT-AMOUNT-1
               PERFORM 3020-EDIT-REJECT
               PERFORM 3030-DESCRIBE-REASON
               PERFORM 3040-EDIT-NAME
               PERFORM 3050-ACCUM-SOURCE
               PERFORM 3060-SPLIT-AMOUNT-2
               PERFORM 3070-CHECK-LAST-PAY
               PERFORM 3080-ACCUM-REGION
               PERFORM 3090-COUNT-PAYMENT
               PERFORM 3100-LIMIT-CONTROL
               PERFORM 3110-DESCRIBE-SOURCE
               PERFORM 3120-ASSESS-REVERSAL
               PERFORM 3130-ACCUM-SOURCE
               PERFORM 3140-DESCRIBE-TRAN
               PERFORM 3150-PRORATE-AMOUNT-2
               PERFORM 3160-ACCRUE-CONTROL
               PERFORM 3170-FLAG-AMOUNT-2
               PERFORM 3180-DESCRIBE-CYCLE
               PERFORM 3190-ACCRUE-CONTROL
               PERFORM 3200-AGE-LAST-PAY
               PERFORM 3210-SET-PIVOT
               PERFORM 3220-COUNT-PAYMENT
               PERFORM 3230-COUNT-PAYMENT
               PERFORM 3240-CHECK-NEXT-DUE
               PERFORM 3250-SPLIT-AMOUNT-1
               PERFORM 3260-DESCRIBE-REASON
               PERFORM 3270-ACCRUE-AMOUNT-1
               PERFORM 3280-ACCUM-REGION
               PERFORM 3290-CHECK-NOTICE
               PERFORM 3300-SET-CHANNEL
               PERFORM 3310-SPLIT-CONTROL
               PERFORM 3320-DESCRIBE-REGION
               PERFORM 3330-ACCUM-PIVOT
               PERFORM 3340-ASSESS-REVERSAL
               PERFORM 3350-SET-REASON
               PERFORM 3360-LIMIT-AMOUNT-2
               PERFORM 3370-DESCRIBE-CHANNEL
               PERFORM 3380-DESCRIBE-CHANNEL
               PERFORM 3390-EDIT-NAME
               PERFORM 3400-LIMIT-AMOUNT-2
               PERFORM 3410-ACCUM-CYCLE
               PERFORM 3420-ASSESS-NOTICE-SENT
               PERFORM 3430-ACCUM-WINDOW-RULE
               PERFORM 3440-ASSESS-AUTO-RENEW
               PERFORM 3450-AGE-OPEN
               PERFORM 3460-SPLIT-AMOUNT-1
               PERFORM 3470-AGE-EFF
               PERFORM 3480-FLAG-AMOUNT-2
               PERFORM 3490-CHECK-NEXT-DUE
               PERFORM 3500-SET-CHANNEL
               PERFORM 3510-EDIT-CONTACT
               PERFORM 3520-LIMIT-AMOUNT-1
               PERFORM 3530-SPLIT-CONTROL
               PERFORM 3540-LIMIT-AMOUNT-2
               PERFORM 3550-EDIT-REJECT
               PERFORM 3560-AGE-LAST-MAINT
               PERFORM 3570-AGE-EXPIRY
               PERFORM 3580-SPLIT-AMOUNT-2
               PERFORM 3590-CHECK-MATURITY
               PERFORM 3600-FLAG-ITEM
               PERFORM 3610-FLAG-ITEM
               PERFORM 3620-COUNT-RETRY
               PERFORM 3630-SPLIT-CONTROL
               PERFORM 3640-ACCUM-TRAN
               PERFORM 3650-PRORATE-CONTROL
               PERFORM 3660-FLAG-AMOUNT-2
               PERFORM 3670-EDIT-REJECT
               PERFORM 3680-ACCRUE-MARGIN
               PERFORM 3690-ASSESS-AUTO-RENEW
               PERFORM 3700-ASSESS-NOTICE-SENT
               PERFORM 3710-ACCRUE-CONTROL
               PERFORM 3720-SET-SOURCE
               PERFORM 3730-ACCUM-TRAN
               PERFORM 3740-SPLIT-AMOUNT-2
               PERFORM 3750-SET-PIVOT
               PERFORM 3760-ASSESS-AUTO-RENEW
               PERFORM 3770-DESCRIBE-PIVOT
               PERFORM 3780-SPLIT-CONTROL
               PERFORM 3790-LIMIT-CONTROL
               PERFORM 3800-FLAG-ITEM
               PERFORM 3810-CHECK-MATURITY
               PERFORM 3820-ASSESS-OVERRIDE
               PERFORM 3830-AGE-LAST-STMT
               PERFORM 3840-ACCUM-CHANNEL
               PERFORM 3850-PRORATE-AMOUNT-1
               PERFORM 3860-SPLIT-CONTROL
               PERFORM 3870-COUNT-RETRY
               PERFORM 3880-DESCRIBE-SOURCE
               PERFORM 3890-ACCUM-SOURCE
               PERFORM 3900-EDIT-REF-NO
               PERFORM 3910-AGE-LAST-MAINT
               PERFORM 3920-PRORATE-AMOUNT-1
               PERFORM 3930-SELECT-PENALTY-PCT
               PERFORM 3940-CHECK-EXPIRY
               PERFORM 3950-SPLIT-CONTROL
               PERFORM 3960-SET-CHANNEL
               PERFORM 3970-ACCRUE-CONTROL
               PERFORM 3980-AGE-NOTICE
               PERFORM 3990-DESCRIBE-TRAN
               PERFORM 4000-ASSESS-OVERRIDE
               PERFORM 4010-ACCRUE-AMOUNT-2
               PERFORM 4020-FLAG-AMOUNT-2
               PERFORM 4030-SET-PIVOT
               PERFORM 4040-SELECT-MARGIN
               PERFORM 4050-PRORATE-CONTROL
               PERFORM 4060-LIMIT-CONTROL
               PERFORM 4070-ACCUM-REASON
               PERFORM 4080-LIMIT-ITEM
               PERFORM 4090-CHECK-LAST-ACT
               PERFORM 4100-AGE-LAST-MAINT
               PERFORM 4110-SET-TRAN
               PERFORM 4120-AGE-NOTICE
               PERFORM 4130-SET-TRAN
               PERFORM 4140-EDIT-CUST-ID
               PERFORM 4150-ASSESS-OVERRIDE
               PERFORM 4160-ACCRUE-AMOUNT-1
               PERFORM 4170-COUNT-PAYMENT
               PERFORM 4180-CHECK-LAST-STMT
               PERFORM 4190-EDIT-REF-NO
               PERFORM 4200-EDIT-OPERATOR
               PERFORM 4210-FLAG-AMOUNT-1
               PERFORM 4220-ASSESS-OVERRIDE
               PERFORM 4230-FLAG-ITEM
               PERFORM 4240-ASSESS-OVERRIDE
               PERFORM 4250-SPLIT-CONTROL
               PERFORM 4260-EDIT-CUST-ID
               PERFORM 4270-ACCRUE-AMOUNT-1
               PERFORM 4280-DESCRIBE-REASON
               PERFORM 4290-AGE-LAST-STMT
               PERFORM 4300-AGE-NOTICE
               PERFORM 4310-SPLIT-AMOUNT-1
               PERFORM 4320-SPLIT-CONTROL
               PERFORM 4330-ASSESS-EXEMPT
               PERFORM 4340-SET-REGION
               PERFORM 4350-ASSESS-OVERRIDE
               PERFORM 4360-ASSESS-EXEMPT
               PERFORM 4370-DESCRIBE-SOURCE
               PERFORM 7040-CALL-YKLIM
               PERFORM 7030-CALL-YKWAIV
               PERFORM 7000-CALL-YKPRC
               REWRITE YDB-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'YDBMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM YDB-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3000-COUNT-RETRY.
           IF YDBM-RETRY-CNT < 9999999
               ADD 1 TO YDBM-RETRY-CNT
           END-IF
           IF YDBM-RETRY-CNT NOT < 12
               MOVE 'Y' TO YDBT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT 1 YTD IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3010-LIMIT-AMOUNT-1.
           IF YDBM-AMOUNT-1-YTD < 50.00
               MOVE 50.00 TO YDBM-AMOUNT-1-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YDBM-AMOUNT-1-YTD > 2500.00
               MOVE 2500.00 TO YDBM-AMOUNT-1-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E490 REJECT CD REQUIRED
      * E330 CONTROL AMT NEGATIVE
      * E019 CONTROL AMT OVER 250000
      * E349 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3020-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YDBT-REJECT-CD = SPACES
                   MOVE 'E490' TO WS-REJECT-CODE
               WHEN YDBT-CONTROL-AMT < 0
                   MOVE 'E330' TO WS-REJECT-CODE
               WHEN YDBT-CONTROL-AMT > 250000.00
                   MOVE 'E019' TO WS-REJECT-CODE
               WHEN YDBM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E349' TO WS-REJECT-CODE
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
       3030-DESCRIBE-REASON.
           EVALUATE YDBM-REASON-CD
               WHEN 'AX'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'LK'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'NT'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'UW'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E378 NAME REQUIRED
      * E213 CONTROL AMT NEGATIVE
      * E388 CONTROL AMT OVER 99999
      * E298 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3040-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YDBM-NAME = SPACES
                   MOVE 'E378' TO WS-REJECT-CODE
               WHEN YDBM-CONTROL-AMT < 0
                   MOVE 'E213' TO WS-REJECT-CODE
               WHEN YDBM-CONTROL-AMT > 99999.00
                   MOVE 'E388' TO WS-REJECT-CODE
               WHEN YDBT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E298' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE AMOUNT 2 AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3050-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YDBM-SOURCE-CD
                   ADD YDBM-AMOUNT-2-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YDBM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE YDBM-AMOUNT-2-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT AMOUNT 2 AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3060-SPLIT-AMOUNT-2.
           COMPUTE WS-WORK-AMT = YDBM-AMOUNT-2-AMT / 12
           COMPUTE WS-WORK-AMT-2 = YDBM-AMOUNT-2-AMT - (WS-WORK-AMT *
               12)
           COMPUTE YDBT-CONTROL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3070-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF YDBM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YDBM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO YDBM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE AMOUNT 1 YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3080-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YDBM-REGION-CD
                   ADD YDBM-AMOUNT-1-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YDBM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE YDBM-AMOUNT-1-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3090-COUNT-PAYMENT.
           IF YDBM-PAYMENT-CNT < 9999999
               ADD 1 TO YDBM-PAYMENT-CNT
           END-IF
           IF YDBM-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO YDBT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CONTROL AMT IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3100-LIMIT-CONTROL.
           IF YDBM-CONTROL-AMT < 25.00
               MOVE 25.00 TO YDBM-CONTROL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YDBM-CONTROL-AMT > 10000.00
               MOVE 10000.00 TO YDBM-CONTROL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3110-DESCRIBE-SOURCE.
           EVALUATE YDBM-SOURCE-CD
               WHEN 'BNX'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'FTB'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'GAP'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'PAP'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'VGC'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'VJV'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE CONTROL YTD FEE ON THE CONTROL AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3120-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN YDBT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO YDBM-CONTROL-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YDBM-PREFERRED-FLAG = 'Y'
                   COMPUTE YDBM-CONTROL-YTD ROUNDED = YDBM-CONTROL-AMT *
                       YDBM-DISCOUNT / 200
               WHEN YDBM-SOURCE-CD = 'BNX'
                   MOVE 15.00 TO YDBM-CONTROL-YTD
               WHEN YDBM-SOURCE-CD = 'FTB'
                   MOVE 12.00 TO YDBM-CONTROL-YTD
               WHEN OTHER
                   COMPUTE YDBM-CONTROL-YTD ROUNDED = YDBM-CONTROL-AMT *
                       YDBM-DISCOUNT / 100
           END-EVALUATE
           IF YDBM-CONTROL-YTD > 0 AND YDBM-CONTROL-YTD < 5.00
               MOVE 5.00 TO YDBM-CONTROL-YTD
           END-IF
           IF YDBM-CONTROL-YTD > 150.00
               MOVE 150.00 TO YDBM-CONTROL-YTD
           END-IF
           ADD YDBM-CONTROL-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE AMOUNT 1 AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3130-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YDBM-SOURCE-CD
                   ADD YDBM-AMOUNT-1-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YDBM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE YDBM-AMOUNT-1-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3140-DESCRIBE-TRAN.
           EVALUATE YDBT-TRAN-CD
               WHEN 'CV'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'RV'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'TS'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE AMOUNT 2 YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO AMOUNT 2 AMT.
      *----------------------------------------------------------------*
       3150-PRORATE-AMOUNT-2.
           IF WS-PERIOD-DAYS > 0
               COMPUTE YDBM-AMOUNT-2-AMT ROUNDED =
                   YDBM-AMOUNT-2-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO YDBM-AMOUNT-2-AMT
           END-IF
           ADD YDBM-AMOUNT-2-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CONTROL AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3160-ACCRUE-CONTROL.
           IF YDBM-CONTROL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   YDBM-CONTROL-AMT * YDBM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO YDBM-AMOUNT-2-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT 2 AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3170-FLAG-AMOUNT-2.
           IF YDBT-AMOUNT-2-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE YDBT-AMOUNT-2-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3180-DESCRIBE-CYCLE.
           EVALUATE YDBM-CYCLE-CD
               WHEN 'FJ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'LA'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'SP'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'UC'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CONTROL YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3190-ACCRUE-CONTROL.
           IF YDBM-CONTROL-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   YDBM-CONTROL-YTD * YDBM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3200-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF YDBM-LAST-PAY-DATE > 0 AND YDBM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YDBM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PIVOT CD:
      * KGT -> NNG WHEN LAST PAY DATE IS PAST
      * NNG -> PTC WHEN LAST PAY DATE IS PAST
      * PTC -> WZW WHEN LAST PAY DATE IS PAST
      * WZW -> KGT WHEN LAST PAY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3210-SET-PIVOT.
           EVALUATE YDBM-PIVOT-CD
               WHEN 'KGT'
                   IF YDBM-LAST-PAY-DATE > 0 AND YDBM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NNG' TO YDBM-PIVOT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NNG'
                   IF YDBM-LAST-PAY-DATE > 0 AND YDBM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PTC' TO YDBM-PIVOT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PTC'
                   IF YDBM-LAST-PAY-DATE > 0 AND YDBM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WZW' TO YDBM-PIVOT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WZW'
                   IF YDBM-LAST-PAY-DATE > 0 AND YDBM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KGT' TO YDBM-PIVOT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3220-COUNT-PAYMENT.
           IF YDBM-PAYMENT-CNT < 9999999
               ADD 1 TO YDBM-PAYMENT-CNT
           END-IF
           IF YDBM-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO YDBT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3230-COUNT-PAYMENT.
           IF YDBM-PAYMENT-CNT < 9999999
               ADD 1 TO YDBM-PAYMENT-CNT
           END-IF
           IF YDBM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO YDBM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3240-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF YDBM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YDBM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO YDBM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT AMOUNT 1 AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3250-SPLIT-AMOUNT-1.
           COMPUTE WS-WORK-AMT = YDBM-AMOUNT-1-AMT / 3
           COMPUTE WS-WORK-AMT-2 = YDBM-AMOUNT-1-AMT - (WS-WORK-AMT * 3)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3260-DESCRIBE-REASON.
           EVALUATE YDBM-REASON-CD
               WHEN 'AX'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'LK'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'NT'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'UW'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON AMOUNT 1 AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3270-ACCRUE-AMOUNT-1.
           IF YDBM-AMOUNT-1-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   YDBM-AMOUNT-1-AMT * YDBM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO YDBT-AMOUNT-1-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CONTROL AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3280-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YDBM-REGION-CD
                   ADD YDBM-CONTROL-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YDBM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE YDBM-CONTROL-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3290-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF YDBM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YDBM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO YDBM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN NEXT DUE DATE IS PAST
      * BAT -> MAL WHEN NEXT DUE DATE IS PAST
      * MAL -> TEL WHEN NEXT DUE DATE IS PAST
      * TEL -> ONL WHEN NEXT DUE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3300-SET-CHANNEL.
           EVALUATE YDBT-CHANNEL-CD
               WHEN 'BRN'
                   IF YDBM-NEXT-DUE-DATE > 0 AND YDBM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO YDBT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF YDBM-NEXT-DUE-DATE > 0 AND YDBM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO YDBT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF YDBM-NEXT-DUE-DATE > 0 AND YDBM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO YDBT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF YDBM-NEXT-DUE-DATE > 0 AND YDBM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO YDBT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT CONTROL YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3310-SPLIT-CONTROL.
           COMPUTE WS-WORK-AMT = YDBM-CONTROL-YTD / 3
           COMPUTE WS-WORK-AMT-2 = YDBM-CONTROL-YTD - (WS-WORK-AMT * 3)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3320-DESCRIBE-REGION.
           EVALUATE YDBM-REGION-CD
               WHEN 'C'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'H'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'K'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'S'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'V'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'W'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'X'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE AMOUNT 2 AMT BY PIVOT CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3330-ACCUM-PIVOT.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YDBM-PIVOT-CD
                   ADD YDBM-AMOUNT-2-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YDBM-PIVOT-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE YDBM-AMOUNT-2-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CONTROL FEE ON THE CONTROL YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3340-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN YDBT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO YDBM-CONTROL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YDBM-OVERRIDE-FLAG = 'Y'
                   COMPUTE YDBM-CONTROL-AMT ROUNDED = YDBM-CONTROL-YTD *
                       YDBM-MARGIN / 200
               WHEN YDBM-CYCLE-CD = 'FJ'
                   MOVE 20.00 TO YDBM-CONTROL-AMT
               WHEN YDBM-CYCLE-CD = 'LA'
                   MOVE 5.00 TO YDBM-CONTROL-AMT
               WHEN OTHER
                   COMPUTE YDBM-CONTROL-AMT ROUNDED = YDBM-CONTROL-YTD *
                       YDBM-MARGIN / 100
           END-EVALUATE
           IF YDBM-CONTROL-AMT > 0 AND YDBM-CONTROL-AMT < 10.00
               MOVE 10.00 TO YDBM-CONTROL-AMT
           END-IF
           IF YDBM-CONTROL-AMT > 35.00
               MOVE 35.00 TO YDBM-CONTROL-AMT
           END-IF
           ADD YDBM-CONTROL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * AX -> LK WHEN EXPIRY DATE IS PAST
      * LK -> NT WHEN EXPIRY DATE IS PAST
      * NT -> UW WHEN EXPIRY DATE IS PAST
      * UW -> AX WHEN EXPIRY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3350-SET-REASON.
           EVALUATE YDBM-REASON-CD
               WHEN 'AX'
                   IF YDBM-EXPIRY-DATE > 0 AND YDBM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LK' TO YDBM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LK'
                   IF YDBM-EXPIRY-DATE > 0 AND YDBM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NT' TO YDBM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NT'
                   IF YDBM-EXPIRY-DATE > 0 AND YDBM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UW' TO YDBM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UW'
                   IF YDBM-EXPIRY-DATE > 0 AND YDBM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AX' TO YDBM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AMOUNT 2 AMT IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3360-LIMIT-AMOUNT-2.
           IF YDBT-AMOUNT-2-AMT < 25.00
               MOVE 25.00 TO YDBT-AMOUNT-2-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YDBT-AMOUNT-2-AMT > 50000.00
               MOVE 50000.00 TO YDBT-AMOUNT-2-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3370-DESCRIBE-CHANNEL.
           EVALUATE YDBT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3380-DESCRIBE-CHANNEL.
           EVALUATE YDBT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E860 NAME REQUIRED
      * E801 CONTROL AMT NEGATIVE
      * E175 CONTROL AMT OVER 250000
      * E395 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3390-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YDBM-NAME = SPACES
                   MOVE 'E860' TO WS-REJECT-CODE
               WHEN YDBT-CONTROL-AMT < 0
                   MOVE 'E801' TO WS-REJECT-CODE
               WHEN YDBT-CONTROL-AMT > 250000.00
                   MOVE 'E175' TO WS-REJECT-CODE
               WHEN YDBM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E395' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT 2 YTD IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3400-LIMIT-AMOUNT-2.
           IF YDBM-AMOUNT-2-YTD < 50.00
               MOVE 50.00 TO YDBM-AMOUNT-2-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YDBM-AMOUNT-2-YTD > 500.00
               MOVE 500.00 TO YDBM-AMOUNT-2-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE AMOUNT 2 AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3410-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YDBM-CYCLE-CD
                   ADD YDBM-AMOUNT-2-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YDBM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE YDBM-AMOUNT-2-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE AMOUNT 1 YTD FEE ON THE AMOUNT 1 AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3420-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN YDBM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO YDBM-AMOUNT-1-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YDBM-REVIEW-FLAG = 'Y'
                   COMPUTE YDBM-AMOUNT-1-YTD ROUNDED = YDBT-AMOUNT-1-AMT
                       * YDBM-PENALTY-PCT / 200
               WHEN YDBM-REASON-CD = 'AX'
                   MOVE 10.00 TO YDBM-AMOUNT-1-YTD
               WHEN YDBM-REASON-CD = 'LK'
                   MOVE 3.00 TO YDBM-AMOUNT-1-YTD
               WHEN OTHER
                   COMPUTE YDBM-AMOUNT-1-YTD ROUNDED = YDBT-AMOUNT-1-AMT
                       * YDBM-PENALTY-PCT / 100
           END-EVALUATE
           IF YDBM-AMOUNT-1-YTD > 0 AND YDBM-AMOUNT-1-YTD < 1.00
               MOVE 1.00 TO YDBM-AMOUNT-1-YTD
           END-IF
           IF YDBM-AMOUNT-1-YTD > 150.00
               MOVE 150.00 TO YDBM-AMOUNT-1-YTD
           END-IF
           ADD YDBM-AMOUNT-1-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE AMOUNT 1 AMT BY WINDOW RULE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3430-ACCUM-WINDOW-RULE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YDBM-WINDOW-RULE-CD
                   ADD YDBT-AMOUNT-1-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YDBM-WINDOW-RULE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE YDBT-AMOUNT-1-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE AMOUNT 1 YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3440-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN YDBM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YDBM-REVIEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = YDBM-AMOUNT-1-YTD *
                       YDBM-MARGIN / 200
               WHEN YDBM-PIVOT-CD = 'KGT'
                   MOVE 7.00 TO WS-WORK-AMT-2
               WHEN YDBM-PIVOT-CD = 'NNG'
                   MOVE 20.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = YDBM-AMOUNT-1-YTD *
                       YDBM-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 2.00
               MOVE 2.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3450-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF YDBM-OPEN-DATE > 0 AND YDBM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YDBM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD YDBM-CONTROL-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD YDBM-CONTROL-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD YDBM-CONTROL-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD YDBM-CONTROL-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD YDBM-CONTROL-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT AMOUNT 1 AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3460-SPLIT-AMOUNT-1.
           COMPUTE WS-WORK-AMT = YDBT-AMOUNT-1-AMT / 12
           COMPUTE WS-WORK-AMT-2 = YDBT-AMOUNT-1-AMT - (WS-WORK-AMT *
               12)
           COMPUTE YDBM-CONTROL-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3470-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF YDBT-EFF-DATE > 0 AND YDBT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YDBT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AMOUNT 2 AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3480-FLAG-AMOUNT-2.
           IF YDBT-AMOUNT-2-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE YDBT-AMOUNT-2-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3490-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF YDBM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YDBM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO YDBM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN MATURITY DATE IS PAST
      * BAT -> MAL WHEN MATURITY DATE IS PAST
      * MAL -> TEL WHEN MATURITY DATE IS PAST
      * TEL -> ONL WHEN MATURITY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3500-SET-CHANNEL.
           EVALUATE YDBT-CHANNEL-CD
               WHEN 'BRN'
                   IF YDBM-MATURITY-DATE > 0 AND YDBM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO YDBT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF YDBM-MATURITY-DATE > 0 AND YDBM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO YDBT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF YDBM-MATURITY-DATE > 0 AND YDBM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO YDBT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF YDBM-MATURITY-DATE > 0 AND YDBM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO YDBT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E267 CONTACT REQUIRED
      * E672 AMOUNT 2 AMT NEGATIVE
      * E665 AMOUNT 2 AMT OVER 25000
      * E161 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3510-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YDBM-CONTACT = SPACES
                   MOVE 'E267' TO WS-REJECT-CODE
               WHEN YDBT-AMOUNT-2-AMT < 0
                   MOVE 'E672' TO WS-REJECT-CODE
               WHEN YDBT-AMOUNT-2-AMT > 25000.00
                   MOVE 'E665' TO WS-REJECT-CODE
               WHEN YDBM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E161' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT 1 AMT IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3520-LIMIT-AMOUNT-1.
           IF YDBM-AMOUNT-1-AMT < 100.00
               MOVE 100.00 TO YDBM-AMOUNT-1-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YDBM-AMOUNT-1-AMT > 500.00
               MOVE 500.00 TO YDBM-AMOUNT-1-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CONTROL AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3530-SPLIT-CONTROL.
           COMPUTE WS-WORK-AMT = YDBT-CONTROL-AMT / 3
           COMPUTE WS-WORK-AMT-2 = YDBT-CONTROL-AMT - (WS-WORK-AMT * 3)
           COMPUTE YDBM-CONTROL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT 2 YTD IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3540-LIMIT-AMOUNT-2.
           IF YDBM-AMOUNT-2-YTD < 100.00
               MOVE 100.00 TO YDBM-AMOUNT-2-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YDBM-AMOUNT-2-YTD > 2500.00
               MOVE 2500.00 TO YDBM-AMOUNT-2-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E899 REJECT CD REQUIRED
      * E773 AMOUNT 2 YTD NEGATIVE
      * E263 AMOUNT 2 YTD OVER 250000
      * E800 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3550-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YDBT-REJECT-CD = SPACES
                   MOVE 'E899' TO WS-REJECT-CODE
               WHEN YDBM-AMOUNT-2-YTD < 0
                   MOVE 'E773' TO WS-REJECT-CODE
               WHEN YDBM-AMOUNT-2-YTD > 250000.00
                   MOVE 'E263' TO WS-REJECT-CODE
               WHEN YDBM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E800' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3560-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF YDBM-LAST-MAINT-DATE > 0 AND YDBM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YDBM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD YDBT-AMOUNT-1-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD YDBT-AMOUNT-1-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD YDBT-AMOUNT-1-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD YDBT-AMOUNT-1-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD YDBT-AMOUNT-1-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3570-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF YDBM-EXPIRY-DATE > 0 AND YDBM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YDBM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD YDBM-AMOUNT-2-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD YDBM-AMOUNT-2-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD YDBM-AMOUNT-2-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD YDBM-AMOUNT-2-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD YDBM-AMOUNT-2-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT AMOUNT 2 YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3580-SPLIT-AMOUNT-2.
           COMPUTE WS-WORK-AMT = YDBM-AMOUNT-2-YTD / 4
           COMPUTE WS-WORK-AMT-2 = YDBM-AMOUNT-2-YTD - (WS-WORK-AMT * 4)
           COMPUTE YDBM-AMOUNT-2-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 45 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3590-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF YDBM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YDBM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO YDBT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3600-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3610-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3620-COUNT-RETRY.
           IF YDBM-RETRY-CNT < 9999999
               ADD 1 TO YDBM-RETRY-CNT
           END-IF
           IF YDBM-RETRY-CNT NOT < 6
               MOVE 'Y' TO YDBM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CONTROL YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3630-SPLIT-CONTROL.
           COMPUTE WS-WORK-AMT = YDBM-CONTROL-YTD / 2
           COMPUTE WS-WORK-AMT-2 = YDBM-CONTROL-YTD - (WS-WORK-AMT * 2)
           COMPUTE YDBM-AMOUNT-1-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE AMOUNT 1 AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3640-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YDBT-TRAN-CD
                   ADD YDBT-AMOUNT-1-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YDBT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE YDBT-AMOUNT-1-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE AMOUNT 2 AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CONTROL AMT.
      *----------------------------------------------------------------*
       3650-PRORATE-CONTROL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE YDBT-CONTROL-AMT ROUNDED =
                   YDBT-AMOUNT-2-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO YDBT-CONTROL-AMT
           END-IF
           ADD YDBT-CONTROL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AMOUNT 2 AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3660-FLAG-AMOUNT-2.
           IF YDBT-AMOUNT-2-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE YDBT-AMOUNT-2-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E916 REJECT CD REQUIRED
      * E589 CONTROL YTD NEGATIVE
      * E351 CONTROL YTD OVER 50000
      * E714 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3670-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YDBT-REJECT-CD = SPACES
                   MOVE 'E916' TO WS-REJECT-CODE
               WHEN YDBM-CONTROL-YTD < 0
                   MOVE 'E589' TO WS-REJECT-CODE
               WHEN YDBM-CONTROL-YTD > 50000.00
                   MOVE 'E351' TO WS-REJECT-CODE
               WHEN YDBM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E714' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3680-ACCRUE-MARGIN.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * YDBM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO YDBM-AMOUNT-2-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE AMOUNT 1 YTD FEE ON THE AMOUNT 1 AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3690-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN YDBM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO YDBM-AMOUNT-1-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YDBM-REVIEW-FLAG = 'Y'
                   COMPUTE YDBM-AMOUNT-1-YTD ROUNDED = YDBM-AMOUNT-1-AMT
                       * YDBM-DISCOUNT / 200
               WHEN YDBM-SOURCE-CD = 'BNX'
                   MOVE 20.00 TO YDBM-AMOUNT-1-YTD
               WHEN YDBM-SOURCE-CD = 'FTB'
                   MOVE 7.00 TO YDBM-AMOUNT-1-YTD
               WHEN OTHER
                   COMPUTE YDBM-AMOUNT-1-YTD ROUNDED = YDBM-AMOUNT-1-AMT
                       * YDBM-DISCOUNT / 100
           END-EVALUATE
           IF YDBM-AMOUNT-1-YTD > 0 AND YDBM-AMOUNT-1-YTD < 1.00
               MOVE 1.00 TO YDBM-AMOUNT-1-YTD
           END-IF
           IF YDBM-AMOUNT-1-YTD > 50.00
               MOVE 50.00 TO YDBM-AMOUNT-1-YTD
           END-IF
           ADD YDBM-AMOUNT-1-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE AMOUNT 2 FEE ON THE WORK AMT 2.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3700-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN YDBM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO YDBT-AMOUNT-2-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YDBM-EXEMPT-FLAG = 'Y'
                   COMPUTE YDBT-AMOUNT-2-AMT ROUNDED = WS-WORK-AMT-2 *
                       YDBM-DISCOUNT / 200
               WHEN YDBM-PIVOT-CD = 'KGT'
                   MOVE 3.00 TO YDBT-AMOUNT-2-AMT
               WHEN YDBM-PIVOT-CD = 'NNG'
                   MOVE 5.00 TO YDBT-AMOUNT-2-AMT
               WHEN OTHER
                   COMPUTE YDBT-AMOUNT-2-AMT ROUNDED = WS-WORK-AMT-2 *
                       YDBM-DISCOUNT / 100
           END-EVALUATE
           IF YDBT-AMOUNT-2-AMT > 0 AND YDBT-AMOUNT-2-AMT < 5.00
               MOVE 5.00 TO YDBT-AMOUNT-2-AMT
           END-IF
           IF YDBT-AMOUNT-2-AMT > 250.00
               MOVE 250.00 TO YDBT-AMOUNT-2-AMT
           END-IF
           ADD YDBT-AMOUNT-2-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CONTROL AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3710-ACCRUE-CONTROL.
           IF YDBM-CONTROL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   YDBM-CONTROL-AMT * YDBM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO YDBM-CONTROL-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * BNX -> FTB WHEN OPEN DATE IS PAST
      * FTB -> GAP WHEN OPEN DATE IS PAST
      * GAP -> PAP WHEN OPEN DATE IS PAST
      * PAP -> VGC WHEN OPEN DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3720-SET-SOURCE.
           EVALUATE YDBM-SOURCE-CD
               WHEN 'BNX'
                   IF YDBM-OPEN-DATE > 0 AND YDBM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FTB' TO YDBM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FTB'
                   IF YDBM-OPEN-DATE > 0 AND YDBM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GAP' TO YDBM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GAP'
                   IF YDBM-OPEN-DATE > 0 AND YDBM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PAP' TO YDBM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PAP'
                   IF YDBM-OPEN-DATE > 0 AND YDBM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VGC' TO YDBM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE AMOUNT 1 AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3730-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YDBT-TRAN-CD
                   ADD YDBT-AMOUNT-1-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YDBT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE YDBT-AMOUNT-1-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT AMOUNT 2 YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3740-SPLIT-AMOUNT-2.
           COMPUTE WS-WORK-AMT = YDBM-AMOUNT-2-YTD / 12
           COMPUTE WS-WORK-AMT-2 = YDBM-AMOUNT-2-YTD - (WS-WORK-AMT *
               12)
           COMPUTE YDBT-AMOUNT-1-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PIVOT CD:
      * KGT -> NNG WHEN LAST STMT DATE IS PAST
      * NNG -> PTC WHEN LAST STMT DATE IS PAST
      * PTC -> WZW WHEN LAST STMT DATE IS PAST
      * WZW -> KGT WHEN LAST STMT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3750-SET-PIVOT.
           EVALUATE YDBM-PIVOT-CD
               WHEN 'KGT'
                   IF YDBM-LAST-STMT-DATE > 0 AND YDBM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NNG' TO YDBM-PIVOT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NNG'
                   IF YDBM-LAST-STMT-DATE > 0 AND YDBM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PTC' TO YDBM-PIVOT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PTC'
                   IF YDBM-LAST-STMT-DATE > 0 AND YDBM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WZW' TO YDBM-PIVOT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WZW'
                   IF YDBM-LAST-STMT-DATE > 0 AND YDBM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KGT' TO YDBM-PIVOT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE AMOUNT 2 AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3760-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN YDBM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YDBM-OVERRIDE-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = YDBT-AMOUNT-2-AMT *
                       YDBM-MARGIN / 200
               WHEN YDBM-PIVOT-CD = 'KGT'
                   MOVE 7.00 TO WS-WORK-AMT
               WHEN YDBM-PIVOT-CD = 'NNG'
                   MOVE 15.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = YDBT-AMOUNT-2-AMT *
                       YDBM-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 1.00
               MOVE 1.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 250.00
               MOVE 250.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE PIVOT CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3770-DESCRIBE-PIVOT.
           EVALUATE YDBM-PIVOT-CD
               WHEN 'KGT'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'NNG'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'PTC'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'WZW'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3780-SPLIT-CONTROL.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 12)
           COMPUTE YDBT-CONTROL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CONTROL AMT IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3790-LIMIT-CONTROL.
           IF YDBT-CONTROL-AMT < 0.00
               MOVE 0.00 TO YDBT-CONTROL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YDBT-CONTROL-AMT > 500.00
               MOVE 500.00 TO YDBT-CONTROL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3800-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 10 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3810-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF YDBM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YDBM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO YDBM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE AMOUNT 1 YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3820-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN YDBM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YDBM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = YDBM-AMOUNT-1-YTD *
                       YDBM-MARGIN / 200
               WHEN YDBT-TRAN-CD = 'CV'
                   MOVE 7.00 TO WS-WORK-AMT
               WHEN YDBT-TRAN-CD = 'RV'
                   MOVE 20.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = YDBM-AMOUNT-1-YTD *
                       YDBM-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 2.00
               MOVE 2.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 75.00
               MOVE 75.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3830-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF YDBM-LAST-STMT-DATE > 0 AND YDBM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YDBM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD YDBM-AMOUNT-1-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD YDBM-AMOUNT-1-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD YDBM-AMOUNT-1-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD YDBM-AMOUNT-1-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD YDBM-AMOUNT-1-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE AMOUNT 2 YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3840-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YDBT-CHANNEL-CD
                   ADD YDBM-AMOUNT-2-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YDBT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE YDBM-AMOUNT-2-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE AMOUNT 1 YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO AMOUNT 1 AMT.
      *----------------------------------------------------------------*
       3850-PRORATE-AMOUNT-1.
           IF WS-PERIOD-DAYS > 0
               COMPUTE YDBT-AMOUNT-1-AMT ROUNDED =
                   YDBM-AMOUNT-1-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO YDBT-AMOUNT-1-AMT
           END-IF
           ADD YDBT-AMOUNT-1-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT CONTROL YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3860-SPLIT-CONTROL.
           COMPUTE WS-WORK-AMT = YDBM-CONTROL-YTD / 12
           COMPUTE WS-WORK-AMT-2 = YDBM-CONTROL-YTD - (WS-WORK-AMT * 12)
           COMPUTE YDBM-AMOUNT-2-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3870-COUNT-RETRY.
           IF YDBM-RETRY-CNT < 9999999
               ADD 1 TO YDBM-RETRY-CNT
           END-IF
           IF YDBM-RETRY-CNT NOT < 5
               MOVE 'Y' TO YDBM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3880-DESCRIBE-SOURCE.
           EVALUATE YDBM-SOURCE-CD
               WHEN 'BNX'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'FTB'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'GAP'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'PAP'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'VGC'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'VJV'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE AMOUNT 1 YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3890-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YDBM-SOURCE-CD
                   ADD YDBM-AMOUNT-1-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YDBM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE YDBM-AMOUNT-1-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E540 REF NO REQUIRED
      * E691 CONTROL AMT NEGATIVE
      * E424 CONTROL AMT OVER 1000000
      * E175 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3900-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YDBM-REF-NO = SPACES
                   MOVE 'E540' TO WS-REJECT-CODE
               WHEN YDBT-CONTROL-AMT < 0
                   MOVE 'E691' TO WS-REJECT-CODE
               WHEN YDBT-CONTROL-AMT > 1000000.00
                   MOVE 'E424' TO WS-REJECT-CODE
               WHEN YDBT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E175' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-60, 61-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3910-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF YDBM-LAST-MAINT-DATE > 0 AND YDBM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YDBM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD YDBM-AMOUNT-2-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD YDBM-AMOUNT-2-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD YDBM-AMOUNT-2-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD YDBM-AMOUNT-2-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD YDBM-AMOUNT-2-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO AMOUNT 1 YTD.
      *----------------------------------------------------------------*
       3920-PRORATE-AMOUNT-1.
           IF WS-PERIOD-DAYS > 0
               COMPUTE YDBM-AMOUNT-1-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO YDBM-AMOUNT-1-YTD
           END-IF
           ADD YDBM-AMOUNT-1-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE CONTROL TIER TIER FOR THE CONTROL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3930-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CONTROL-TIER-RATE(1) TO YDBM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF YDBT-CONTROL-AMT NOT < WS-CONTROL-TIER-LOW(WS-TX)
                   MOVE WS-CONTROL-TIER-RATE(WS-TX) TO YDBM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF YDBM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO YDBM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 15 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3940-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF YDBM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YDBM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO YDBM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CONTROL YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3950-SPLIT-CONTROL.
           COMPUTE WS-WORK-AMT = YDBM-CONTROL-YTD / 3
           COMPUTE WS-WORK-AMT-2 = YDBM-CONTROL-YTD - (WS-WORK-AMT * 3)
           COMPUTE YDBM-AMOUNT-2-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN LAST MAINT DATE IS PAST
      * BAT -> MAL WHEN LAST MAINT DATE IS PAST
      * MAL -> TEL WHEN LAST MAINT DATE IS PAST
      * TEL -> ONL WHEN LAST MAINT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3960-SET-CHANNEL.
           EVALUATE YDBT-CHANNEL-CD
               WHEN 'BRN'
                   IF YDBM-LAST-MAINT-DATE > 0 AND YDBM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'BAT' TO YDBT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF YDBM-LAST-MAINT-DATE > 0 AND YDBM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'MAL' TO YDBT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF YDBM-LAST-MAINT-DATE > 0 AND YDBM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'TEL' TO YDBT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF YDBM-LAST-MAINT-DATE > 0 AND YDBM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'ONL' TO YDBT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CONTROL AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3970-ACCRUE-CONTROL.
           IF YDBT-CONTROL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   YDBT-CONTROL-AMT * YDBM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO YDBM-CONTROL-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3980-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF YDBM-NOTICE-DATE > 0 AND YDBM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YDBM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3990-DESCRIBE-TRAN.
           EVALUATE YDBT-TRAN-CD
               WHEN 'CV'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'RV'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'TS'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE AMOUNT 1 YTD FEE ON THE AMOUNT 2 AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4000-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN YDBM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO YDBM-AMOUNT-1-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YDBM-EXEMPT-FLAG = 'Y'
                   COMPUTE YDBM-AMOUNT-1-YTD ROUNDED = YDBM-AMOUNT-2-AMT
                       * YDBM-MARGIN / 200
               WHEN YDBM-PIVOT-CD = 'KGT'
                   MOVE 3.00 TO YDBM-AMOUNT-1-YTD
               WHEN YDBM-PIVOT-CD = 'NNG'
                   MOVE 3.00 TO YDBM-AMOUNT-1-YTD
               WHEN OTHER
                   COMPUTE YDBM-AMOUNT-1-YTD ROUNDED = YDBM-AMOUNT-2-AMT
                       * YDBM-MARGIN / 100
           END-EVALUATE
           IF YDBM-AMOUNT-1-YTD > 0 AND YDBM-AMOUNT-1-YTD < 1.00
               MOVE 1.00 TO YDBM-AMOUNT-1-YTD
           END-IF
           IF YDBM-AMOUNT-1-YTD > 75.00
               MOVE 75.00 TO YDBM-AMOUNT-1-YTD
           END-IF
           ADD YDBM-AMOUNT-1-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON AMOUNT 2 AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4010-ACCRUE-AMOUNT-2.
           IF YDBT-AMOUNT-2-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   YDBT-AMOUNT-2-AMT * YDBM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO YDBT-AMOUNT-1-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT 2 YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4020-FLAG-AMOUNT-2.
           IF YDBM-AMOUNT-2-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE YDBM-AMOUNT-2-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PIVOT CD:
      * KGT -> NNG WHEN POST DATE IS PAST
      * NNG -> PTC WHEN POST DATE IS PAST
      * PTC -> WZW WHEN POST DATE IS PAST
      * WZW -> KGT WHEN POST DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4030-SET-PIVOT.
           EVALUATE YDBM-PIVOT-CD
               WHEN 'KGT'
                   IF YDBT-POST-DATE > 0 AND YDBT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NNG' TO YDBM-PIVOT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NNG'
                   IF YDBT-POST-DATE > 0 AND YDBT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PTC' TO YDBM-PIVOT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PTC'
                   IF YDBT-POST-DATE > 0 AND YDBT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WZW' TO YDBM-PIVOT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WZW'
                   IF YDBT-POST-DATE > 0 AND YDBT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KGT' TO YDBM-PIVOT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE CONTROL SCHED TIER FOR THE AMOUNT 1 AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4040-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-CONTROL-SCHED-RATE(1) TO YDBM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF YDBM-AMOUNT-1-AMT NOT < WS-CONTROL-SCHED-LOW(WS-TX)
                   MOVE WS-CONTROL-SCHED-RATE(WS-TX) TO YDBM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF YDBM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO YDBM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CONTROL YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CONTROL AMT.
      *----------------------------------------------------------------*
       4050-PRORATE-CONTROL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE YDBT-CONTROL-AMT ROUNDED =
                   YDBM-CONTROL-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO YDBT-CONTROL-AMT
           END-IF
           ADD YDBT-CONTROL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * CONTROL YTD IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4060-LIMIT-CONTROL.
           IF YDBM-CONTROL-YTD < 10.00
               MOVE 10.00 TO YDBM-CONTROL-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YDBM-CONTROL-YTD > 1000.00
               MOVE 1000.00 TO YDBM-CONTROL-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE AMOUNT 2 AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4070-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YDBM-REASON-CD
                   ADD YDBT-AMOUNT-2-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YDBM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE YDBT-AMOUNT-2-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4080-LIMIT-ITEM.
           IF WS-WORK-AMT < 100.00
               MOVE 100.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 10000.00
               MOVE 10000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 90 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4090-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF YDBM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YDBM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO YDBT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4100-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF YDBM-LAST-MAINT-DATE > 0 AND YDBM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YDBM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD YDBT-AMOUNT-1-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD YDBT-AMOUNT-1-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD YDBT-AMOUNT-1-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD YDBT-AMOUNT-1-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD YDBT-AMOUNT-1-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * CV -> RV WHEN POST DATE IS PAST
      * RV -> TS WHEN POST DATE IS PAST
      * TS -> CV WHEN POST DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4110-SET-TRAN.
           EVALUATE YDBT-TRAN-CD
               WHEN 'CV'
                   IF YDBT-POST-DATE > 0 AND YDBT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RV' TO YDBT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RV'
                   IF YDBT-POST-DATE > 0 AND YDBT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TS' TO YDBT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TS'
                   IF YDBT-POST-DATE > 0 AND YDBT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CV' TO YDBT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4120-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF YDBM-NOTICE-DATE > 0 AND YDBM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YDBM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD YDBM-CONTROL-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD YDBM-CONTROL-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD YDBM-CONTROL-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD YDBM-CONTROL-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD YDBM-CONTROL-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * CV -> RV WHEN LAST PAY DATE IS PAST
      * RV -> TS WHEN LAST PAY DATE IS PAST
      * TS -> CV WHEN LAST PAY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4130-SET-TRAN.
           EVALUATE YDBT-TRAN-CD
               WHEN 'CV'
                   IF YDBM-LAST-PAY-DATE > 0 AND YDBM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RV' TO YDBT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RV'
                   IF YDBM-LAST-PAY-DATE > 0 AND YDBM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TS' TO YDBT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TS'
                   IF YDBM-LAST-PAY-DATE > 0 AND YDBM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CV' TO YDBT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E148 CUST ID REQUIRED
      * E390 WORK AMT 2 NEGATIVE
      * E275 WORK AMT 2 OVER 10000
      * E488 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       4140-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YDBM-CUST-ID = SPACES
                   MOVE 'E148' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E390' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 10000.00
                   MOVE 'E275' TO WS-REJECT-CODE
               WHEN YDBM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E488' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CONTROL FEE ON THE AMOUNT 1 AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4150-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN YDBM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO YDBT-CONTROL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YDBT-REVERSAL-FLAG = 'Y'
                   COMPUTE YDBT-CONTROL-AMT ROUNDED = YDBT-AMOUNT-1-AMT
                       * YDBM-MARGIN / 200
               WHEN YDBM-SOURCE-CD = 'BNX'
                   MOVE 15.00 TO YDBT-CONTROL-AMT
               WHEN YDBM-SOURCE-CD = 'FTB'
                   MOVE 10.00 TO YDBT-CONTROL-AMT
               WHEN OTHER
                   COMPUTE YDBT-CONTROL-AMT ROUNDED = YDBT-AMOUNT-1-AMT
                       * YDBM-MARGIN / 100
           END-EVALUATE
           IF YDBT-CONTROL-AMT > 0 AND YDBT-CONTROL-AMT < 1.00
               MOVE 1.00 TO YDBT-CONTROL-AMT
           END-IF
           IF YDBT-CONTROL-AMT > 25.00
               MOVE 25.00 TO YDBT-CONTROL-AMT
           END-IF
           ADD YDBT-CONTROL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON AMOUNT 1 AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4160-ACCRUE-AMOUNT-1.
           IF YDBT-AMOUNT-1-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   YDBT-AMOUNT-1-AMT * YDBM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO YDBT-AMOUNT-2-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       4170-COUNT-PAYMENT.
           IF YDBM-PAYMENT-CNT < 9999999
               ADD 1 TO YDBM-PAYMENT-CNT
           END-IF
           IF YDBM-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO YDBM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 30 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4180-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF YDBM-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YDBM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO YDBM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E534 REF NO REQUIRED
      * E752 WORK AMT 2 NEGATIVE
      * E190 WORK AMT 2 OVER 5000
      * E770 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       4190-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YDBM-REF-NO = SPACES
                   MOVE 'E534' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E752' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 5000.00
                   MOVE 'E190' TO WS-REJECT-CODE
               WHEN YDBM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E770' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E779 OPERATOR REQUIRED
      * E463 AMOUNT 1 AMT NEGATIVE
      * E209 AMOUNT 1 AMT OVER 99999
      * E970 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       4200-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YDBT-OPERATOR = SPACES
                   MOVE 'E779' TO WS-REJECT-CODE
               WHEN YDBM-AMOUNT-1-AMT < 0
                   MOVE 'E463' TO WS-REJECT-CODE
               WHEN YDBM-AMOUNT-1-AMT > 99999.00
                   MOVE 'E209' TO WS-REJECT-CODE
               WHEN YDBT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E970' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT 1 AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4210-FLAG-AMOUNT-1.
           IF YDBM-AMOUNT-1-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE YDBM-AMOUNT-1-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CONTROL FEE ON THE AMOUNT 2 AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4220-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN YDBM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO YDBM-CONTROL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YDBM-PREFERRED-FLAG = 'Y'
                   COMPUTE YDBM-CONTROL-AMT ROUNDED = YDBM-AMOUNT-2-AMT
                       * YDBM-PENALTY-PCT / 200
               WHEN YDBM-SOURCE-CD = 'BNX'
                   MOVE 7.00 TO YDBM-CONTROL-AMT
               WHEN YDBM-SOURCE-CD = 'FTB'
                   MOVE 20.00 TO YDBM-CONTROL-AMT
               WHEN OTHER
                   COMPUTE YDBM-CONTROL-AMT ROUNDED = YDBM-AMOUNT-2-AMT
                       * YDBM-PENALTY-PCT / 100
           END-EVALUATE
           IF YDBM-CONTROL-AMT > 0 AND YDBM-CONTROL-AMT < 2.00
               MOVE 2.00 TO YDBM-CONTROL-AMT
           END-IF
           IF YDBM-CONTROL-AMT > 100.00
               MOVE 100.00 TO YDBM-CONTROL-AMT
           END-IF
           ADD YDBM-CONTROL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4230-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE AMOUNT 1 YTD FEE ON THE AMOUNT 2 AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       4240-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN YDBM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO YDBM-AMOUNT-1-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YDBM-PREFERRED-FLAG = 'Y'
                   COMPUTE YDBM-AMOUNT-1-YTD ROUNDED = YDBT-AMOUNT-2-AMT
                       * YDBM-DISCOUNT / 200
               WHEN YDBM-REASON-CD = 'AX'
                   MOVE 15.00 TO YDBM-AMOUNT-1-YTD
               WHEN YDBM-REASON-CD = 'LK'
                   MOVE 12.00 TO YDBM-AMOUNT-1-YTD
               WHEN OTHER
                   COMPUTE YDBM-AMOUNT-1-YTD ROUNDED = YDBT-AMOUNT-2-AMT
                       * YDBM-DISCOUNT / 100
           END-EVALUATE
           IF YDBM-AMOUNT-1-YTD > 0 AND YDBM-AMOUNT-1-YTD < 5.00
               MOVE 5.00 TO YDBM-AMOUNT-1-YTD
           END-IF
           IF YDBM-AMOUNT-1-YTD > 150.00
               MOVE 150.00 TO YDBM-AMOUNT-1-YTD
           END-IF
           ADD YDBM-AMOUNT-1-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT CONTROL YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4250-SPLIT-CONTROL.
           COMPUTE WS-WORK-AMT = YDBM-CONTROL-YTD / 4
           COMPUTE WS-WORK-AMT-2 = YDBM-CONTROL-YTD - (WS-WORK-AMT * 4)
           COMPUTE YDBT-CONTROL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E694 CUST ID REQUIRED
      * E730 AMOUNT 1 AMT NEGATIVE
      * E848 AMOUNT 1 AMT OVER 25000
      * E612 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       4260-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YDBM-CUST-ID = SPACES
                   MOVE 'E694' TO WS-REJECT-CODE
               WHEN YDBT-AMOUNT-1-AMT < 0
                   MOVE 'E730' TO WS-REJECT-CODE
               WHEN YDBT-AMOUNT-1-AMT > 25000.00
                   MOVE 'E848' TO WS-REJECT-CODE
               WHEN YDBM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E612' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON AMOUNT 1 AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4270-ACCRUE-AMOUNT-1.
           IF YDBT-AMOUNT-1-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   YDBT-AMOUNT-1-AMT * YDBM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO YDBM-AMOUNT-1-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4280-DESCRIBE-REASON.
           EVALUATE YDBM-REASON-CD
               WHEN 'AX'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'LK'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'NT'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'UW'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4290-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF YDBM-LAST-STMT-DATE > 0 AND YDBM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YDBM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD YDBT-AMOUNT-2-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD YDBT-AMOUNT-2-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD YDBT-AMOUNT-2-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD YDBT-AMOUNT-2-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD YDBT-AMOUNT-2-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-120, 121-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4300-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF YDBM-NOTICE-DATE > 0 AND YDBM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YDBM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD YDBM-CONTROL-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 120
                   ADD YDBM-CONTROL-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD YDBM-CONTROL-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD YDBM-CONTROL-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD YDBM-CONTROL-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT AMOUNT 1 AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4310-SPLIT-AMOUNT-1.
           COMPUTE WS-WORK-AMT = YDBT-AMOUNT-1-AMT / 2
           COMPUTE WS-WORK-AMT-2 = YDBT-AMOUNT-1-AMT - (WS-WORK-AMT * 2)
           COMPUTE YDBM-AMOUNT-1-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CONTROL YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4320-SPLIT-CONTROL.
           COMPUTE WS-WORK-AMT = YDBM-CONTROL-YTD / 12
           COMPUTE WS-WORK-AMT-2 = YDBM-CONTROL-YTD - (WS-WORK-AMT * 12)
           COMPUTE YDBM-AMOUNT-2-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE AMOUNT 2 FEE ON THE CONTROL AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       4330-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN YDBM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO YDBT-AMOUNT-2-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YDBM-PREFERRED-FLAG = 'Y'
                   COMPUTE YDBT-AMOUNT-2-AMT ROUNDED = YDBM-CONTROL-AMT
                       * YDBM-MARGIN / 200
               WHEN YDBM-CYCLE-CD = 'FJ'
                   MOVE 10.00 TO YDBT-AMOUNT-2-AMT
               WHEN YDBM-CYCLE-CD = 'LA'
                   MOVE 3.00 TO YDBT-AMOUNT-2-AMT
               WHEN OTHER
                   COMPUTE YDBT-AMOUNT-2-AMT ROUNDED = YDBM-CONTROL-AMT
                       * YDBM-MARGIN / 100
           END-EVALUATE
           IF YDBT-AMOUNT-2-AMT > 0 AND YDBT-AMOUNT-2-AMT < 10.00
               MOVE 10.00 TO YDBT-AMOUNT-2-AMT
           END-IF
           IF YDBT-AMOUNT-2-AMT > 250.00
               MOVE 250.00 TO YDBT-AMOUNT-2-AMT
           END-IF
           ADD YDBT-AMOUNT-2-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * C -> H WHEN POST DATE IS PAST
      * H -> K WHEN POST DATE IS PAST
      * K -> S WHEN POST DATE IS PAST
      * S -> V WHEN POST DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4340-SET-REGION.
           EVALUATE YDBM-REGION-CD
               WHEN 'C'
                   IF YDBT-POST-DATE > 0 AND YDBT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO YDBM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF YDBT-POST-DATE > 0 AND YDBT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'K' TO YDBM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF YDBT-POST-DATE > 0 AND YDBT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO YDBM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF YDBT-POST-DATE > 0 AND YDBT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'V' TO YDBM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE AMOUNT 2 FEE ON THE AMOUNT 1 AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4350-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN YDBM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO YDBM-AMOUNT-2-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YDBM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE YDBM-AMOUNT-2-AMT ROUNDED = YDBT-AMOUNT-1-AMT
                       * YDBM-DISCOUNT / 200
               WHEN YDBT-CHANNEL-CD = 'BRN'
                   MOVE 15.00 TO YDBM-AMOUNT-2-AMT
               WHEN YDBT-CHANNEL-CD = 'BAT'
                   MOVE 3.00 TO YDBM-AMOUNT-2-AMT
               WHEN OTHER
                   COMPUTE YDBM-AMOUNT-2-AMT ROUNDED = YDBT-AMOUNT-1-AMT
                       * YDBM-DISCOUNT / 100
           END-EVALUATE
           IF YDBM-AMOUNT-2-AMT > 0 AND YDBM-AMOUNT-2-AMT < 10.00
               MOVE 10.00 TO YDBM-AMOUNT-2-AMT
           END-IF
           IF YDBM-AMOUNT-2-AMT > 100.00
               MOVE 100.00 TO YDBM-AMOUNT-2-AMT
           END-IF
           ADD YDBM-AMOUNT-2-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE CONTROL FEE ON THE AMOUNT 2 AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4360-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN YDBM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO YDBT-CONTROL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YDBM-REVIEW-FLAG = 'Y'
                   COMPUTE YDBT-CONTROL-AMT ROUNDED = YDBM-AMOUNT-2-AMT
                       * YDBM-DISCOUNT / 200
               WHEN YDBM-PIVOT-CD = 'KGT'
                   MOVE 15.00 TO YDBT-CONTROL-AMT
               WHEN YDBM-PIVOT-CD = 'NNG'
                   MOVE 12.00 TO YDBT-CONTROL-AMT
               WHEN OTHER
                   COMPUTE YDBT-CONTROL-AMT ROUNDED = YDBM-AMOUNT-2-AMT
                       * YDBM-DISCOUNT / 100
           END-EVALUATE
           IF YDBT-CONTROL-AMT > 0 AND YDBT-CONTROL-AMT < 2.00
               MOVE 2.00 TO YDBT-CONTROL-AMT
           END-IF
           IF YDBT-CONTROL-AMT > 35.00
               MOVE 35.00 TO YDBT-CONTROL-AMT
           END-IF
           ADD YDBT-CONTROL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4370-DESCRIBE-SOURCE.
           EVALUATE YDBM-SOURCE-CD
               WHEN 'BNX'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'FTB'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'GAP'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'PAP'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'VGC'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'VJV'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE YDB FIELDS TO YKLIM AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7040-CALL-YKLIM.
           INITIALIZE YKLIM-PARMS
           MOVE YDBM-AMOUNT-2-AMT
             TO YKLIM-AMOUNT-2-AMT
           MOVE YDBM-AMOUNT-1-YTD
             TO YKLIM-AMOUNT-1-YTD
           MOVE YDBM-CONTROL-AMT
             TO YKLIM-CONTROL-AMT
           MOVE YDBM-AMOUNT-1-AMT
             TO YKLIM-AMOUNT-1-AMT
           MOVE YDBM-AMOUNT-2-YTD
             TO YKLIM-AMOUNT-2-YTD
           MOVE YDBM-CONTROL-YTD
             TO YKLIM-CONTROL-YTD
           MOVE YDBM-LAST-MAINT-DATE
             TO YKLIM-LAST-MAINT-DATE
           MOVE YDBM-OPEN-DATE
             TO YKLIM-OPEN-DATE
           MOVE YDBM-LAST-PAY-DATE
             TO YKLIM-LAST-PAY-DATE
           MOVE YDBM-LAST-STMT-DATE
             TO YKLIM-LAST-STMT-DATE
           MOVE YDBM-PIVOT-CD
             TO YKLIM-PIVOT-CD
           MOVE YDBM-REGION-CD
             TO YKLIM-REGION-CD
           MOVE YDBM-SOURCE-CD
             TO YKLIM-SOURCE-CD
           MOVE YDBM-WINDOW-RULE-CD
             TO YKLIM-WINDOW-RULE-CD
           MOVE YDBM-PREFERRED-FLAG
             TO YKLIM-PREFERRED-FLAG
           MOVE YDBM-OVERRIDE-FLAG
             TO YKLIM-OVERRIDE-FLAG
           MOVE YDBM-NOTICE-SENT-FLAG
             TO YKLIM-NOTICE-SENT-FLAG
           MOVE YDBM-CYCLE-CNT
             TO YKLIM-CYCLE-CNT
           MOVE YDBM-PAYMENT-CNT
             TO YKLIM-PAYMENT-CNT
           MOVE YDBM-PENALTY-PCT
             TO YKLIM-PENALTY-PCT
           MOVE YDBM-DISCOUNT
             TO YKLIM-DISCOUNT
           MOVE YDBM-DESC
             TO YKLIM-DESC
           MOVE YDBM-CONTACT
             TO YKLIM-CONTACT
           CALL 'YKLIM' USING YKLIM-PARMS
           EVALUATE YKLIM-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE YKLIM-AMOUNT-1-YTD
                     TO YDBM-AMOUNT-1-YTD
                   MOVE YKLIM-AMOUNT-1-AMT
                     TO YDBM-AMOUNT-1-AMT
                   MOVE YKLIM-AMOUNT-2-YTD
                     TO YDBM-AMOUNT-2-YTD
                   MOVE YKLIM-PREFERRED-FLAG
                     TO YDBM-PREFERRED-FLAG
                   MOVE YKLIM-OVERRIDE-FLAG
                     TO YDBM-OVERRIDE-FLAG
                   MOVE YKLIM-NOTICE-SENT-FLAG
                     TO YDBM-NOTICE-SENT-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RLIM' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7040-CALL-YKLIM' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE YDB FIELDS TO YKWAIV AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7030-CALL-YKWAIV.
           INITIALIZE YKWAIV-PARMS
           MOVE YDBM-AMOUNT-1-YTD
             TO YKWAIV-AMOUNT-1-YTD
           MOVE YDBM-CONTROL-AMT
             TO YKWAIV-CONTROL-AMT
           MOVE YDBM-AMOUNT-1-AMT
             TO YKWAIV-AMOUNT-1-AMT
           MOVE YDBM-AMOUNT-2-YTD
             TO YKWAIV-AMOUNT-2-YTD
           MOVE YDBM-CONTROL-YTD
             TO YKWAIV-CONTROL-YTD
           MOVE YDBM-AMOUNT-2-AMT
             TO YKWAIV-AMOUNT-2-AMT
           MOVE YDBM-NOTICE-DATE
             TO YKWAIV-NOTICE-DATE
           MOVE YDBM-LAST-ACT-DATE
             TO YKWAIV-LAST-ACT-DATE
           MOVE YDBM-MATURITY-DATE
             TO YKWAIV-MATURITY-DATE
           MOVE YDBM-NEXT-DUE-DATE
             TO YKWAIV-NEXT-DUE-DATE
           MOVE YDBM-SOURCE-CD
             TO YKWAIV-SOURCE-CD
           MOVE YDBM-CYCLE-CD
             TO YKWAIV-CYCLE-CD
           MOVE YDBM-REASON-CD
             TO YKWAIV-REASON-CD
           MOVE YDBM-REGION-CD
             TO YKWAIV-REGION-CD
           MOVE YDBM-AUTO-RENEW-FLAG
             TO YKWAIV-AUTO-RENEW-FLAG
           MOVE YDBM-NOTICE-SENT-FLAG
             TO YKWAIV-NOTICE-SENT-FLAG
           MOVE YDBM-PREFERRED-FLAG
             TO YKWAIV-PREFERRED-FLAG
           MOVE YDBM-PAYMENT-CNT
             TO YKWAIV-PAYMENT-CNT
           MOVE YDBM-CYCLE-CNT
             TO YKWAIV-CYCLE-CNT
           MOVE YDBM-DISCOUNT
             TO YKWAIV-DISCOUNT
           MOVE YDBM-MARGIN
             TO YKWAIV-MARGIN
           MOVE YDBM-CUST-ID
             TO YKWAIV-CUST-ID
           MOVE YDBM-CONTACT
             TO YKWAIV-CONTACT
           CALL 'YKWAIV' USING YKWAIV-PARMS
           EVALUATE YKWAIV-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE YKWAIV-AUTO-RENEW-FLAG
                     TO YDBM-AUTO-RENEW-FLAG
                   MOVE YKWAIV-NOTICE-SENT-FLAG
                     TO YDBM-NOTICE-SENT-FLAG
                   MOVE YKWAIV-PREFERRED-FLAG
                     TO YDBM-PREFERRED-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RAIV' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7030-CALL-YKWAIV' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE YDB FIELDS TO YKPRC AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7000-CALL-YKPRC.
           INITIALIZE YKPRC-PARMS
           MOVE YDBM-AMOUNT-1-AMT
             TO YKPRC-AMOUNT-1-AMT
           MOVE YDBM-CONTROL-YTD
             TO YKPRC-CONTROL-YTD
           MOVE YDBM-AMOUNT-1-YTD
             TO YKPRC-AMOUNT-1-YTD
           MOVE YDBM-AMOUNT-2-AMT
             TO YKPRC-AMOUNT-2-AMT
           MOVE YDBM-CONTROL-AMT
             TO YKPRC-CONTROL-AMT
           MOVE YDBM-AMOUNT-2-YTD
             TO YKPRC-AMOUNT-2-YTD
           MOVE YDBM-EXPIRY-DATE
             TO YKPRC-EXPIRY-DATE
           MOVE YDBM-LAST-STMT-DATE
             TO YKPRC-LAST-STMT-DATE
           MOVE YDBM-MATURITY-DATE
             TO YKPRC-MATURITY-DATE
           MOVE YDBM-LAST-ACT-DATE
             TO YKPRC-LAST-ACT-DATE
           MOVE YDBM-CYCLE-CD
             TO YKPRC-CYCLE-CD
           MOVE YDBM-SOURCE-CD
             TO YKPRC-SOURCE-CD
           MOVE YDBM-PIVOT-CD
             TO YKPRC-PIVOT-CD
           MOVE YDBM-REGION-CD
             TO YKPRC-REGION-CD
           MOVE YDBM-NOTICE-SENT-FLAG
             TO YKPRC-NOTICE-SENT-FLAG
           MOVE YDBM-AUTO-RENEW-FLAG
             TO YKPRC-AUTO-RENEW-FLAG
           MOVE YDBM-PREFERRED-FLAG
             TO YKPRC-PREFERRED-FLAG
           MOVE YDBM-ITEM-CNT
             TO YKPRC-ITEM-CNT
           MOVE YDBM-PAYMENT-CNT
             TO YKPRC-PAYMENT-CNT
           MOVE YDBM-MARGIN
             TO YKPRC-MARGIN
           MOVE YDBM-DISCOUNT
             TO YKPRC-DISCOUNT
           MOVE YDBM-NAME
             TO YKPRC-NAME
           MOVE YDBM-CONTACT
             TO YKPRC-CONTACT
           CALL 'YKPRC' USING YKPRC-PARMS
           EVALUATE YKPRC-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE YKPRC-AMOUNT-1-YTD
                     TO YDBM-AMOUNT-1-YTD
                   CONTINUE
               WHEN 08
                   MOVE 'RPRC' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-YKPRC' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE YDBT-KEY TO RD-KEY
           MOVE YDBT-TRAN-CD TO RD-CODE
           MOVE YDBT-AMOUNT-1-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE YDBT-KEY TO RD-KEY
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
           DISPLAY 'YKBNOTC READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'YKBNOTC' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
