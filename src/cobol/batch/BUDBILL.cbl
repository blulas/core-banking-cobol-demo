      *================================================================*
      * PROGRAM  : BUDBILL                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : BUMP-UP CERTIFICATES - CYCLE BILLING. READS THE BCD *
      *            TRANSACTIONS, UPDATES THE BCD MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. BUMP-UP AND STEP-RATE           *
      *            CERTIFICATE RATE ELECTION AND ACCRUAL               *
      * INPUT    : BCDMST (KSDS), BCDTRN, BANKCTL                      *
      * OUTPUT   : BCDOUT, BUDBIRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, BUSTAT, BURISK          *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1992-05-28 TWH  ORIGINAL                                       *
      * 2001-07-12 RJT  PRINCIPAL CUTOFF MOVED INC3994                 *
      * 2005-03-07 GAF  PENALTY RERUN PROTECTION INC6962               *
      * 2006-04-11 LMB  PENALTY TABLE EXPANDED REQ5988                 *
      * 2007-01-05 JLM  PENALTY ROUNDING - HALF UP CHG0994             *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BUDBILL.
       AUTHOR. B H SANDERS.

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
           SELECT REPORT-FILE ASSIGN TO "BUDBIRPT"
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
                                           VALUE 10.000000.
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
      * WS-INTEREST-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-INTEREST-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000895500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000514500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000150000000095000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000400000000177500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005400000000484000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005500000000513000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005550000000812000'.
       01  WS-INTEREST-SCHED-TBL REDEFINES WS-INTEREST-SCHED-VALUES.
           05  WS-INTEREST-SCHED-ROW OCCURS 7 TIMES.
               10  WS-INTEREST-SCHED-LOW   PIC 9(09)V99.
               10  WS-INTEREST-SCHED-RATE  PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-ADDON-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-ADDON-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000188500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000549000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010050000000030000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011050000000429500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012050000000663000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017050000000157000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018050000000715000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018550000000360500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00019050000000210000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00019100000000698500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021600000000277500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022600000000422000'.
       01  WS-ADDON-BAND-TBL REDEFINES WS-ADDON-BAND-VALUES.
           05  WS-ADDON-BAND-ROW OCCURS 12 TIMES.
               10  WS-ADDON-BAND-LOW       PIC 9(09)V99.
               10  WS-ADDON-BAND-RATE      PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  BUDBILL  BUMP-UP CERTIFICATES'.
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
       COPY BUSTATP.
       COPY BURISKP.

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
               PERFORM 3000-ASSESS-SUSPEND
               PERFORM 3010-PRORATE-ADDON
               PERFORM 3020-PRORATE-ADDON
               PERFORM 3030-SET-CHANNEL
               PERFORM 3040-ACCUM-TRAN
               PERFORM 3050-LIMIT-ADDON
               PERFORM 3060-FLAG-INTEREST
               PERFORM 3070-ACCUM-SOURCE
               PERFORM 3080-EDIT-REJECT
               PERFORM 3090-PRORATE-ADDON
               PERFORM 3100-ASSESS-HOLD
               PERFORM 3110-SPLIT-PRINCIPAL
               PERFORM 3120-SELECT-PENALTY-PCT
               PERFORM 3130-SPLIT-INTEREST
               PERFORM 3140-EDIT-MEMO
               PERFORM 3150-PRORATE-BUMP
               PERFORM 3160-FLAG-INTEREST
               PERFORM 3170-COUNT-EXCEPT
               PERFORM 3180-ACCUM-ELECTION
               PERFORM 3190-SPLIT-BUMP
               PERFORM 3200-COUNT-ITEM
               PERFORM 3210-DESCRIBE-TRAN
               PERFORM 3220-COUNT-ITEM
               PERFORM 3230-DESCRIBE-CHANNEL
               PERFORM 3240-PRORATE-PENALTY
               PERFORM 3250-DESCRIBE-STEP-PLAN
               PERFORM 3260-COUNT-CYCLE
               PERFORM 3270-SELECT-PENALTY-PCT
               PERFORM 3280-SELECT-MARGIN
               PERFORM 3290-SPLIT-BUMP
               PERFORM 3300-ASSESS-HOLD
               PERFORM 3310-DESCRIBE-TRAN
               PERFORM 3320-LIMIT-ADDON
               PERFORM 3330-ASSESS-REVERSAL
               PERFORM 3340-AGE-NEXT-REVIEW
               PERFORM 3350-ACCRUE-BUMP
               PERFORM 3360-LIMIT-BUMP
               PERFORM 3370-SELECT-PENALTY-PCT
               PERFORM 3380-ACCUM-STEP-PLAN
               PERFORM 3390-SPLIT-BUMP
               PERFORM 3400-AGE-EXPIRY
               PERFORM 3410-PRORATE-PENALTY
               PERFORM 3420-EDIT-MEMO
               PERFORM 3430-PRORATE-BUMP
               PERFORM 3440-ACCRUE-BUMP
               PERFORM 3450-EDIT-DESC
               PERFORM 3460-EDIT-REF-NO
               PERFORM 3470-PRORATE-ADDON
               PERFORM 3480-FLAG-PRINCIPAL
               PERFORM 3490-DESCRIBE-ELECTION
               PERFORM 3500-COUNT-ITEM
               PERFORM 3510-SET-SOURCE
               PERFORM 3520-ASSESS-EXEMPT
               PERFORM 3530-CHECK-EFF
               PERFORM 3540-ASSESS-REVERSAL
               PERFORM 3550-AGE-EXPIRY
               PERFORM 3560-SPLIT-INTEREST
               PERFORM 3570-SET-CYCLE
               PERFORM 3580-COUNT-RETRY
               PERFORM 3590-SPLIT-INTEREST
               PERFORM 3600-ACCUM-CHANNEL
               PERFORM 3610-ASSESS-SUSPEND
               PERFORM 3620-CHECK-POST
               PERFORM 3630-LIMIT-INTEREST
               PERFORM 3640-DESCRIBE-STEP-PLAN
               PERFORM 3650-EDIT-NAME
               PERFORM 3660-ASSESS-HOLD
               PERFORM 3670-AGE-LAST-PAY
               PERFORM 3680-DESCRIBE-CYCLE
               PERFORM 3690-AGE-LAST-STMT
               PERFORM 3700-AGE-LAST-STMT
               PERFORM 3710-PRORATE-ADDON
               PERFORM 3720-SET-CYCLE
               PERFORM 3730-SPLIT-ADDON
               PERFORM 3740-SELECT-RATE
               PERFORM 3750-DESCRIBE-CHANNEL
               PERFORM 3760-PRORATE-ADDON
               PERFORM 3770-CHECK-CLOSE
               PERFORM 3780-PRORATE-BUMP
               PERFORM 3790-LIMIT-INTEREST
               PERFORM 3800-DESCRIBE-TRAN
               PERFORM 3810-COUNT-RETRY
               PERFORM 3820-EDIT-DESC
               PERFORM 3830-FLAG-BUMP
               PERFORM 3840-CHECK-EFFECTIVE
               PERFORM 3850-SELECT-PENALTY-PCT
               PERFORM 3860-SET-STEP-PLAN
               PERFORM 3870-SELECT-RATE
               PERFORM 3880-ACCRUE-ADDON
               PERFORM 3890-AGE-NEXT-DUE
               PERFORM 3900-AGE-EFFECTIVE
               PERFORM 3910-ASSESS-SUSPEND
               PERFORM 3920-ACCUM-CYCLE
               PERFORM 3930-ACCRUE-ADDON
               PERFORM 3940-SPLIT-INTEREST
               PERFORM 3950-CHECK-MATURITY
               PERFORM 3960-FLAG-INTEREST
               PERFORM 3970-CHECK-MATURITY
               PERFORM 3980-EDIT-OPERATOR
               PERFORM 3990-PRORATE-INTEREST
               PERFORM 4000-COUNT-EXCEPT
               PERFORM 4010-SPLIT-ADDON
               PERFORM 4020-AGE-EFF
               PERFORM 4030-SET-CHANNEL
               PERFORM 4040-COUNT-SEQ
               PERFORM 4050-EDIT-REJECT
               PERFORM 4060-FLAG-PENALTY
               PERFORM 4070-DESCRIBE-TRAN
               PERFORM 4080-ACCUM-TRAN
               PERFORM 4090-ACCRUE-BUMP
               PERFORM 4100-SET-STEP-PLAN
               PERFORM 4110-ACCUM-TRAN
               PERFORM 4120-AGE-LAST-PAY
               PERFORM 4130-FLAG-PENALTY
               PERFORM 4140-SPLIT-INTEREST
               PERFORM 4150-SPLIT-INTEREST
               PERFORM 4160-AGE-LAST-STMT
               PERFORM 4170-CHECK-CLOSE
               PERFORM 4180-ASSESS-REVERSAL
               PERFORM 4190-EDIT-REF-NO
               PERFORM 4200-LIMIT-ADDON
               PERFORM 4210-FLAG-ITEM
               PERFORM 4220-CHECK-LAST-STMT
               PERFORM 4230-CHECK-EFFECTIVE
               PERFORM 4240-COUNT-ITEM
               PERFORM 4250-SET-ELECTION
               PERFORM 4260-COUNT-ITEM
               PERFORM 4270-SELECT-PENALTY-PCT
               PERFORM 4280-LIMIT-PENALTY
               PERFORM 4290-SET-TRAN
               PERFORM 4300-EDIT-REF-NO
               PERFORM 7030-CALL-BUSTAT
               PERFORM 7020-CALL-BURISK
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
      * ASSESS THE BUMP YTD FEE ON THE INTEREST YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3000-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN BCDM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO BCDM-BUMP-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BCDM-STOP-FLAG = 'Y'
                   COMPUTE BCDM-BUMP-YTD ROUNDED = BCDM-INTEREST-YTD *
                       BCDM-PENALTY-PCT / 200
               WHEN BCDM-STEP-PLAN-CD = 'AYX'
                   MOVE 10.00 TO BCDM-BUMP-YTD
               WHEN BCDM-STEP-PLAN-CD = 'DGU'
                   MOVE 20.00 TO BCDM-BUMP-YTD
               WHEN OTHER
                   COMPUTE BCDM-BUMP-YTD ROUNDED = BCDM-INTEREST-YTD *
                       BCDM-PENALTY-PCT / 100
           END-EVALUATE
           IF BCDM-BUMP-YTD > 0 AND BCDM-BUMP-YTD < 1.00
               MOVE 1.00 TO BCDM-BUMP-YTD
           END-IF
           IF BCDM-BUMP-YTD > 250.00
               MOVE 250.00 TO BCDM-BUMP-YTD
           END-IF
           ADD BCDM-BUMP-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ADDON AMT.
      *----------------------------------------------------------------*
       3010-PRORATE-ADDON.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BCDM-ADDON-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BCDM-ADDON-AMT
           END-IF
           ADD BCDM-ADDON-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE BUMP AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ADDON YTD.
      *----------------------------------------------------------------*
       3020-PRORATE-ADDON.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BCDM-ADDON-YTD ROUNDED =
                   BCDT-BUMP-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BCDM-ADDON-YTD
           END-IF
           ADD BCDM-ADDON-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN LAST STMT DATE IS PAST
      * BAT -> MAL WHEN LAST STMT DATE IS PAST
      * MAL -> TEL WHEN LAST STMT DATE IS PAST
      * TEL -> ONL WHEN LAST STMT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3030-SET-CHANNEL.
           EVALUATE BCDT-CHANNEL-CD
               WHEN 'BRN'
                   IF BCDM-LAST-STMT-DATE > 0 AND BCDM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO BCDT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF BCDM-LAST-STMT-DATE > 0 AND BCDM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO BCDT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF BCDM-LAST-STMT-DATE > 0 AND BCDM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO BCDT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF BCDM-LAST-STMT-DATE > 0 AND BCDM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO BCDT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE INTEREST AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3040-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BCDT-TRAN-CD
                   ADD BCDM-INTEREST-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BCDT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BCDM-INTEREST-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ADDON AMT IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3050-LIMIT-ADDON.
           IF BCDM-ADDON-AMT < 10.00
               MOVE 10.00 TO BCDM-ADDON-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BCDM-ADDON-AMT > 10000.00
               MOVE 10000.00 TO BCDM-ADDON-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INTEREST YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3060-FLAG-INTEREST.
           IF BCDM-INTEREST-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE BCDM-INTEREST-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3070-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BCDM-SOURCE-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BCDM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E441 REJECT CD REQUIRED
      * E322 BUMP YTD NEGATIVE
      * E950 BUMP YTD OVER 99999
      * E993 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3080-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BCDT-REJECT-CD = SPACES
                   MOVE 'E441' TO WS-REJECT-CODE
               WHEN BCDM-BUMP-YTD < 0
                   MOVE 'E322' TO WS-REJECT-CODE
               WHEN BCDM-BUMP-YTD > 99999.00
                   MOVE 'E950' TO WS-REJECT-CODE
               WHEN BCDM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E993' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ADDON AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ADDON AMT.
      *----------------------------------------------------------------*
       3090-PRORATE-ADDON.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BCDT-ADDON-AMT ROUNDED =
                   BCDM-ADDON-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BCDT-ADDON-AMT
           END-IF
           ADD BCDT-ADDON-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE PRINCIPAL FEE ON THE INTEREST YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3100-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN BCDM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO BCDM-PRINCIPAL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BCDM-REVIEW-FLAG = 'Y'
                   COMPUTE BCDM-PRINCIPAL-AMT ROUNDED =
                       BCDM-INTEREST-YTD * BCDM-RATE / 200
               WHEN BCDM-REGION-CD = 'BGJ'
                   MOVE 15.00 TO BCDM-PRINCIPAL-AMT
               WHEN BCDM-REGION-CD = 'LFX'
                   MOVE 15.00 TO BCDM-PRINCIPAL-AMT
               WHEN OTHER
                   COMPUTE BCDM-PRINCIPAL-AMT ROUNDED =
                       BCDM-INTEREST-YTD * BCDM-RATE / 100
           END-EVALUATE
           IF BCDM-PRINCIPAL-AMT > 0 AND BCDM-PRINCIPAL-AMT < 1.00
               MOVE 1.00 TO BCDM-PRINCIPAL-AMT
           END-IF
           IF BCDM-PRINCIPAL-AMT > 50.00
               MOVE 50.00 TO BCDM-PRINCIPAL-AMT
           END-IF
           ADD BCDM-PRINCIPAL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT PRINCIPAL YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3110-SPLIT-PRINCIPAL.
           COMPUTE WS-WORK-AMT = BCDM-PRINCIPAL-YTD / 3
           COMPUTE WS-WORK-AMT-2 = BCDM-PRINCIPAL-YTD - (WS-WORK-AMT *
               3)
           COMPUTE BCDT-ADDON-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ADDON BAND TIER FOR THE ADDON AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3120-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ADDON-BAND-RATE(1) TO BCDM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF BCDT-ADDON-AMT NOT < WS-ADDON-BAND-LOW(WS-TX)
                   MOVE WS-ADDON-BAND-RATE(WS-TX) TO BCDM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BCDM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BCDM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT INTEREST AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3130-SPLIT-INTEREST.
           COMPUTE WS-WORK-AMT = BCDM-INTEREST-AMT / 2
           COMPUTE WS-WORK-AMT-2 = BCDM-INTEREST-AMT - (WS-WORK-AMT * 2)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E043 MEMO REQUIRED
      * E103 ADDON AMT NEGATIVE
      * E871 ADDON AMT OVER 250000
      * E804 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3140-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BCDT-MEMO = SPACES
                   MOVE 'E043' TO WS-REJECT-CODE
               WHEN BCDT-ADDON-AMT < 0
                   MOVE 'E103' TO WS-REJECT-CODE
               WHEN BCDT-ADDON-AMT > 250000.00
                   MOVE 'E871' TO WS-REJECT-CODE
               WHEN BCDT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E804' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ADDON YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO BUMP AMT.
      *----------------------------------------------------------------*
       3150-PRORATE-BUMP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BCDT-BUMP-AMT ROUNDED =
                   BCDM-ADDON-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BCDT-BUMP-AMT
           END-IF
           ADD BCDT-BUMP-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * INTEREST AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3160-FLAG-INTEREST.
           IF BCDT-INTEREST-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE BCDT-INTEREST-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3170-COUNT-EXCEPT.
           IF BCDM-EXCEPT-CNT < 9999999
               ADD 1 TO BCDM-EXCEPT-CNT
           END-IF
           IF BCDM-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO BCDM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE BUMP AMT BY ELECTION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3180-ACCUM-ELECTION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BCDM-ELECTION-CD
                   ADD BCDM-BUMP-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BCDM-ELECTION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BCDM-BUMP-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT BUMP AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3190-SPLIT-BUMP.
           COMPUTE WS-WORK-AMT = BCDT-BUMP-AMT / 3
           COMPUTE WS-WORK-AMT-2 = BCDT-BUMP-AMT - (WS-WORK-AMT * 3)
           COMPUTE BCDM-INTEREST-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3200-COUNT-ITEM.
           IF BCDM-ITEM-CNT < 9999999
               ADD 1 TO BCDM-ITEM-CNT
           END-IF
           IF BCDM-ITEM-CNT NOT < 12
               MOVE 'Y' TO BCDM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3210-DESCRIBE-TRAN.
           EVALUATE BCDT-TRAN-CD
               WHEN 'BU'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'AD'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'WD'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'MT'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3220-COUNT-ITEM.
           IF BCDM-ITEM-CNT < 9999999
               ADD 1 TO BCDM-ITEM-CNT
           END-IF
           IF BCDM-ITEM-CNT NOT < 5
               MOVE 'Y' TO BCDM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3230-DESCRIBE-CHANNEL.
           EVALUATE BCDM-CHANNEL-CD
               WHEN 'EK'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'JE'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'PN'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'WV'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE ADDON AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PENALTY AMT.
      *----------------------------------------------------------------*
       3240-PRORATE-PENALTY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BCDT-PENALTY-AMT ROUNDED =
                   BCDM-ADDON-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BCDT-PENALTY-AMT
           END-IF
           ADD BCDT-PENALTY-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE STEP PLAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3250-DESCRIBE-STEP-PLAN.
           EVALUATE BCDM-STEP-PLAN-CD
               WHEN 'AYX'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'DGU'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'GWJ'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'RJL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'UPM'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'VCX'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'YPY'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3260-COUNT-CYCLE.
           IF BCDM-CYCLE-CNT < 9999999
               ADD 1 TO BCDM-CYCLE-CNT
           END-IF
           IF BCDM-CYCLE-CNT NOT < 5
               MOVE 'Y' TO BCDT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ADDON BAND TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3270-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ADDON-BAND-RATE(1) TO BCDM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF WS-WORK-AMT-2 NOT < WS-ADDON-BAND-LOW(WS-TX)
                   MOVE WS-ADDON-BAND-RATE(WS-TX) TO BCDM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BCDM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BCDM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE INTEREST SCHED TIER FOR THE BUMP AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3280-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-INTEREST-SCHED-RATE(1) TO BCDM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF BCDT-BUMP-AMT NOT < WS-INTEREST-SCHED-LOW(WS-TX)
                   MOVE WS-INTEREST-SCHED-RATE(WS-TX) TO BCDM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BCDM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BCDM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT BUMP AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3290-SPLIT-BUMP.
           COMPUTE WS-WORK-AMT = BCDT-BUMP-AMT / 4
           COMPUTE WS-WORK-AMT-2 = BCDT-BUMP-AMT - (WS-WORK-AMT * 4)
           COMPUTE BCDT-PENALTY-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE INTEREST FEE ON THE PRINCIPAL AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3300-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN BCDM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO BCDT-INTEREST-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BCDM-EXEMPT-FLAG = 'Y'
                   COMPUTE BCDT-INTEREST-AMT ROUNDED =
                       BCDM-PRINCIPAL-AMT * BCDM-RATE / 200
               WHEN BCDM-STEP-PLAN-CD = 'AYX'
                   MOVE 20.00 TO BCDT-INTEREST-AMT
               WHEN BCDM-STEP-PLAN-CD = 'DGU'
                   MOVE 5.00 TO BCDT-INTEREST-AMT
               WHEN OTHER
                   COMPUTE BCDT-INTEREST-AMT ROUNDED =
                       BCDM-PRINCIPAL-AMT * BCDM-RATE / 100
           END-EVALUATE
           IF BCDT-INTEREST-AMT > 0 AND BCDT-INTEREST-AMT < 1.00
               MOVE 1.00 TO BCDT-INTEREST-AMT
           END-IF
           IF BCDT-INTEREST-AMT > 75.00
               MOVE 75.00 TO BCDT-INTEREST-AMT
           END-IF
           ADD BCDT-INTEREST-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3310-DESCRIBE-TRAN.
           EVALUATE BCDT-TRAN-CD
               WHEN 'BU'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'AD'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'WD'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'MT'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ADDON AMT IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3320-LIMIT-ADDON.
           IF BCDM-ADDON-AMT < 25.00
               MOVE 25.00 TO BCDM-ADDON-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BCDM-ADDON-AMT > 50000.00
               MOVE 50000.00 TO BCDM-ADDON-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ADDON YTD FEE ON THE PRINCIPAL YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3330-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN BCDT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO BCDM-ADDON-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BCDM-EXEMPT-FLAG = 'Y'
                   COMPUTE BCDM-ADDON-YTD ROUNDED = BCDM-PRINCIPAL-YTD *
                       BCDM-RATE / 200
               WHEN BCDM-STEP-PLAN-CD = 'AYX'
                   MOVE 7.00 TO BCDM-ADDON-YTD
               WHEN BCDM-STEP-PLAN-CD = 'DGU'
                   MOVE 20.00 TO BCDM-ADDON-YTD
               WHEN OTHER
                   COMPUTE BCDM-ADDON-YTD ROUNDED = BCDM-PRINCIPAL-YTD *
                       BCDM-RATE / 100
           END-EVALUATE
           IF BCDM-ADDON-YTD > 0 AND BCDM-ADDON-YTD < 1.00
               MOVE 1.00 TO BCDM-ADDON-YTD
           END-IF
           IF BCDM-ADDON-YTD > 100.00
               MOVE 100.00 TO BCDM-ADDON-YTD
           END-IF
           ADD BCDM-ADDON-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3340-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF BCDM-NEXT-REVIEW-DATE > 0 AND BCDM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BCDM-INTEREST-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD BCDM-INTEREST-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD BCDM-INTEREST-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD BCDM-INTEREST-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BCDM-INTEREST-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON BUMP AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3350-ACCRUE-BUMP.
           IF BCDT-BUMP-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BCDT-BUMP-AMT * BCDM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO BCDM-PRINCIPAL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * BUMP AMT IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3360-LIMIT-BUMP.
           IF BCDM-BUMP-AMT < 25.00
               MOVE 25.00 TO BCDM-BUMP-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BCDM-BUMP-AMT > 10000.00
               MOVE 10000.00 TO BCDM-BUMP-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE INTEREST SCHED TIER FOR THE INTEREST YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3370-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-INTEREST-SCHED-RATE(1) TO BCDM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF BCDM-INTEREST-YTD NOT < WS-INTEREST-SCHED-LOW(WS-TX)
                   MOVE WS-INTEREST-SCHED-RATE(WS-TX) TO
                       BCDM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BCDM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BCDM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE INTEREST AMT BY STEP PLAN CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3380-ACCUM-STEP-PLAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BCDM-STEP-PLAN-CD
                   ADD BCDM-INTEREST-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BCDM-STEP-PLAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BCDM-INTEREST-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT BUMP YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3390-SPLIT-BUMP.
           COMPUTE WS-WORK-AMT = BCDM-BUMP-YTD / 3
           COMPUTE WS-WORK-AMT-2 = BCDM-BUMP-YTD - (WS-WORK-AMT * 3)
           COMPUTE BCDM-PENALTY-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3400-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF BCDM-EXPIRY-DATE > 0 AND BCDM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BCDM-PENALTY-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD BCDM-PENALTY-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD BCDM-PENALTY-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD BCDM-PENALTY-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BCDM-PENALTY-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE PENALTY AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3410-PRORATE-PENALTY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   BCDM-PENALTY-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E564 MEMO REQUIRED
      * E974 PENALTY AMT NEGATIVE
      * E801 PENALTY AMT OVER 1000000
      * E613 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3420-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BCDT-MEMO = SPACES
                   MOVE 'E564' TO WS-REJECT-CODE
               WHEN BCDT-PENALTY-AMT < 0
                   MOVE 'E974' TO WS-REJECT-CODE
               WHEN BCDT-PENALTY-AMT > 1000000.00
                   MOVE 'E801' TO WS-REJECT-CODE
               WHEN BCDM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E613' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ADDON AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO BUMP AMT.
      *----------------------------------------------------------------*
       3430-PRORATE-BUMP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BCDT-BUMP-AMT ROUNDED =
                   BCDT-ADDON-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BCDT-BUMP-AMT
           END-IF
           ADD BCDT-BUMP-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON BUMP AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3440-ACCRUE-BUMP.
           IF BCDM-BUMP-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BCDM-BUMP-AMT * BCDM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO BCDM-BUMP-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E495 DESC REQUIRED
      * E519 PENALTY AMT NEGATIVE
      * E931 PENALTY AMT OVER 10000
      * E385 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3450-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BCDM-DESC = SPACES
                   MOVE 'E495' TO WS-REJECT-CODE
               WHEN BCDM-PENALTY-AMT < 0
                   MOVE 'E519' TO WS-REJECT-CODE
               WHEN BCDM-PENALTY-AMT > 10000.00
                   MOVE 'E931' TO WS-REJECT-CODE
               WHEN BCDM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E385' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E728 REF NO REQUIRED
      * E928 PRINCIPAL AMT NEGATIVE
      * E836 PRINCIPAL AMT OVER 99999
      * E431 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3460-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BCDM-REF-NO = SPACES
                   MOVE 'E728' TO WS-REJECT-CODE
               WHEN BCDM-PRINCIPAL-AMT < 0
                   MOVE 'E928' TO WS-REJECT-CODE
               WHEN BCDM-PRINCIPAL-AMT > 99999.00
                   MOVE 'E836' TO WS-REJECT-CODE
               WHEN BCDM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E431' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE INTEREST YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ADDON AMT.
      *----------------------------------------------------------------*
       3470-PRORATE-ADDON.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BCDM-ADDON-AMT ROUNDED =
                   BCDM-INTEREST-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BCDM-ADDON-AMT
           END-IF
           ADD BCDM-ADDON-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRINCIPAL AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3480-FLAG-PRINCIPAL.
           IF BCDM-PRINCIPAL-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE BCDM-PRINCIPAL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE ELECTION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3490-DESCRIBE-ELECTION.
           EVALUATE BCDM-ELECTION-CD
               WHEN 'L'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'M'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'N'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'R'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'V'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3500-COUNT-ITEM.
           IF BCDM-ITEM-CNT < 9999999
               ADD 1 TO BCDM-ITEM-CNT
           END-IF
           IF BCDM-ITEM-CNT NOT < 5
               MOVE 'Y' TO BCDM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * AG -> BF WHEN LAST PAY DATE IS PAST
      * BF -> HF WHEN LAST PAY DATE IS PAST
      * HF -> HR WHEN LAST PAY DATE IS PAST
      * HR -> UF WHEN LAST PAY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3510-SET-SOURCE.
           EVALUATE BCDM-SOURCE-CD
               WHEN 'AG'
                   IF BCDM-LAST-PAY-DATE > 0 AND BCDM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BF' TO BCDM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BF'
                   IF BCDM-LAST-PAY-DATE > 0 AND BCDM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HF' TO BCDM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HF'
                   IF BCDM-LAST-PAY-DATE > 0 AND BCDM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HR' TO BCDM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HR'
                   IF BCDM-LAST-PAY-DATE > 0 AND BCDM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UF' TO BCDM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE PENALTY FEE ON THE BUMP AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3520-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN BCDM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO BCDT-PENALTY-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BCDT-REVERSAL-FLAG = 'Y'
                   COMPUTE BCDT-PENALTY-AMT ROUNDED = BCDT-BUMP-AMT *
                       BCDM-PENALTY-PCT / 200
               WHEN BCDM-CYCLE-CD = 'A'
                   MOVE 12.00 TO BCDT-PENALTY-AMT
               WHEN BCDM-CYCLE-CD = 'S'
                   MOVE 3.00 TO BCDT-PENALTY-AMT
               WHEN OTHER
                   COMPUTE BCDT-PENALTY-AMT ROUNDED = BCDT-BUMP-AMT *
                       BCDM-PENALTY-PCT / 100
           END-EVALUATE
           IF BCDT-PENALTY-AMT > 0 AND BCDT-PENALTY-AMT < 2.00
               MOVE 2.00 TO BCDT-PENALTY-AMT
           END-IF
           IF BCDT-PENALTY-AMT > 50.00
               MOVE 50.00 TO BCDT-PENALTY-AMT
           END-IF
           ADD BCDT-PENALTY-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 365 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3530-CHECK-EFF.
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
           IF WS-DAYS > 365
               MOVE 'Y' TO BCDM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PRINCIPAL FEE ON THE ADDON YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3540-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN BCDT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO BCDM-PRINCIPAL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BCDM-HOLD-FLAG = 'Y'
                   COMPUTE BCDM-PRINCIPAL-AMT ROUNDED = BCDM-ADDON-YTD *
                       BCDM-RATE / 200
               WHEN BCDT-CHANNEL-CD = 'BRN'
                   MOVE 3.00 TO BCDM-PRINCIPAL-AMT
               WHEN BCDT-CHANNEL-CD = 'BAT'
                   MOVE 20.00 TO BCDM-PRINCIPAL-AMT
               WHEN OTHER
                   COMPUTE BCDM-PRINCIPAL-AMT ROUNDED = BCDM-ADDON-YTD *
                       BCDM-RATE / 100
           END-EVALUATE
           IF BCDM-PRINCIPAL-AMT > 0 AND BCDM-PRINCIPAL-AMT < 1.00
               MOVE 1.00 TO BCDM-PRINCIPAL-AMT
           END-IF
           IF BCDM-PRINCIPAL-AMT > 25.00
               MOVE 25.00 TO BCDM-PRINCIPAL-AMT
           END-IF
           ADD BCDM-PRINCIPAL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3550-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF BCDM-EXPIRY-DATE > 0 AND BCDM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT INTEREST AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3560-SPLIT-INTEREST.
           COMPUTE WS-WORK-AMT = BCDT-INTEREST-AMT / 12
           COMPUTE WS-WORK-AMT-2 = BCDT-INTEREST-AMT - (WS-WORK-AMT *
               12)
           COMPUTE BCDT-PENALTY-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * A -> S WHEN CLOSE DATE IS PAST
      * S -> T WHEN CLOSE DATE IS PAST
      * T -> U WHEN CLOSE DATE IS PAST
      * U -> X WHEN CLOSE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3570-SET-CYCLE.
           EVALUATE BCDM-CYCLE-CD
               WHEN 'A'
                   IF BCDM-CLOSE-DATE > 0 AND BCDM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO BCDM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF BCDM-CLOSE-DATE > 0 AND BCDM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO BCDM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF BCDM-CLOSE-DATE > 0 AND BCDM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO BCDM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF BCDM-CLOSE-DATE > 0 AND BCDM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'X' TO BCDM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3580-COUNT-RETRY.
           IF BCDM-RETRY-CNT < 9999999
               ADD 1 TO BCDM-RETRY-CNT
           END-IF
           IF BCDM-RETRY-CNT NOT < 12
               MOVE 'Y' TO BCDM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT INTEREST AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3590-SPLIT-INTEREST.
           COMPUTE WS-WORK-AMT = BCDM-INTEREST-AMT / 4
           COMPUTE WS-WORK-AMT-2 = BCDM-INTEREST-AMT - (WS-WORK-AMT * 4)
           COMPUTE BCDT-BUMP-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ADDON AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3600-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BCDM-CHANNEL-CD
                   ADD BCDT-ADDON-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BCDM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BCDT-ADDON-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ADDON FEE ON THE ADDON AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3610-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN BCDM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO BCDT-ADDON-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BCDM-EXEMPT-FLAG = 'Y'
                   COMPUTE BCDT-ADDON-AMT ROUNDED = BCDM-ADDON-AMT *
                       BCDM-PENALTY-PCT / 200
               WHEN BCDT-CHANNEL-CD = 'BRN'
                   MOVE 20.00 TO BCDT-ADDON-AMT
               WHEN BCDT-CHANNEL-CD = 'BAT'
                   MOVE 3.00 TO BCDT-ADDON-AMT
               WHEN OTHER
                   COMPUTE BCDT-ADDON-AMT ROUNDED = BCDM-ADDON-AMT *
                       BCDM-PENALTY-PCT / 100
           END-EVALUATE
           IF BCDT-ADDON-AMT > 0 AND BCDT-ADDON-AMT < 5.00
               MOVE 5.00 TO BCDT-ADDON-AMT
           END-IF
           IF BCDT-ADDON-AMT > 100.00
               MOVE 100.00 TO BCDT-ADDON-AMT
           END-IF
           ADD BCDT-ADDON-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 90 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3620-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF BCDT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO BCDM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INTEREST YTD IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3630-LIMIT-INTEREST.
           IF BCDM-INTEREST-YTD < 25.00
               MOVE 25.00 TO BCDM-INTEREST-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BCDM-INTEREST-YTD > 50000.00
               MOVE 50000.00 TO BCDM-INTEREST-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STEP PLAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3640-DESCRIBE-STEP-PLAN.
           EVALUATE BCDM-STEP-PLAN-CD
               WHEN 'AYX'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'DGU'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'GWJ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'RJL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'UPM'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'VCX'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'YPY'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E331 NAME REQUIRED
      * E198 PRINCIPAL AMT NEGATIVE
      * E021 PRINCIPAL AMT OVER 10000
      * E790 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3650-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BCDM-NAME = SPACES
                   MOVE 'E331' TO WS-REJECT-CODE
               WHEN BCDM-PRINCIPAL-AMT < 0
                   MOVE 'E198' TO WS-REJECT-CODE
               WHEN BCDM-PRINCIPAL-AMT > 10000.00
                   MOVE 'E021' TO WS-REJECT-CODE
               WHEN BCDM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E790' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE INTEREST YTD FEE ON THE BUMP AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3660-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN BCDM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO BCDM-INTEREST-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BCDM-STOP-FLAG = 'Y'
                   COMPUTE BCDM-INTEREST-YTD ROUNDED = BCDT-BUMP-AMT *
                       BCDM-PENALTY-PCT / 200
               WHEN BCDM-ELECTION-CD = 'L'
                   MOVE 7.00 TO BCDM-INTEREST-YTD
               WHEN BCDM-ELECTION-CD = 'M'
                   MOVE 7.00 TO BCDM-INTEREST-YTD
               WHEN OTHER
                   COMPUTE BCDM-INTEREST-YTD ROUNDED = BCDT-BUMP-AMT *
                       BCDM-PENALTY-PCT / 100
           END-EVALUATE
           IF BCDM-INTEREST-YTD > 0 AND BCDM-INTEREST-YTD < 10.00
               MOVE 10.00 TO BCDM-INTEREST-YTD
           END-IF
           IF BCDM-INTEREST-YTD > 150.00
               MOVE 150.00 TO BCDM-INTEREST-YTD
           END-IF
           ADD BCDM-INTEREST-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3670-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF BCDM-LAST-PAY-DATE > 0 AND BCDM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BCDM-INTEREST-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD BCDM-INTEREST-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD BCDM-INTEREST-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD BCDM-INTEREST-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BCDM-INTEREST-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3680-DESCRIBE-CYCLE.
           EVALUATE BCDM-CYCLE-CD
               WHEN 'A'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'S'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'T'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'U'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'X'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'Y'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3690-AGE-LAST-STMT.
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
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3700-AGE-LAST-STMT.
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
                   ADD BCDM-ADDON-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD BCDM-ADDON-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD BCDM-ADDON-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD BCDM-ADDON-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BCDM-ADDON-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ADDON YTD.
      *----------------------------------------------------------------*
       3710-PRORATE-ADDON.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BCDM-ADDON-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BCDM-ADDON-YTD
           END-IF
           ADD BCDM-ADDON-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * A -> S WHEN POST DATE IS PAST
      * S -> T WHEN POST DATE IS PAST
      * T -> U WHEN POST DATE IS PAST
      * U -> X WHEN POST DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3720-SET-CYCLE.
           EVALUATE BCDM-CYCLE-CD
               WHEN 'A'
                   IF BCDT-POST-DATE > 0 AND BCDT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO BCDM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF BCDT-POST-DATE > 0 AND BCDT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO BCDM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF BCDT-POST-DATE > 0 AND BCDT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO BCDM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF BCDT-POST-DATE > 0 AND BCDT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'X' TO BCDM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT ADDON AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3730-SPLIT-ADDON.
           COMPUTE WS-WORK-AMT = BCDT-ADDON-AMT / 12
           COMPUTE WS-WORK-AMT-2 = BCDT-ADDON-AMT - (WS-WORK-AMT * 12)
           COMPUTE BCDM-ADDON-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ADDON BAND TIER FOR THE ADDON AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3740-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-ADDON-BAND-RATE(1) TO BCDM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF BCDT-ADDON-AMT NOT < WS-ADDON-BAND-LOW(WS-TX)
                   MOVE WS-ADDON-BAND-RATE(WS-TX) TO BCDM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BCDM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BCDM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3750-DESCRIBE-CHANNEL.
           EVALUATE BCDM-CHANNEL-CD
               WHEN 'EK'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'JE'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'PN'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'WV'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE BUMP YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ADDON AMT.
      *----------------------------------------------------------------*
       3760-PRORATE-ADDON.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BCDM-ADDON-AMT ROUNDED =
                   BCDM-BUMP-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BCDM-ADDON-AMT
           END-IF
           ADD BCDM-ADDON-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3770-CHECK-CLOSE.
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
               MOVE 'Y' TO BCDM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE BUMP YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3780-PRORATE-BUMP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   BCDM-BUMP-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * INTEREST YTD IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3790-LIMIT-INTEREST.
           IF BCDM-INTEREST-YTD < 100.00
               MOVE 100.00 TO BCDM-INTEREST-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BCDM-INTEREST-YTD > 5000.00
               MOVE 5000.00 TO BCDM-INTEREST-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3800-DESCRIBE-TRAN.
           EVALUATE BCDT-TRAN-CD
               WHEN 'BU'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'AD'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'WD'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'MT'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3810-COUNT-RETRY.
           IF BCDM-RETRY-CNT < 9999999
               ADD 1 TO BCDM-RETRY-CNT
           END-IF
           IF BCDM-RETRY-CNT NOT < 3
               MOVE 'Y' TO BCDM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E722 DESC REQUIRED
      * E169 INTEREST YTD NEGATIVE
      * E319 INTEREST YTD OVER 1000000
      * E231 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3820-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BCDM-DESC = SPACES
                   MOVE 'E722' TO WS-REJECT-CODE
               WHEN BCDM-INTEREST-YTD < 0
                   MOVE 'E169' TO WS-REJECT-CODE
               WHEN BCDM-INTEREST-YTD > 1000000.00
                   MOVE 'E319' TO WS-REJECT-CODE
               WHEN BCDM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E231' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * BUMP AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3830-FLAG-BUMP.
           IF BCDT-BUMP-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE BCDT-BUMP-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3840-CHECK-EFFECTIVE.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO BCDM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ADDON BAND TIER FOR THE INTEREST AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3850-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ADDON-BAND-RATE(1) TO BCDM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF BCDM-INTEREST-AMT NOT < WS-ADDON-BAND-LOW(WS-TX)
                   MOVE WS-ADDON-BAND-RATE(WS-TX) TO BCDM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BCDM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BCDM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STEP PLAN CD:
      * AYX -> DGU WHEN LAST PAY DATE IS PAST
      * DGU -> GWJ WHEN LAST PAY DATE IS PAST
      * GWJ -> RJL WHEN LAST PAY DATE IS PAST
      * RJL -> UPM WHEN LAST PAY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3860-SET-STEP-PLAN.
           EVALUATE BCDM-STEP-PLAN-CD
               WHEN 'AYX'
                   IF BCDM-LAST-PAY-DATE > 0 AND BCDM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DGU' TO BCDM-STEP-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DGU'
                   IF BCDM-LAST-PAY-DATE > 0 AND BCDM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GWJ' TO BCDM-STEP-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GWJ'
                   IF BCDM-LAST-PAY-DATE > 0 AND BCDM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RJL' TO BCDM-STEP-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RJL'
                   IF BCDM-LAST-PAY-DATE > 0 AND BCDM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UPM' TO BCDM-STEP-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE ADDON BAND TIER FOR THE INTEREST AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3870-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-ADDON-BAND-RATE(1) TO BCDM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF BCDT-INTEREST-AMT NOT < WS-ADDON-BAND-LOW(WS-TX)
                   MOVE WS-ADDON-BAND-RATE(WS-TX) TO BCDM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BCDM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BCDM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ADDON YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3880-ACCRUE-ADDON.
           IF BCDM-ADDON-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BCDM-ADDON-YTD * BCDM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO BCDM-ADDON-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3890-AGE-NEXT-DUE.
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
                   ADD BCDM-PRINCIPAL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD BCDM-PRINCIPAL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD BCDM-PRINCIPAL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD BCDM-PRINCIPAL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BCDM-PRINCIPAL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3900-AGE-EFFECTIVE.
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
                   ADD BCDM-PRINCIPAL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD BCDM-PRINCIPAL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD BCDM-PRINCIPAL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD BCDM-PRINCIPAL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BCDM-PRINCIPAL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE BUMP FEE ON THE PENALTY AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3910-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN BCDM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO BCDM-BUMP-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BCDM-STOP-FLAG = 'Y'
                   COMPUTE BCDM-BUMP-AMT ROUNDED = BCDM-PENALTY-AMT *
                       BCDM-PENALTY-PCT / 200
               WHEN BCDM-CYCLE-CD = 'A'
                   MOVE 7.00 TO BCDM-BUMP-AMT
               WHEN BCDM-CYCLE-CD = 'S'
                   MOVE 5.00 TO BCDM-BUMP-AMT
               WHEN OTHER
                   COMPUTE BCDM-BUMP-AMT ROUNDED = BCDM-PENALTY-AMT *
                       BCDM-PENALTY-PCT / 100
           END-EVALUATE
           IF BCDM-BUMP-AMT > 0 AND BCDM-BUMP-AMT < 5.00
               MOVE 5.00 TO BCDM-BUMP-AMT
           END-IF
           IF BCDM-BUMP-AMT > 100.00
               MOVE 100.00 TO BCDM-BUMP-AMT
           END-IF
           ADD BCDM-BUMP-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE PENALTY AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3920-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BCDM-CYCLE-CD
                   ADD BCDT-PENALTY-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BCDM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BCDT-PENALTY-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ADDON AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3930-ACCRUE-ADDON.
           IF BCDT-ADDON-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BCDT-ADDON-AMT * BCDM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO BCDT-BUMP-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT INTEREST AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3940-SPLIT-INTEREST.
           COMPUTE WS-WORK-AMT = BCDT-INTEREST-AMT / 12
           COMPUTE WS-WORK-AMT-2 = BCDT-INTEREST-AMT - (WS-WORK-AMT *
               12)
           COMPUTE BCDM-INTEREST-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 45 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3950-CHECK-MATURITY.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO BCDM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INTEREST YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3960-FLAG-INTEREST.
           IF BCDM-INTEREST-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE BCDM-INTEREST-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 10 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3970-CHECK-MATURITY.
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
               MOVE 'Y' TO BCDM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E375 OPERATOR REQUIRED
      * E727 BUMP AMT NEGATIVE
      * E720 BUMP AMT OVER 1000000
      * E387 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3980-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BCDT-OPERATOR = SPACES
                   MOVE 'E375' TO WS-REJECT-CODE
               WHEN BCDM-BUMP-AMT < 0
                   MOVE 'E727' TO WS-REJECT-CODE
               WHEN BCDM-BUMP-AMT > 1000000.00
                   MOVE 'E720' TO WS-REJECT-CODE
               WHEN BCDM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E387' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE BUMP AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO INTEREST YTD.
      *----------------------------------------------------------------*
       3990-PRORATE-INTEREST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BCDM-INTEREST-YTD ROUNDED =
                   BCDM-BUMP-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BCDM-INTEREST-YTD
           END-IF
           ADD BCDM-INTEREST-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4000-COUNT-EXCEPT.
           IF BCDM-EXCEPT-CNT < 9999999
               ADD 1 TO BCDM-EXCEPT-CNT
           END-IF
           IF BCDM-EXCEPT-CNT NOT < 12
               MOVE 'Y' TO BCDT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ADDON AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4010-SPLIT-ADDON.
           COMPUTE WS-WORK-AMT = BCDT-ADDON-AMT / 12
           COMPUTE WS-WORK-AMT-2 = BCDT-ADDON-AMT - (WS-WORK-AMT * 12)
           COMPUTE BCDM-BUMP-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4020-AGE-EFF.
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
                   ADD BCDM-PENALTY-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD BCDM-PENALTY-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD BCDM-PENALTY-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD BCDM-PENALTY-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BCDM-PENALTY-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN EXPIRY DATE IS PAST
      * BAT -> MAL WHEN EXPIRY DATE IS PAST
      * MAL -> TEL WHEN EXPIRY DATE IS PAST
      * TEL -> ONL WHEN EXPIRY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4030-SET-CHANNEL.
           EVALUATE BCDT-CHANNEL-CD
               WHEN 'BRN'
                   IF BCDM-EXPIRY-DATE > 0 AND BCDM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO BCDT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF BCDM-EXPIRY-DATE > 0 AND BCDM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO BCDT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF BCDM-EXPIRY-DATE > 0 AND BCDM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO BCDT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF BCDM-EXPIRY-DATE > 0 AND BCDM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO BCDT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4040-COUNT-SEQ.
           IF BCDT-SEQ < 9999999
               ADD 1 TO BCDT-SEQ
           END-IF
           IF BCDT-SEQ NOT < 3
               MOVE 'Y' TO BCDM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E889 REJECT CD REQUIRED
      * E006 INTEREST AMT NEGATIVE
      * E493 INTEREST AMT OVER 1000000
      * E726 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       4050-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BCDT-REJECT-CD = SPACES
                   MOVE 'E889' TO WS-REJECT-CODE
               WHEN BCDM-INTEREST-AMT < 0
                   MOVE 'E006' TO WS-REJECT-CODE
               WHEN BCDM-INTEREST-AMT > 1000000.00
                   MOVE 'E493' TO WS-REJECT-CODE
               WHEN BCDM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E726' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PENALTY AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4060-FLAG-PENALTY.
           IF BCDT-PENALTY-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE BCDT-PENALTY-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4070-DESCRIBE-TRAN.
           EVALUATE BCDT-TRAN-CD
               WHEN 'BU'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'AD'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'WD'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'MT'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE INTEREST AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4080-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BCDT-TRAN-CD
                   ADD BCDM-INTEREST-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BCDT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BCDM-INTEREST-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON BUMP AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4090-ACCRUE-BUMP.
           IF BCDT-BUMP-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BCDT-BUMP-AMT * BCDM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO BCDM-INTEREST-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STEP PLAN CD:
      * AYX -> DGU WHEN LAST MAINT DATE IS PAST
      * DGU -> GWJ WHEN LAST MAINT DATE IS PAST
      * GWJ -> RJL WHEN LAST MAINT DATE IS PAST
      * RJL -> UPM WHEN LAST MAINT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4100-SET-STEP-PLAN.
           EVALUATE BCDM-STEP-PLAN-CD
               WHEN 'AYX'
                   IF BCDM-LAST-MAINT-DATE > 0 AND BCDM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'DGU' TO BCDM-STEP-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DGU'
                   IF BCDM-LAST-MAINT-DATE > 0 AND BCDM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'GWJ' TO BCDM-STEP-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GWJ'
                   IF BCDM-LAST-MAINT-DATE > 0 AND BCDM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'RJL' TO BCDM-STEP-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RJL'
                   IF BCDM-LAST-MAINT-DATE > 0 AND BCDM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'UPM' TO BCDM-STEP-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE PENALTY AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4110-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BCDT-TRAN-CD
                   ADD BCDT-PENALTY-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BCDT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BCDT-PENALTY-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4120-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF BCDM-LAST-PAY-DATE > 0 AND BCDM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BCDM-PRINCIPAL-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD BCDM-PRINCIPAL-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD BCDM-PRINCIPAL-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD BCDM-PRINCIPAL-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BCDM-PRINCIPAL-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PENALTY AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4130-FLAG-PENALTY.
           IF BCDT-PENALTY-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE BCDT-PENALTY-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT INTEREST AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4140-SPLIT-INTEREST.
           COMPUTE WS-WORK-AMT = BCDT-INTEREST-AMT / 12
           COMPUTE WS-WORK-AMT-2 = BCDT-INTEREST-AMT - (WS-WORK-AMT *
               12)
           COMPUTE BCDM-PRINCIPAL-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT INTEREST YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4150-SPLIT-INTEREST.
           COMPUTE WS-WORK-AMT = BCDM-INTEREST-YTD / 4
           COMPUTE WS-WORK-AMT-2 = BCDM-INTEREST-YTD - (WS-WORK-AMT * 4)
           COMPUTE BCDM-ADDON-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4160-AGE-LAST-STMT.
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
                   ADD BCDM-PENALTY-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD BCDM-PENALTY-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD BCDM-PENALTY-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD BCDM-PENALTY-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BCDM-PENALTY-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4170-CHECK-CLOSE.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO BCDM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PENALTY FEE ON THE PENALTY AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4180-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN BCDT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO BCDM-PENALTY-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BCDM-SUSPEND-FLAG = 'Y'
                   COMPUTE BCDM-PENALTY-AMT ROUNDED = BCDT-PENALTY-AMT *
                       BCDM-MARGIN / 200
               WHEN BCDT-TRAN-CD = 'BU'
                   MOVE 20.00 TO BCDM-PENALTY-AMT
               WHEN BCDT-TRAN-CD = 'AD'
                   MOVE 20.00 TO BCDM-PENALTY-AMT
               WHEN OTHER
                   COMPUTE BCDM-PENALTY-AMT ROUNDED = BCDT-PENALTY-AMT *
                       BCDM-MARGIN / 100
           END-EVALUATE
           IF BCDM-PENALTY-AMT > 0 AND BCDM-PENALTY-AMT < 1.00
               MOVE 1.00 TO BCDM-PENALTY-AMT
           END-IF
           IF BCDM-PENALTY-AMT > 35.00
               MOVE 35.00 TO BCDM-PENALTY-AMT
           END-IF
           ADD BCDM-PENALTY-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E011 REF NO REQUIRED
      * E401 PENALTY AMT NEGATIVE
      * E533 PENALTY AMT OVER 99999
      * E351 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       4190-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BCDM-REF-NO = SPACES
                   MOVE 'E011' TO WS-REJECT-CODE
               WHEN BCDM-PENALTY-AMT < 0
                   MOVE 'E401' TO WS-REJECT-CODE
               WHEN BCDM-PENALTY-AMT > 99999.00
                   MOVE 'E533' TO WS-REJECT-CODE
               WHEN BCDM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E351' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ADDON YTD IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4200-LIMIT-ADDON.
           IF BCDM-ADDON-YTD < 10.00
               MOVE 10.00 TO BCDM-ADDON-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BCDM-ADDON-YTD > 2500.00
               MOVE 2500.00 TO BCDM-ADDON-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4210-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 15 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4220-CHECK-LAST-STMT.
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
           IF WS-DAYS > 15
               MOVE 'Y' TO BCDT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4230-CHECK-EFFECTIVE.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO BCDM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       4240-COUNT-ITEM.
           IF BCDM-ITEM-CNT < 9999999
               ADD 1 TO BCDM-ITEM-CNT
           END-IF
           IF BCDM-ITEM-CNT NOT < 6
               MOVE 'Y' TO BCDM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ELECTION CD:
      * L -> M WHEN LAST STMT DATE IS PAST
      * M -> N WHEN LAST STMT DATE IS PAST
      * N -> R WHEN LAST STMT DATE IS PAST
      * R -> V WHEN LAST STMT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4250-SET-ELECTION.
           EVALUATE BCDM-ELECTION-CD
               WHEN 'L'
                   IF BCDM-LAST-STMT-DATE > 0 AND BCDM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO BCDM-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF BCDM-LAST-STMT-DATE > 0 AND BCDM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'N' TO BCDM-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'N'
                   IF BCDM-LAST-STMT-DATE > 0 AND BCDM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'R' TO BCDM-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF BCDM-LAST-STMT-DATE > 0 AND BCDM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'V' TO BCDM-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       4260-COUNT-ITEM.
           IF BCDM-ITEM-CNT < 9999999
               ADD 1 TO BCDM-ITEM-CNT
           END-IF
           IF BCDM-ITEM-CNT NOT < 5
               MOVE 'Y' TO BCDM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE INTEREST SCHED TIER FOR THE INTEREST AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4270-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-INTEREST-SCHED-RATE(1) TO BCDM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF BCDT-INTEREST-AMT NOT < WS-INTEREST-SCHED-LOW(WS-TX)
                   MOVE WS-INTEREST-SCHED-RATE(WS-TX) TO
                       BCDM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BCDM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BCDM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PENALTY YTD IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4280-LIMIT-PENALTY.
           IF BCDM-PENALTY-YTD < 0.00
               MOVE 0.00 TO BCDM-PENALTY-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BCDM-PENALTY-YTD > 10000.00
               MOVE 10000.00 TO BCDM-PENALTY-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * BU -> AD WHEN NEXT DUE DATE IS PAST
      * AD -> WD WHEN NEXT DUE DATE IS PAST
      * WD -> MT WHEN NEXT DUE DATE IS PAST
      * MT -> BU WHEN NEXT DUE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4290-SET-TRAN.
           EVALUATE BCDT-TRAN-CD
               WHEN 'BU'
                   IF BCDM-NEXT-DUE-DATE > 0 AND BCDM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AD' TO BCDT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AD'
                   IF BCDM-NEXT-DUE-DATE > 0 AND BCDM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WD' TO BCDT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WD'
                   IF BCDM-NEXT-DUE-DATE > 0 AND BCDM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MT' TO BCDT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MT'
                   IF BCDM-NEXT-DUE-DATE > 0 AND BCDM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BU' TO BCDT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E160 REF NO REQUIRED
      * E281 INTEREST AMT NEGATIVE
      * E475 INTEREST AMT OVER 5000
      * E476 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       4300-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BCDM-REF-NO = SPACES
                   MOVE 'E160' TO WS-REJECT-CODE
               WHEN BCDM-INTEREST-AMT < 0
                   MOVE 'E281' TO WS-REJECT-CODE
               WHEN BCDM-INTEREST-AMT > 5000.00
                   MOVE 'E475' TO WS-REJECT-CODE
               WHEN BCDT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E476' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE BCD FIELDS TO BUSTAT AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7030-CALL-BUSTAT.
           INITIALIZE BUSTAT-PARMS
           MOVE BCDM-PENALTY-YTD
             TO BUSTAT-PENALTY-YTD
           MOVE BCDM-ADDON-AMT
             TO BUSTAT-ADDON-AMT
           MOVE BCDM-BUMP-AMT
             TO BUSTAT-BUMP-AMT
           MOVE BCDM-PENALTY-AMT
             TO BUSTAT-PENALTY-AMT
           MOVE BCDM-PRINCIPAL-AMT
             TO BUSTAT-PRINCIPAL-AMT
           MOVE BCDM-INTEREST-YTD
             TO BUSTAT-INTEREST-YTD
           MOVE BCDM-LAST-PAY-DATE
             TO BUSTAT-LAST-PAY-DATE
           MOVE BCDM-EXPIRY-DATE
             TO BUSTAT-EXPIRY-DATE
           MOVE BCDM-MATURITY-DATE
             TO BUSTAT-MATURITY-DATE
           MOVE BCDM-LAST-STMT-DATE
             TO BUSTAT-LAST-STMT-DATE
           MOVE BCDM-ELECTION-CD
             TO BUSTAT-ELECTION-CD
           MOVE BCDM-CHANNEL-CD
             TO BUSTAT-CHANNEL-CD
           MOVE BCDM-SOURCE-CD
             TO BUSTAT-SOURCE-CD
           MOVE BCDM-STEP-PLAN-CD
             TO BUSTAT-STEP-PLAN-CD
           MOVE BCDM-OVERRIDE-FLAG
             TO BUSTAT-OVERRIDE-FLAG
           MOVE BCDM-REVIEW-FLAG
             TO BUSTAT-REVIEW-FLAG
           MOVE BCDM-STOP-FLAG
             TO BUSTAT-STOP-FLAG
           MOVE BCDM-CYCLE-CNT
             TO BUSTAT-CYCLE-CNT
           MOVE BCDM-ITEM-CNT
             TO BUSTAT-ITEM-CNT
           MOVE BCDM-PENALTY-PCT
             TO BUSTAT-PENALTY-PCT
           MOVE BCDM-RATE
             TO BUSTAT-RATE
           MOVE BCDM-ADDR
             TO BUSTAT-ADDR
           MOVE BCDM-NAME
             TO BUSTAT-NAME
           CALL 'BUSTAT' USING BUSTAT-PARMS
           EVALUATE BUSTAT-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE BUSTAT-ADDON-AMT
                     TO BCDM-ADDON-AMT
                   MOVE BUSTAT-BUMP-AMT
                     TO BCDM-BUMP-AMT
                   MOVE BUSTAT-PRINCIPAL-AMT
                     TO BCDM-PRINCIPAL-AMT
                   MOVE BUSTAT-STOP-FLAG
                     TO BCDM-STOP-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RTAT' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7030-CALL-BUSTAT' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE BCD FIELDS TO BURISK AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7020-CALL-BURISK.
           INITIALIZE BURISK-PARMS
           MOVE BCDM-PENALTY-YTD
             TO BURISK-PENALTY-YTD
           MOVE BCDM-INTEREST-YTD
             TO BURISK-INTEREST-YTD
           MOVE BCDM-ADDON-AMT
             TO BURISK-ADDON-AMT
           MOVE BCDM-ADDON-YTD
             TO BURISK-ADDON-YTD
           MOVE BCDM-PRINCIPAL-AMT
             TO BURISK-PRINCIPAL-AMT
           MOVE BCDM-INTEREST-AMT
             TO BURISK-INTEREST-AMT
           MOVE BCDM-LAST-MAINT-DATE
             TO BURISK-LAST-MAINT-DATE
           MOVE BCDM-LAST-PAY-DATE
             TO BURISK-LAST-PAY-DATE
           MOVE BCDM-NEXT-REVIEW-DATE
             TO BURISK-NEXT-REVIEW-DATE
           MOVE BCDM-LAST-STMT-DATE
             TO BURISK-LAST-STMT-DATE
           MOVE BCDM-CYCLE-CD
             TO BURISK-CYCLE-CD
           MOVE BCDM-ELECTION-CD
             TO BURISK-ELECTION-CD
           MOVE BCDM-REGION-CD
             TO BURISK-REGION-CD
           MOVE BCDM-CHANNEL-CD
             TO BURISK-CHANNEL-CD
           MOVE BCDM-REVIEW-FLAG
             TO BURISK-REVIEW-FLAG
           MOVE BCDM-HOLD-FLAG
             TO BURISK-HOLD-FLAG
           MOVE BCDM-EXEMPT-FLAG
             TO BURISK-EXEMPT-FLAG
           MOVE BCDM-EXCEPT-CNT
             TO BURISK-EXCEPT-CNT
           MOVE BCDM-CYCLE-CNT
             TO BURISK-CYCLE-CNT
           MOVE BCDM-PENALTY-PCT
             TO BURISK-PENALTY-PCT
           MOVE BCDM-MARGIN
             TO BURISK-MARGIN
           MOVE BCDM-DESC
             TO BURISK-DESC
           MOVE BCDM-CUST-ID
             TO BURISK-CUST-ID
           CALL 'BURISK' USING BURISK-PARMS
           EVALUATE BURISK-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE BURISK-PENALTY-YTD
                     TO BCDM-PENALTY-YTD
                   MOVE BURISK-ADDON-AMT
                     TO BCDM-ADDON-AMT
                   MOVE BURISK-ADDON-YTD
                     TO BCDM-ADDON-YTD
                   MOVE BURISK-REVIEW-FLAG
                     TO BCDM-REVIEW-FLAG
                   MOVE BURISK-HOLD-FLAG
                     TO BCDM-HOLD-FLAG
                   MOVE BURISK-EXEMPT-FLAG
                     TO BCDM-EXEMPT-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RISK' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-BURISK' TO ABND-PARAGRAPH
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
           DISPLAY 'BUDBILL READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'BUDBILL' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
