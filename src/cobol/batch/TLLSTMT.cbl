      *================================================================*
      * PROGRAM  : TLLSTMT                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : TELLER BALANCING - STATEMENT PREPARATION. READS THE *
      *            TBL TRANSACTIONS, UPDATES THE TBL MASTER AND        *
      *            PRODUCES THE CONTROL REPORT. TELLER CASH DRAWER     *
      *            BALANCING, BATCH PROOF AND OVER/SHORT               *
      * INPUT    : TBLMST (KSDS), TBLTRN, BANKCTL                      *
      * OUTPUT   : TBLOUT, TLLSTRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, TLWAIV                  *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1990-02-24 EWS  ORIGINAL                                       *
      * 1992-08-02 RJT  OVERSHORT CUTOFF MOVED CHG7839                 *
      * 1994-10-01 PQR  VAULT-SELL STATUS CODES ADDED PRB4194          *
      * 2000-01-01 GAF  VAULT-BUY NOTICE TEXT PER LEGAL INC4370        *
      * 2001-03-07 EWS  VAULT-SELL STATUS CODES ADDED CHG8666          *
      * 2002-07-06 LMB  VAULT-SELL FEE SCHEDULE UPDATE PRB3864         *
      * 2005-09-08 BHS  AUDIT FINDING - OVERSHORT CONTROL TOTALS PRB220*
      * 2008-06-20 PQR  PERFORMANCE - CASH-IN READ AHEAD REQ2669       *
      * 2012-03-21 GAF  VAULT-SELL TABLE EXPANDED REQ4887              *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TLLSTMT.
       AUTHOR. T W HOLT.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "TBLMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS TBLM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "TBLTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "TBLOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "TLLSTRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY TBLMST.
       FD  TRAN-FILE.
       COPY TBLTRN.
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
      * WS-VAULT-BU-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-VAULT-BU-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000753000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000744500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015000000000124000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020000000000478000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020250000000031000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021250000000691000'.
       01  WS-VAULT-BU-GRID-TBL REDEFINES WS-VAULT-BU-GRID-VALUES.
           05  WS-VAULT-BU-GRID-ROW OCCURS 6 TIMES.
               10  WS-VAULT-BU-GRID-LOW    PIC 9(09)V99.
               10  WS-VAULT-BU-GRID-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-CASH-OUT-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CASH-OUT-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000534000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000739000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002600000000581500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005100000000215000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005600000000262000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010600000000184500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015600000000007500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018100000000681000'.
       01  WS-CASH-OUT-BAND-TBL REDEFINES WS-CASH-OUT-BAND-VALUES.
           05  WS-CASH-OUT-BAND-ROW OCCURS 8 TIMES.
               10  WS-CASH-OUT-BAND-LOW    PIC 9(09)V99.
               10  WS-CASH-OUT-BAND-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-CASH-IN-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CASH-IN-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000527000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000052500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000150000000297500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000321000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000750000000655500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003250000000452000'.
       01  WS-CASH-IN-TIER-TBL REDEFINES WS-CASH-IN-TIER-VALUES.
           05  WS-CASH-IN-TIER-ROW OCCURS 6 TIMES.
               10  WS-CASH-IN-TIER-LOW     PIC 9(09)V99.
               10  WS-CASH-IN-TIER-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-VAULT-SE-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-VAULT-SE-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000374500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000249500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000600000000427500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000850000000043000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000900000000127000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005900000000844500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005950000000452000'.
       01  WS-VAULT-SE-SCHED-TBL REDEFINES WS-VAULT-SE-SCHED-VALUES.
           05  WS-VAULT-SE-SCHED-ROW OCCURS 7 TIMES.
               10  WS-VAULT-SE-SCHED-LOW   PIC 9(09)V99.
               10  WS-VAULT-SE-SCHED-RATE  PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  TLLSTMT  TELLER BALANCING'.
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
       COPY TLWAIVP.

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
               MOVE 'TBLMST' TO ABND-FILE-NAME
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
           MOVE TBLT-KEY TO TBLM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-EDIT-OPERATOR
               PERFORM 3010-SET-REASON
               PERFORM 3020-ASSESS-SUSPEND
               PERFORM 3030-FLAG-CASH-OUT
               PERFORM 3040-ASSESS-EXEMPT
               PERFORM 3050-LIMIT-CASH-OUT
               PERFORM 3060-COUNT-ITEM
               PERFORM 3070-LIMIT-VAULT-SELL
               PERFORM 3080-SET-CHANNEL
               PERFORM 3090-DESCRIBE-PROOF-TYPE
               PERFORM 3100-SET-CYCLE
               PERFORM 3110-SELECT-RATE
               PERFORM 3120-ASSESS-EXEMPT
               PERFORM 3130-ACCUM-REASON
               PERFORM 3140-DESCRIBE-CYCLE
               PERFORM 3150-EDIT-CONTACT
               PERFORM 3160-SPLIT-OVERSHORT
               PERFORM 3170-AGE-EXPIRY
               PERFORM 3180-PRORATE-CASH-OUT
               PERFORM 3190-DESCRIBE-TRAN
               PERFORM 3200-PRORATE-OVERSHORT
               PERFORM 3210-ASSESS-REVERSAL
               PERFORM 3220-EDIT-ADDR
               PERFORM 3230-ASSESS-REVERSAL
               PERFORM 3240-ACCRUE-DISCOUNT
               PERFORM 3250-LIMIT-CASH-OUT
               PERFORM 3260-EDIT-REF-NO
               PERFORM 3270-PRORATE-OVERSHORT
               PERFORM 3280-FLAG-BATCH
               PERFORM 3290-FLAG-OVERSHORT
               PERFORM 3300-COUNT-RETRY
               PERFORM 3310-SPLIT-BATCH
               PERFORM 3320-AGE-LAST-PAY
               PERFORM 3330-COUNT-RETRY
               PERFORM 3340-PRORATE-CASH-OUT
               PERFORM 3350-SPLIT-VAULT-SELL
               PERFORM 3360-PRORATE-BATCH
               PERFORM 3370-CHECK-EXPIRY
               PERFORM 3380-ASSESS-REVIEW
               PERFORM 3390-SELECT-RATE
               PERFORM 3400-COUNT-RETRY
               PERFORM 3410-SELECT-DISCOUNT
               PERFORM 3420-DESCRIBE-CYCLE
               PERFORM 3430-AGE-EFFECTIVE
               PERFORM 3440-AGE-OPEN
               PERFORM 3450-ACCUM-PROOF-TYPE
               PERFORM 3460-SELECT-FEE-PCT
               PERFORM 3470-FLAG-CASH-OUT
               PERFORM 3480-SET-CYCLE
               PERFORM 3490-ASSESS-OVERRIDE
               PERFORM 3500-CHECK-LAST-STMT
               PERFORM 3510-FLAG-OVERSHORT
               PERFORM 3520-AGE-OPEN
               PERFORM 3530-FLAG-VAULT-SELL
               PERFORM 3540-COUNT-EXCEPT
               PERFORM 3550-LIMIT-CASH-IN
               PERFORM 3560-AGE-NEXT-REVIEW
               PERFORM 3570-SET-REASON
               PERFORM 3580-FLAG-VAULT-SELL
               PERFORM 3590-EDIT-CONTACT
               PERFORM 3600-SELECT-DISCOUNT
               PERFORM 3610-FLAG-ITEM
               PERFORM 3620-AGE-EFFECTIVE
               PERFORM 3630-PRORATE-VAULT-SELL
               PERFORM 3640-SET-TRAN
               PERFORM 3650-COUNT-EXCEPT
               PERFORM 3660-SPLIT-VAULT-BUY
               PERFORM 3670-LIMIT-CASH-OUT
               PERFORM 3680-ASSESS-HOLD
               PERFORM 3690-AGE-LAST-ACT
               PERFORM 3700-ACCUM-CHANNEL
               PERFORM 3710-ACCUM-DRAWER
               PERFORM 3720-ACCUM-CHANNEL
               PERFORM 3730-FLAG-CASH-OUT
               PERFORM 3740-ACCRUE-CASH-OUT
               PERFORM 3750-SELECT-DISCOUNT
               PERFORM 3760-DESCRIBE-REASON
               PERFORM 3770-ACCUM-DRAWER
               PERFORM 3780-DESCRIBE-CHANNEL
               PERFORM 3790-SELECT-FEE-PCT
               PERFORM 3800-FLAG-VAULT-SELL
               PERFORM 3810-LIMIT-VAULT-SELL
               PERFORM 3820-LIMIT-VAULT-BUY
               PERFORM 3830-LIMIT-OVERSHORT
               PERFORM 3840-FLAG-BATCH
               PERFORM 3850-SELECT-FEE-PCT
               PERFORM 3860-ACCRUE-OVERSHORT
               PERFORM 3870-SPLIT-OVERSHORT
               PERFORM 3880-ASSESS-OVERRIDE
               PERFORM 3890-DESCRIBE-CHANNEL
               PERFORM 3900-LIMIT-CASH-OUT
               PERFORM 3910-AGE-LAST-PAY
               PERFORM 3920-FLAG-VAULT-SELL
               PERFORM 3930-ACCRUE-VAULT-SELL
               PERFORM 3940-CHECK-POST
               PERFORM 3950-CHECK-EFF
               PERFORM 3960-AGE-EFFECTIVE
               PERFORM 3970-DESCRIBE-DRAWER
               PERFORM 3980-EDIT-DESC
               PERFORM 3990-LIMIT-CASH-OUT
               PERFORM 4000-SELECT-RATE
               PERFORM 4010-PRORATE-BATCH
               PERFORM 4020-LIMIT-CASH-IN
               PERFORM 4030-LIMIT-VAULT-BUY
               PERFORM 4040-CHECK-LAST-ACT
               PERFORM 4050-ASSESS-REVERSAL
               PERFORM 4060-PRORATE-OVERSHORT
               PERFORM 4070-DESCRIBE-SOURCE
               PERFORM 4080-AGE-NEXT-REVIEW
               PERFORM 4090-EDIT-NAME
               PERFORM 4100-CHECK-POST
               PERFORM 4110-ACCRUE-OVERSHORT
               PERFORM 4120-FLAG-OVERSHORT
               PERFORM 4130-DESCRIBE-CHANNEL
               PERFORM 4140-ACCUM-CYCLE
               PERFORM 4150-PRORATE-CASH-OUT
               PERFORM 4160-CHECK-LAST-PAY
               PERFORM 4170-ACCRUE-OVERSHORT
               PERFORM 4180-COUNT-ITEM
               PERFORM 4190-AGE-LAST-MAINT
               PERFORM 4200-ACCUM-SOURCE
               PERFORM 4210-ASSESS-SUSPEND
               PERFORM 4220-AGE-OPEN
               PERFORM 4230-COUNT-RETRY
               PERFORM 7020-CALL-TLWAIV
               REWRITE TBL-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'TBLMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM TBL-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E170 OPERATOR REQUIRED
      * E011 VAULT SELL AMT NEGATIVE
      * E940 VAULT SELL AMT OVER 10000
      * E554 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3000-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLT-OPERATOR = SPACES
                   MOVE 'E170' TO WS-REJECT-CODE
               WHEN TBLM-VAULT-SELL-AMT < 0
                   MOVE 'E011' TO WS-REJECT-CODE
               WHEN TBLM-VAULT-SELL-AMT > 10000.00
                   MOVE 'E940' TO WS-REJECT-CODE
               WHEN TBLM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E554' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * GS -> GV WHEN EFFECTIVE DATE IS PAST
      * GV -> PG WHEN EFFECTIVE DATE IS PAST
      * PG -> SX WHEN EFFECTIVE DATE IS PAST
      * SX -> XN WHEN EFFECTIVE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3010-SET-REASON.
           EVALUATE TBLM-REASON-CD
               WHEN 'GS'
                   IF TBLM-EFFECTIVE-DATE > 0 AND TBLM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GV' TO TBLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GV'
                   IF TBLM-EFFECTIVE-DATE > 0 AND TBLM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PG' TO TBLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PG'
                   IF TBLM-EFFECTIVE-DATE > 0 AND TBLM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SX' TO TBLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SX'
                   IF TBLM-EFFECTIVE-DATE > 0 AND TBLM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XN' TO TBLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE CASH OUT YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3020-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN TBLM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLM-HOLD-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = TBLM-CASH-OUT-YTD *
                       TBLM-DISCOUNT / 200
               WHEN TBLT-CHANNEL-CD = 'BRN'
                   MOVE 3.00 TO WS-WORK-AMT
               WHEN TBLT-CHANNEL-CD = 'BAT'
                   MOVE 12.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = TBLM-CASH-OUT-YTD *
                       TBLM-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 1.00
               MOVE 1.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 250.00
               MOVE 250.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * CASH OUT AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3030-FLAG-CASH-OUT.
           IF TBLT-CASH-OUT-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TBLT-CASH-OUT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE OVERSHORT FEE ON THE OVERSHORT YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3040-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN TBLM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO TBLM-OVERSHORT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLT-REVERSAL-FLAG = 'Y'
                   COMPUTE TBLM-OVERSHORT-AMT ROUNDED =
                       TBLM-OVERSHORT-YTD * TBLM-DISCOUNT / 200
               WHEN TBLM-SOURCE-CD = 'AF'
                   MOVE 20.00 TO TBLM-OVERSHORT-AMT
               WHEN TBLM-SOURCE-CD = 'PT'
                   MOVE 5.00 TO TBLM-OVERSHORT-AMT
               WHEN OTHER
                   COMPUTE TBLM-OVERSHORT-AMT ROUNDED =
                       TBLM-OVERSHORT-YTD * TBLM-DISCOUNT / 100
           END-EVALUATE
           IF TBLM-OVERSHORT-AMT > 0 AND TBLM-OVERSHORT-AMT < 1.00
               MOVE 1.00 TO TBLM-OVERSHORT-AMT
           END-IF
           IF TBLM-OVERSHORT-AMT > 250.00
               MOVE 250.00 TO TBLM-OVERSHORT-AMT
           END-IF
           ADD TBLM-OVERSHORT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * CASH OUT YTD IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3050-LIMIT-CASH-OUT.
           IF TBLM-CASH-OUT-YTD < 10.00
               MOVE 10.00 TO TBLM-CASH-OUT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLM-CASH-OUT-YTD > 500.00
               MOVE 500.00 TO TBLM-CASH-OUT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3060-COUNT-ITEM.
           IF TBLM-ITEM-CNT < 9999999
               ADD 1 TO TBLM-ITEM-CNT
           END-IF
           IF TBLM-ITEM-CNT NOT < 3
               MOVE 'Y' TO TBLM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * VAULT SELL YTD IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3070-LIMIT-VAULT-SELL.
           IF TBLM-VAULT-SELL-YTD < 0.00
               MOVE 0.00 TO TBLM-VAULT-SELL-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLM-VAULT-SELL-YTD > 500.00
               MOVE 500.00 TO TBLM-VAULT-SELL-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * D -> F WHEN LAST MAINT DATE IS PAST
      * F -> M WHEN LAST MAINT DATE IS PAST
      * M -> N WHEN LAST MAINT DATE IS PAST
      * N -> R WHEN LAST MAINT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3080-SET-CHANNEL.
           EVALUATE TBLM-CHANNEL-CD
               WHEN 'D'
                   IF TBLM-LAST-MAINT-DATE > 0 AND TBLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'F' TO TBLM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'F'
                   IF TBLM-LAST-MAINT-DATE > 0 AND TBLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'M' TO TBLM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF TBLM-LAST-MAINT-DATE > 0 AND TBLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'N' TO TBLM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'N'
                   IF TBLM-LAST-MAINT-DATE > 0 AND TBLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'R' TO TBLM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE PROOF TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3090-DESCRIBE-PROOF-TYPE.
           EVALUATE TBLM-PROOF-TYPE-CD
               WHEN 'JG'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'RX'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'TW'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'UG'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'XY'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * KX -> TA WHEN LAST STMT DATE IS PAST
      * TA -> TG WHEN LAST STMT DATE IS PAST
      * TG -> WG WHEN LAST STMT DATE IS PAST
      * WG -> KX WHEN LAST STMT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3100-SET-CYCLE.
           EVALUATE TBLM-CYCLE-CD
               WHEN 'KX'
                   IF TBLM-LAST-STMT-DATE > 0 AND TBLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TA' TO TBLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TA'
                   IF TBLM-LAST-STMT-DATE > 0 AND TBLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TG' TO TBLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TG'
                   IF TBLM-LAST-STMT-DATE > 0 AND TBLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WG' TO TBLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WG'
                   IF TBLM-LAST-STMT-DATE > 0 AND TBLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KX' TO TBLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE VAULT SE SCHED TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3110-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-VAULT-SE-SCHED-RATE(1) TO TBLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF WS-WORK-AMT NOT < WS-VAULT-SE-SCHED-LOW(WS-TX)
                   MOVE WS-VAULT-SE-SCHED-RATE(WS-TX) TO TBLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TBLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TBLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CASH OUT FEE ON THE OVERSHORT AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3120-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN TBLM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO TBLT-CASH-OUT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLM-OVERRIDE-FLAG = 'Y'
                   COMPUTE TBLT-CASH-OUT-AMT ROUNDED =
                       TBLT-OVERSHORT-AMT * TBLM-RATE / 200
               WHEN TBLM-DRAWER-CD = 'D'
                   MOVE 20.00 TO TBLT-CASH-OUT-AMT
               WHEN TBLM-DRAWER-CD = 'F'
                   MOVE 10.00 TO TBLT-CASH-OUT-AMT
               WHEN OTHER
                   COMPUTE TBLT-CASH-OUT-AMT ROUNDED =
                       TBLT-OVERSHORT-AMT * TBLM-RATE / 100
           END-EVALUATE
           IF TBLT-CASH-OUT-AMT > 0 AND TBLT-CASH-OUT-AMT < 5.00
               MOVE 5.00 TO TBLT-CASH-OUT-AMT
           END-IF
           IF TBLT-CASH-OUT-AMT > 250.00
               MOVE 250.00 TO TBLT-CASH-OUT-AMT
           END-IF
           ADD TBLT-CASH-OUT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE VAULT SELL YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3130-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TBLM-REASON-CD
                   ADD TBLM-VAULT-SELL-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TBLM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TBLM-VAULT-SELL-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3140-DESCRIBE-CYCLE.
           EVALUATE TBLM-CYCLE-CD
               WHEN 'KX'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'TA'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'TG'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'WG'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E645 CONTACT REQUIRED
      * E567 CASH OUT AMT NEGATIVE
      * E550 CASH OUT AMT OVER 250000
      * E502 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3150-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLM-CONTACT = SPACES
                   MOVE 'E645' TO WS-REJECT-CODE
               WHEN TBLM-CASH-OUT-AMT < 0
                   MOVE 'E567' TO WS-REJECT-CODE
               WHEN TBLM-CASH-OUT-AMT > 250000.00
                   MOVE 'E550' TO WS-REJECT-CODE
               WHEN TBLM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E502' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT OVERSHORT YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3160-SPLIT-OVERSHORT.
           COMPUTE WS-WORK-AMT = TBLM-OVERSHORT-YTD / 2
           COMPUTE WS-WORK-AMT-2 = TBLM-OVERSHORT-YTD - (WS-WORK-AMT *
               2)
           COMPUTE TBLT-OVERSHORT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3170-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF TBLM-EXPIRY-DATE > 0 AND TBLM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TBLM-CASH-OUT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TBLM-CASH-OUT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD TBLM-CASH-OUT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD TBLM-CASH-OUT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLM-CASH-OUT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE OVERSHORT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CASH OUT YTD.
      *----------------------------------------------------------------*
       3180-PRORATE-CASH-OUT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLM-CASH-OUT-YTD ROUNDED =
                   TBLM-OVERSHORT-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TBLM-CASH-OUT-YTD
           END-IF
           ADD TBLM-CASH-OUT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3190-DESCRIBE-TRAN.
           EVALUATE TBLT-TRAN-CD
               WHEN 'BI'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'BO'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'VB'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'VS'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'OS'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE VAULT SELL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO OVERSHORT YTD.
      *----------------------------------------------------------------*
       3200-PRORATE-OVERSHORT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLM-OVERSHORT-YTD ROUNDED =
                   TBLT-VAULT-SELL-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TBLM-OVERSHORT-YTD
           END-IF
           ADD TBLM-OVERSHORT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE VAULT SELL FEE ON THE VAULT SELL YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3210-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN TBLT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO TBLM-VAULT-SELL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLM-HOLD-FLAG = 'Y'
                   COMPUTE TBLM-VAULT-SELL-AMT ROUNDED =
                       TBLM-VAULT-SELL-YTD * TBLM-RATE / 200
               WHEN TBLM-PROOF-TYPE-CD = 'JG'
                   MOVE 5.00 TO TBLM-VAULT-SELL-AMT
               WHEN TBLM-PROOF-TYPE-CD = 'RX'
                   MOVE 5.00 TO TBLM-VAULT-SELL-AMT
               WHEN OTHER
                   COMPUTE TBLM-VAULT-SELL-AMT ROUNDED =
                       TBLM-VAULT-SELL-YTD * TBLM-RATE / 100
           END-EVALUATE
           IF TBLM-VAULT-SELL-AMT > 0 AND TBLM-VAULT-SELL-AMT < 2.00
               MOVE 2.00 TO TBLM-VAULT-SELL-AMT
           END-IF
           IF TBLM-VAULT-SELL-AMT > 100.00
               MOVE 100.00 TO TBLM-VAULT-SELL-AMT
           END-IF
           ADD TBLM-VAULT-SELL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E886 ADDR REQUIRED
      * E606 CASH OUT AMT NEGATIVE
      * E623 CASH OUT AMT OVER 1000000
      * E360 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3220-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLM-ADDR = SPACES
                   MOVE 'E886' TO WS-REJECT-CODE
               WHEN TBLT-CASH-OUT-AMT < 0
                   MOVE 'E606' TO WS-REJECT-CODE
               WHEN TBLT-CASH-OUT-AMT > 1000000.00
                   MOVE 'E623' TO WS-REJECT-CODE
               WHEN TBLT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E360' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CASH IN FEE ON THE OVERSHORT YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3230-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN TBLT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO TBLM-CASH-IN-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLM-SUSPEND-FLAG = 'Y'
                   COMPUTE TBLM-CASH-IN-AMT ROUNDED = TBLM-OVERSHORT-YTD
                       * TBLM-DISCOUNT / 200
               WHEN TBLM-DRAWER-CD = 'D'
                   MOVE 15.00 TO TBLM-CASH-IN-AMT
               WHEN TBLM-DRAWER-CD = 'F'
                   MOVE 5.00 TO TBLM-CASH-IN-AMT
               WHEN OTHER
                   COMPUTE TBLM-CASH-IN-AMT ROUNDED = TBLM-OVERSHORT-YTD
                       * TBLM-DISCOUNT / 100
           END-EVALUATE
           IF TBLM-CASH-IN-AMT > 0 AND TBLM-CASH-IN-AMT < 10.00
               MOVE 10.00 TO TBLM-CASH-IN-AMT
           END-IF
           IF TBLM-CASH-IN-AMT > 35.00
               MOVE 35.00 TO TBLM-CASH-IN-AMT
           END-IF
           ADD TBLM-CASH-IN-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3240-ACCRUE-DISCOUNT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * TBLM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO TBLM-CASH-IN-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CASH OUT AMT IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3250-LIMIT-CASH-OUT.
           IF TBLT-CASH-OUT-AMT < 25.00
               MOVE 25.00 TO TBLT-CASH-OUT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLT-CASH-OUT-AMT > 50000.00
               MOVE 50000.00 TO TBLT-CASH-OUT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E429 REF NO REQUIRED
      * E958 BATCH AMT NEGATIVE
      * E199 BATCH AMT OVER 250000
      * E822 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3260-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLM-REF-NO = SPACES
                   MOVE 'E429' TO WS-REJECT-CODE
               WHEN TBLT-BATCH-AMT < 0
                   MOVE 'E958' TO WS-REJECT-CODE
               WHEN TBLT-BATCH-AMT > 250000.00
                   MOVE 'E199' TO WS-REJECT-CODE
               WHEN TBLM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E822' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE OVERSHORT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3270-PRORATE-OVERSHORT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLM-OVERSHORT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * BATCH AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3280-FLAG-BATCH.
           IF TBLT-BATCH-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TBLT-BATCH-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * OVERSHORT AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3290-FLAG-OVERSHORT.
           IF TBLM-OVERSHORT-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TBLM-OVERSHORT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3300-COUNT-RETRY.
           IF TBLM-RETRY-CNT < 9999999
               ADD 1 TO TBLM-RETRY-CNT
           END-IF
           IF TBLM-RETRY-CNT NOT < 6
               MOVE 'Y' TO TBLM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT BATCH AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3310-SPLIT-BATCH.
           COMPUTE WS-WORK-AMT = TBLM-BATCH-AMT / 12
           COMPUTE WS-WORK-AMT-2 = TBLM-BATCH-AMT - (WS-WORK-AMT * 12)
           COMPUTE TBLM-CASH-IN-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3320-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF TBLM-LAST-PAY-DATE > 0 AND TBLM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TBLM-BATCH-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TBLM-BATCH-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD TBLM-BATCH-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TBLM-BATCH-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLM-BATCH-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3330-COUNT-RETRY.
           IF TBLM-RETRY-CNT < 9999999
               ADD 1 TO TBLM-RETRY-CNT
           END-IF
           IF TBLM-RETRY-CNT NOT < 10
               MOVE 'Y' TO TBLM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CASH OUT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3340-PRORATE-CASH-OUT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLT-CASH-OUT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT VAULT SELL AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3350-SPLIT-VAULT-SELL.
           COMPUTE WS-WORK-AMT = TBLT-VAULT-SELL-AMT / 12
           COMPUTE WS-WORK-AMT-2 = TBLT-VAULT-SELL-AMT - (WS-WORK-AMT *
               12)
           COMPUTE TBLT-OVERSHORT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE OVERSHORT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO BATCH YTD.
      *----------------------------------------------------------------*
       3360-PRORATE-BATCH.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLM-BATCH-YTD ROUNDED =
                   TBLT-OVERSHORT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TBLM-BATCH-YTD
           END-IF
           ADD TBLM-BATCH-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3370-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF TBLM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO TBLM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE OVERSHORT FEE ON THE BATCH AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3380-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN TBLM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO TBLM-OVERSHORT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLM-HOLD-FLAG = 'Y'
                   COMPUTE TBLM-OVERSHORT-AMT ROUNDED = TBLT-BATCH-AMT *
                       TBLM-FEE-PCT / 200
               WHEN TBLM-PROOF-TYPE-CD = 'JG'
                   MOVE 20.00 TO TBLM-OVERSHORT-AMT
               WHEN TBLM-PROOF-TYPE-CD = 'RX'
                   MOVE 3.00 TO TBLM-OVERSHORT-AMT
               WHEN OTHER
                   COMPUTE TBLM-OVERSHORT-AMT ROUNDED = TBLT-BATCH-AMT *
                       TBLM-FEE-PCT / 100
           END-EVALUATE
           IF TBLM-OVERSHORT-AMT > 0 AND TBLM-OVERSHORT-AMT < 10.00
               MOVE 10.00 TO TBLM-OVERSHORT-AMT
           END-IF
           IF TBLM-OVERSHORT-AMT > 25.00
               MOVE 25.00 TO TBLM-OVERSHORT-AMT
           END-IF
           ADD TBLM-OVERSHORT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE VAULT SE SCHED TIER FOR THE CASH IN YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3390-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-VAULT-SE-SCHED-RATE(1) TO TBLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF TBLM-CASH-IN-YTD NOT < WS-VAULT-SE-SCHED-LOW(WS-TX)
                   MOVE WS-VAULT-SE-SCHED-RATE(WS-TX) TO TBLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TBLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TBLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3400-COUNT-RETRY.
           IF TBLM-RETRY-CNT < 9999999
               ADD 1 TO TBLM-RETRY-CNT
           END-IF
           IF TBLM-RETRY-CNT NOT < 6
               MOVE 'Y' TO TBLT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE VAULT BU GRID TIER FOR THE VAULT SELL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3410-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-VAULT-BU-GRID-RATE(1) TO TBLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF TBLT-VAULT-SELL-AMT NOT < WS-VAULT-BU-GRID-LOW(WS-TX)
                   MOVE WS-VAULT-BU-GRID-RATE(WS-TX) TO TBLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TBLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TBLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3420-DESCRIBE-CYCLE.
           EVALUATE TBLM-CYCLE-CD
               WHEN 'KX'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'TA'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'TG'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'WG'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3430-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF TBLM-EFFECTIVE-DATE > 0 AND TBLM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3440-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF TBLM-OPEN-DATE > 0 AND TBLM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TBLM-VAULT-BUY-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TBLM-VAULT-BUY-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TBLM-VAULT-BUY-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TBLM-VAULT-BUY-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLM-VAULT-BUY-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE CASH IN AMT BY PROOF TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3450-ACCUM-PROOF-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TBLM-PROOF-TYPE-CD
                   ADD TBLM-CASH-IN-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TBLM-PROOF-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TBLM-CASH-IN-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE VAULT SE SCHED TIER FOR THE OVERSHORT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3460-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-VAULT-SE-SCHED-RATE(1) TO TBLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF TBLT-OVERSHORT-AMT NOT < WS-VAULT-SE-SCHED-LOW(WS-TX)
                   MOVE WS-VAULT-SE-SCHED-RATE(WS-TX) TO TBLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TBLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TBLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CASH OUT YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3470-FLAG-CASH-OUT.
           IF TBLM-CASH-OUT-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TBLM-CASH-OUT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * KX -> TA WHEN NOTICE DATE IS PAST
      * TA -> TG WHEN NOTICE DATE IS PAST
      * TG -> WG WHEN NOTICE DATE IS PAST
      * WG -> KX WHEN NOTICE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3480-SET-CYCLE.
           EVALUATE TBLM-CYCLE-CD
               WHEN 'KX'
                   IF TBLM-NOTICE-DATE > 0 AND TBLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TA' TO TBLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TA'
                   IF TBLM-NOTICE-DATE > 0 AND TBLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TG' TO TBLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TG'
                   IF TBLM-NOTICE-DATE > 0 AND TBLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WG' TO TBLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WG'
                   IF TBLM-NOTICE-DATE > 0 AND TBLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KX' TO TBLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE CASH OUT FEE ON THE VAULT BUY AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3490-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN TBLM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO TBLM-CASH-OUT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLT-REVERSAL-FLAG = 'Y'
                   COMPUTE TBLM-CASH-OUT-AMT ROUNDED =
                       TBLM-VAULT-BUY-AMT * TBLM-DISCOUNT / 200
               WHEN TBLM-CHANNEL-CD = 'D'
                   MOVE 15.00 TO TBLM-CASH-OUT-AMT
               WHEN TBLM-CHANNEL-CD = 'F'
                   MOVE 10.00 TO TBLM-CASH-OUT-AMT
               WHEN OTHER
                   COMPUTE TBLM-CASH-OUT-AMT ROUNDED =
                       TBLM-VAULT-BUY-AMT * TBLM-DISCOUNT / 100
           END-EVALUATE
           IF TBLM-CASH-OUT-AMT > 0 AND TBLM-CASH-OUT-AMT < 2.00
               MOVE 2.00 TO TBLM-CASH-OUT-AMT
           END-IF
           IF TBLM-CASH-OUT-AMT > 25.00
               MOVE 25.00 TO TBLM-CASH-OUT-AMT
           END-IF
           ADD TBLM-CASH-OUT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 120 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3500-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF TBLM-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO TBLM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * OVERSHORT YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3510-FLAG-OVERSHORT.
           IF TBLM-OVERSHORT-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TBLM-OVERSHORT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-45, 46-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3520-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF TBLM-OPEN-DATE > 0 AND TBLM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TBLM-CASH-IN-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD TBLM-CASH-IN-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD TBLM-CASH-IN-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TBLM-CASH-IN-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLM-CASH-IN-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * VAULT SELL AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3530-FLAG-VAULT-SELL.
           IF TBLM-VAULT-SELL-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TBLM-VAULT-SELL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3540-COUNT-EXCEPT.
           IF TBLM-EXCEPT-CNT < 9999999
               ADD 1 TO TBLM-EXCEPT-CNT
           END-IF
           IF TBLM-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO TBLM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CASH IN AMT IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3550-LIMIT-CASH-IN.
           IF TBLM-CASH-IN-AMT < 100.00
               MOVE 100.00 TO TBLM-CASH-IN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLM-CASH-IN-AMT > 2500.00
               MOVE 2500.00 TO TBLM-CASH-IN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-45, 46-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3560-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF TBLM-NEXT-REVIEW-DATE > 0 AND TBLM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TBLT-VAULT-SELL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD TBLT-VAULT-SELL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD TBLT-VAULT-SELL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TBLT-VAULT-SELL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLT-VAULT-SELL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * GS -> GV WHEN LAST ACT DATE IS PAST
      * GV -> PG WHEN LAST ACT DATE IS PAST
      * PG -> SX WHEN LAST ACT DATE IS PAST
      * SX -> XN WHEN LAST ACT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3570-SET-REASON.
           EVALUATE TBLM-REASON-CD
               WHEN 'GS'
                   IF TBLM-LAST-ACT-DATE > 0 AND TBLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GV' TO TBLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GV'
                   IF TBLM-LAST-ACT-DATE > 0 AND TBLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PG' TO TBLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PG'
                   IF TBLM-LAST-ACT-DATE > 0 AND TBLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SX' TO TBLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SX'
                   IF TBLM-LAST-ACT-DATE > 0 AND TBLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XN' TO TBLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * VAULT SELL AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3580-FLAG-VAULT-SELL.
           IF TBLT-VAULT-SELL-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TBLT-VAULT-SELL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E927 CONTACT REQUIRED
      * E957 CASH IN YTD NEGATIVE
      * E192 CASH IN YTD OVER 10000
      * E910 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3590-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLM-CONTACT = SPACES
                   MOVE 'E927' TO WS-REJECT-CODE
               WHEN TBLM-CASH-IN-YTD < 0
                   MOVE 'E957' TO WS-REJECT-CODE
               WHEN TBLM-CASH-IN-YTD > 10000.00
                   MOVE 'E192' TO WS-REJECT-CODE
               WHEN TBLT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E910' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CASH OUT BAND TIER FOR THE OVERSHORT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3600-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-OUT-BAND-RATE(1) TO TBLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF TBLM-OVERSHORT-AMT NOT < WS-CASH-OUT-BAND-LOW(WS-TX)
                   MOVE WS-CASH-OUT-BAND-RATE(WS-TX) TO TBLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TBLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TBLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3610-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3620-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF TBLM-EFFECTIVE-DATE > 0 AND TBLM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TBLT-CASH-OUT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TBLT-CASH-OUT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TBLT-CASH-OUT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TBLT-CASH-OUT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLT-CASH-OUT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE VAULT SELL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO VAULT SELL AMT.
      *----------------------------------------------------------------*
       3630-PRORATE-VAULT-SELL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLT-VAULT-SELL-AMT ROUNDED =
                   TBLM-VAULT-SELL-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TBLT-VAULT-SELL-AMT
           END-IF
           ADD TBLT-VAULT-SELL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * BI -> BO WHEN LAST STMT DATE IS PAST
      * BO -> VB WHEN LAST STMT DATE IS PAST
      * VB -> VS WHEN LAST STMT DATE IS PAST
      * VS -> OS WHEN LAST STMT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3640-SET-TRAN.
           EVALUATE TBLT-TRAN-CD
               WHEN 'BI'
                   IF TBLM-LAST-STMT-DATE > 0 AND TBLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BO' TO TBLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BO'
                   IF TBLM-LAST-STMT-DATE > 0 AND TBLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VB' TO TBLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VB'
                   IF TBLM-LAST-STMT-DATE > 0 AND TBLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VS' TO TBLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VS'
                   IF TBLM-LAST-STMT-DATE > 0 AND TBLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'OS' TO TBLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3650-COUNT-EXCEPT.
           IF TBLM-EXCEPT-CNT < 9999999
               ADD 1 TO TBLM-EXCEPT-CNT
           END-IF
           IF TBLM-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO TBLM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT VAULT BUY AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3660-SPLIT-VAULT-BUY.
           COMPUTE WS-WORK-AMT = TBLM-VAULT-BUY-AMT / 4
           COMPUTE WS-WORK-AMT-2 = TBLM-VAULT-BUY-AMT - (WS-WORK-AMT *
               4)
           COMPUTE TBLM-VAULT-SELL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CASH OUT YTD IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3670-LIMIT-CASH-OUT.
           IF TBLM-CASH-OUT-YTD < 10.00
               MOVE 10.00 TO TBLM-CASH-OUT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLM-CASH-OUT-YTD > 10000.00
               MOVE 10000.00 TO TBLM-CASH-OUT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE VAULT SELL AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3680-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN TBLM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLM-OVERRIDE-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = TBLT-VAULT-SELL-AMT *
                       TBLM-RATE / 200
               WHEN TBLM-CYCLE-CD = 'KX'
                   MOVE 15.00 TO WS-WORK-AMT-2
               WHEN TBLM-CYCLE-CD = 'TA'
                   MOVE 3.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = TBLT-VAULT-SELL-AMT *
                       TBLM-RATE / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 2.00
               MOVE 2.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 25.00
               MOVE 25.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3690-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF TBLM-LAST-ACT-DATE > 0 AND TBLM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TBLM-VAULT-SELL-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TBLM-VAULT-SELL-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TBLM-VAULT-SELL-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TBLM-VAULT-SELL-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLM-VAULT-SELL-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE CASH IN YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3700-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TBLT-CHANNEL-CD
                   ADD TBLM-CASH-IN-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TBLT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TBLM-CASH-IN-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CASH OUT AMT BY DRAWER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3710-ACCUM-DRAWER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TBLM-DRAWER-CD
                   ADD TBLM-CASH-OUT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TBLM-DRAWER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TBLM-CASH-OUT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CASH IN AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3720-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TBLM-CHANNEL-CD
                   ADD TBLM-CASH-IN-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TBLM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TBLM-CASH-IN-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * CASH OUT YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3730-FLAG-CASH-OUT.
           IF TBLM-CASH-OUT-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TBLM-CASH-OUT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CASH OUT AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3740-ACCRUE-CASH-OUT.
           IF TBLM-CASH-OUT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLM-CASH-OUT-AMT * TBLM-RATE / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE VAULT BU GRID TIER FOR THE OVERSHORT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3750-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-VAULT-BU-GRID-RATE(1) TO TBLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF TBLM-OVERSHORT-YTD NOT < WS-VAULT-BU-GRID-LOW(WS-TX)
                   MOVE WS-VAULT-BU-GRID-RATE(WS-TX) TO TBLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TBLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TBLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3760-DESCRIBE-REASON.
           EVALUATE TBLM-REASON-CD
               WHEN 'GS'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'GV'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'PG'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'SX'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'XN'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'YZ'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE BATCH YTD BY DRAWER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3770-ACCUM-DRAWER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TBLM-DRAWER-CD
                   ADD TBLM-BATCH-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TBLM-DRAWER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TBLM-BATCH-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3780-DESCRIBE-CHANNEL.
           EVALUATE TBLT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE VAULT BU GRID TIER FOR THE CASH IN YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3790-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-VAULT-BU-GRID-RATE(1) TO TBLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF TBLM-CASH-IN-YTD NOT < WS-VAULT-BU-GRID-LOW(WS-TX)
                   MOVE WS-VAULT-BU-GRID-RATE(WS-TX) TO TBLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TBLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TBLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * VAULT SELL AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3800-FLAG-VAULT-SELL.
           IF TBLT-VAULT-SELL-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TBLT-VAULT-SELL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * VAULT SELL YTD IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3810-LIMIT-VAULT-SELL.
           IF TBLM-VAULT-SELL-YTD < 100.00
               MOVE 100.00 TO TBLM-VAULT-SELL-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLM-VAULT-SELL-YTD > 1000.00
               MOVE 1000.00 TO TBLM-VAULT-SELL-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * VAULT BUY AMT IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3820-LIMIT-VAULT-BUY.
           IF TBLM-VAULT-BUY-AMT < 10.00
               MOVE 10.00 TO TBLM-VAULT-BUY-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLM-VAULT-BUY-AMT > 2500.00
               MOVE 2500.00 TO TBLM-VAULT-BUY-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * OVERSHORT AMT IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3830-LIMIT-OVERSHORT.
           IF TBLM-OVERSHORT-AMT < 10.00
               MOVE 10.00 TO TBLM-OVERSHORT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLM-OVERSHORT-AMT > 10000.00
               MOVE 10000.00 TO TBLM-OVERSHORT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * BATCH YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3840-FLAG-BATCH.
           IF TBLM-BATCH-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TBLM-BATCH-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CASH OUT BAND TIER FOR THE VAULT BUY YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3850-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-OUT-BAND-RATE(1) TO TBLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF TBLM-VAULT-BUY-YTD NOT < WS-CASH-OUT-BAND-LOW(WS-TX)
                   MOVE WS-CASH-OUT-BAND-RATE(WS-TX) TO TBLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TBLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TBLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON OVERSHORT YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3860-ACCRUE-OVERSHORT.
           IF TBLM-OVERSHORT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLM-OVERSHORT-YTD * TBLM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO TBLM-CASH-IN-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT OVERSHORT AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3870-SPLIT-OVERSHORT.
           COMPUTE WS-WORK-AMT = TBLT-OVERSHORT-AMT / 12
           COMPUTE WS-WORK-AMT-2 = TBLT-OVERSHORT-AMT - (WS-WORK-AMT *
               12)
           COMPUTE TBLT-CASH-OUT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE OVERSHORT FEE ON THE OVERSHORT AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3880-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN TBLM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO TBLT-OVERSHORT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLM-SUSPEND-FLAG = 'Y'
                   COMPUTE TBLT-OVERSHORT-AMT ROUNDED =
                       TBLM-OVERSHORT-AMT * TBLM-DISCOUNT / 200
               WHEN TBLT-TRAN-CD = 'BI'
                   MOVE 15.00 TO TBLT-OVERSHORT-AMT
               WHEN TBLT-TRAN-CD = 'BO'
                   MOVE 10.00 TO TBLT-OVERSHORT-AMT
               WHEN OTHER
                   COMPUTE TBLT-OVERSHORT-AMT ROUNDED =
                       TBLM-OVERSHORT-AMT * TBLM-DISCOUNT / 100
           END-EVALUATE
           IF TBLT-OVERSHORT-AMT > 0 AND TBLT-OVERSHORT-AMT < 2.00
               MOVE 2.00 TO TBLT-OVERSHORT-AMT
           END-IF
           IF TBLT-OVERSHORT-AMT > 50.00
               MOVE 50.00 TO TBLT-OVERSHORT-AMT
           END-IF
           ADD TBLT-OVERSHORT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3890-DESCRIBE-CHANNEL.
           EVALUATE TBLM-CHANNEL-CD
               WHEN 'D'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'F'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'M'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'N'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'R'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'T'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CASH OUT YTD IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3900-LIMIT-CASH-OUT.
           IF TBLM-CASH-OUT-YTD < 100.00
               MOVE 100.00 TO TBLM-CASH-OUT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLM-CASH-OUT-YTD > 10000.00
               MOVE 10000.00 TO TBLM-CASH-OUT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-120, 121-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3910-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF TBLM-LAST-PAY-DATE > 0 AND TBLM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TBLM-VAULT-SELL-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 120
                   ADD TBLM-VAULT-SELL-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD TBLM-VAULT-SELL-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TBLM-VAULT-SELL-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLM-VAULT-SELL-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * VAULT SELL AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3920-FLAG-VAULT-SELL.
           IF TBLT-VAULT-SELL-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TBLT-VAULT-SELL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VAULT SELL YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3930-ACCRUE-VAULT-SELL.
           IF TBLM-VAULT-SELL-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLM-VAULT-SELL-YTD * TBLM-RATE / 100 / 360
               ADD WS-WORK-AMT TO TBLM-OVERSHORT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 180 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3940-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF TBLT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO TBLM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 45 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3950-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF TBLT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO TBLM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3960-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF TBLM-EFFECTIVE-DATE > 0 AND TBLM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TBLM-VAULT-SELL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TBLM-VAULT-SELL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TBLM-VAULT-SELL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TBLM-VAULT-SELL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLM-VAULT-SELL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE DRAWER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3970-DESCRIBE-DRAWER.
           EVALUATE TBLM-DRAWER-CD
               WHEN 'D'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'F'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'R'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E445 DESC REQUIRED
      * E265 CASH IN YTD NEGATIVE
      * E439 CASH IN YTD OVER 250000
      * E040 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3980-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLM-DESC = SPACES
                   MOVE 'E445' TO WS-REJECT-CODE
               WHEN TBLM-CASH-IN-YTD < 0
                   MOVE 'E265' TO WS-REJECT-CODE
               WHEN TBLM-CASH-IN-YTD > 250000.00
                   MOVE 'E439' TO WS-REJECT-CODE
               WHEN TBLM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E040' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CASH OUT YTD IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3990-LIMIT-CASH-OUT.
           IF TBLM-CASH-OUT-YTD < 0.00
               MOVE 0.00 TO TBLM-CASH-OUT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLM-CASH-OUT-YTD > 10000.00
               MOVE 10000.00 TO TBLM-CASH-OUT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE VAULT SE SCHED TIER FOR THE VAULT SELL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4000-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-VAULT-SE-SCHED-RATE(1) TO TBLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF TBLM-VAULT-SELL-AMT NOT < WS-VAULT-SE-SCHED-LOW(WS-TX)
                   MOVE WS-VAULT-SE-SCHED-RATE(WS-TX) TO TBLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TBLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TBLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE VAULT BUY YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO BATCH AMT.
      *----------------------------------------------------------------*
       4010-PRORATE-BATCH.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLM-BATCH-AMT ROUNDED =
                   TBLM-VAULT-BUY-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TBLM-BATCH-AMT
           END-IF
           ADD TBLM-BATCH-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * CASH IN AMT IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4020-LIMIT-CASH-IN.
           IF TBLM-CASH-IN-AMT < 25.00
               MOVE 25.00 TO TBLM-CASH-IN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLM-CASH-IN-AMT > 1000.00
               MOVE 1000.00 TO TBLM-CASH-IN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * VAULT BUY AMT IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4030-LIMIT-VAULT-BUY.
           IF TBLM-VAULT-BUY-AMT < 100.00
               MOVE 100.00 TO TBLM-VAULT-BUY-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLM-VAULT-BUY-AMT > 50000.00
               MOVE 50000.00 TO TBLM-VAULT-BUY-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 60 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4040-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF TBLM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO TBLM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE CASH OUT YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4050-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN TBLT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLM-OVERRIDE-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = TBLM-CASH-OUT-YTD *
                       TBLM-DISCOUNT / 200
               WHEN TBLM-REASON-CD = 'GS'
                   MOVE 12.00 TO WS-WORK-AMT
               WHEN TBLM-REASON-CD = 'GV'
                   MOVE 3.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = TBLM-CASH-OUT-YTD *
                       TBLM-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 5.00
               MOVE 5.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 75.00
               MOVE 75.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE OVERSHORT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO OVERSHORT YTD.
      *----------------------------------------------------------------*
       4060-PRORATE-OVERSHORT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLM-OVERSHORT-YTD ROUNDED =
                   TBLM-OVERSHORT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TBLM-OVERSHORT-YTD
           END-IF
           ADD TBLM-OVERSHORT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4070-DESCRIBE-SOURCE.
           EVALUATE TBLM-SOURCE-CD
               WHEN 'AF'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'PT'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'SZ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'VK'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'VW'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4080-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF TBLM-NEXT-REVIEW-DATE > 0 AND TBLM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TBLM-OVERSHORT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD TBLM-OVERSHORT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD TBLM-OVERSHORT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TBLM-OVERSHORT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLM-OVERSHORT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E883 NAME REQUIRED
      * E372 VAULT BUY AMT NEGATIVE
      * E767 VAULT BUY AMT OVER 1000000
      * E245 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       4090-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLM-NAME = SPACES
                   MOVE 'E883' TO WS-REJECT-CODE
               WHEN TBLM-VAULT-BUY-AMT < 0
                   MOVE 'E372' TO WS-REJECT-CODE
               WHEN TBLM-VAULT-BUY-AMT > 1000000.00
                   MOVE 'E767' TO WS-REJECT-CODE
               WHEN TBLM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E245' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 120 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4100-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF TBLT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO TBLT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON OVERSHORT AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4110-ACCRUE-OVERSHORT.
           IF TBLM-OVERSHORT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLM-OVERSHORT-AMT * TBLM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO TBLM-OVERSHORT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * OVERSHORT YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4120-FLAG-OVERSHORT.
           IF TBLM-OVERSHORT-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TBLM-OVERSHORT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4130-DESCRIBE-CHANNEL.
           EVALUATE TBLT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE VAULT SELL YTD BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4140-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TBLM-CYCLE-CD
                   ADD TBLM-VAULT-SELL-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TBLM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TBLM-VAULT-SELL-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CASH OUT AMT.
      *----------------------------------------------------------------*
       4150-PRORATE-CASH-OUT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLT-CASH-OUT-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TBLT-CASH-OUT-AMT
           END-IF
           ADD TBLT-CASH-OUT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 10 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4160-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF TBLM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO TBLM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON OVERSHORT AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4170-ACCRUE-OVERSHORT.
           IF TBLT-OVERSHORT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLT-OVERSHORT-AMT * TBLM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO TBLM-OVERSHORT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       4180-COUNT-ITEM.
           IF TBLM-ITEM-CNT < 9999999
               ADD 1 TO TBLM-ITEM-CNT
           END-IF
           IF TBLM-ITEM-CNT NOT < 10
               MOVE 'Y' TO TBLM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4190-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF TBLM-LAST-MAINT-DATE > 0 AND TBLM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TBLM-OVERSHORT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TBLM-OVERSHORT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD TBLM-OVERSHORT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TBLM-OVERSHORT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLM-OVERSHORT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE BATCH AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4200-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TBLM-SOURCE-CD
                   ADD TBLT-BATCH-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TBLM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TBLT-BATCH-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE VAULT SELL FEE ON THE CASH OUT YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4210-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN TBLM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO TBLT-VAULT-SELL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLM-OVERRIDE-FLAG = 'Y'
                   COMPUTE TBLT-VAULT-SELL-AMT ROUNDED =
                       TBLM-CASH-OUT-YTD * TBLM-RATE / 200
               WHEN TBLT-TRAN-CD = 'BI'
                   MOVE 12.00 TO TBLT-VAULT-SELL-AMT
               WHEN TBLT-TRAN-CD = 'BO'
                   MOVE 12.00 TO TBLT-VAULT-SELL-AMT
               WHEN OTHER
                   COMPUTE TBLT-VAULT-SELL-AMT ROUNDED =
                       TBLM-CASH-OUT-YTD * TBLM-RATE / 100
           END-EVALUATE
           IF TBLT-VAULT-SELL-AMT > 0 AND TBLT-VAULT-SELL-AMT < 5.00
               MOVE 5.00 TO TBLT-VAULT-SELL-AMT
           END-IF
           IF TBLT-VAULT-SELL-AMT > 25.00
               MOVE 25.00 TO TBLT-VAULT-SELL-AMT
           END-IF
           ADD TBLT-VAULT-SELL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       4220-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF TBLM-OPEN-DATE > 0 AND TBLM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TBLT-VAULT-SELL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TBLT-VAULT-SELL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TBLT-VAULT-SELL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD TBLT-VAULT-SELL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLT-VAULT-SELL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       4230-COUNT-RETRY.
           IF TBLM-RETRY-CNT < 9999999
               ADD 1 TO TBLM-RETRY-CNT
           END-IF
           IF TBLM-RETRY-CNT NOT < 10
               MOVE 'Y' TO TBLM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE TBL FIELDS TO TLWAIV AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7020-CALL-TLWAIV.
           INITIALIZE TLWAIV-PARMS
           MOVE TBLM-VAULT-SELL-AMT
             TO TLWAIV-VAULT-SELL-AMT
           MOVE TBLM-VAULT-BUY-YTD
             TO TLWAIV-VAULT-BUY-YTD
           MOVE TBLM-BATCH-YTD
             TO TLWAIV-BATCH-YTD
           MOVE TBLM-CASH-IN-AMT
             TO TLWAIV-CASH-IN-AMT
           MOVE TBLM-CASH-IN-YTD
             TO TLWAIV-CASH-IN-YTD
           MOVE TBLM-CASH-OUT-YTD
             TO TLWAIV-CASH-OUT-YTD
           MOVE TBLM-EFFECTIVE-DATE
             TO TLWAIV-EFFECTIVE-DATE
           MOVE TBLM-LAST-STMT-DATE
             TO TLWAIV-LAST-STMT-DATE
           MOVE TBLM-LAST-PAY-DATE
             TO TLWAIV-LAST-PAY-DATE
           MOVE TBLM-OPEN-DATE
             TO TLWAIV-OPEN-DATE
           MOVE TBLM-SOURCE-CD
             TO TLWAIV-SOURCE-CD
           MOVE TBLM-DRAWER-CD
             TO TLWAIV-DRAWER-CD
           MOVE TBLM-CHANNEL-CD
             TO TLWAIV-CHANNEL-CD
           MOVE TBLM-REASON-CD
             TO TLWAIV-REASON-CD
           MOVE TBLM-STOP-FLAG
             TO TLWAIV-STOP-FLAG
           MOVE TBLM-SUSPEND-FLAG
             TO TLWAIV-SUSPEND-FLAG
           MOVE TBLM-EXEMPT-FLAG
             TO TLWAIV-EXEMPT-FLAG
           MOVE TBLM-ITEM-CNT
             TO TLWAIV-ITEM-CNT
           MOVE TBLM-EXCEPT-CNT
             TO TLWAIV-EXCEPT-CNT
           MOVE TBLM-FEE-PCT
             TO TLWAIV-FEE-PCT
           MOVE TBLM-RATE
             TO TLWAIV-RATE
           MOVE TBLM-DESC
             TO TLWAIV-DESC
           MOVE TBLM-REF-NO
             TO TLWAIV-REF-NO
           CALL 'TLWAIV' USING TLWAIV-PARMS
           EVALUATE TLWAIV-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE TLWAIV-VAULT-SELL-AMT
                     TO TBLM-VAULT-SELL-AMT
                   MOVE TLWAIV-CASH-IN-YTD
                     TO TBLM-CASH-IN-YTD
                   MOVE TLWAIV-CASH-OUT-YTD
                     TO TBLM-CASH-OUT-YTD
                   MOVE TLWAIV-SUSPEND-FLAG
                     TO TBLM-SUSPEND-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RAIV' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-TLWAIV' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE TBLT-KEY TO RD-KEY
           MOVE TBLT-TRAN-CD TO RD-CODE
           MOVE TBLT-CASH-OUT-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE TBLT-KEY TO RD-KEY
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
           DISPLAY 'TLLSTMT READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'TLLSTMT' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
