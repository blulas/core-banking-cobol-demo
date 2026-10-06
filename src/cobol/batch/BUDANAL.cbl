      *================================================================*
      * PROGRAM  : BUDANAL                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : BUMP-UP CERTIFICATES - ANALYSIS. READS THE BCD      *
      *            TRANSACTIONS, UPDATES THE BCD MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. BUMP-UP AND STEP-RATE           *
      *            CERTIFICATE RATE ELECTION AND ACCRUAL               *
      * INPUT    : BCDMST (KSDS), BCDTRN, BANKCTL                      *
      * OUTPUT   : BCDOUT, BUDANRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, BUSTAT                  *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1992-10-27 SKN  ORIGINAL                                       *
      * 1993-05-19 JLM  BUMP LIMIT REVISED PRB2333                     *
      * 1994-12-21 MKL  PRINCIPAL EXCEPTIONS TO OPS QUEUE PRB3448      *
      * 1995-09-05 SKN  AUDIT FINDING - PRINCIPAL CONTROL TOTALS INC528*
      * 1996-11-26 LMB  PENALTY RERUN PROTECTION CHG3127               *
      * 1999-05-11 SKN  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-02-20 RJT  PRINCIPAL RERUN PROTECTION CHG5552             *
      * 2001-10-10 JLM  ADDON EXCEPTIONS TO OPS QUEUE INC6609          *
      * 2003-09-11 DPS  PENALTY RERUN PROTECTION CHG5409               *
      * 2003-10-08 DPS  BUMP RERUN PROTECTION INC0223                  *
      * 2005-08-07 EWS  PERFORMANCE - ADDON READ AHEAD INC0988         *
      * 2005-02-02 TWH  PENALTY ROUNDING - HALF UP INC1769             *
      * 2006-07-22 RJT  AUDIT FINDING - PRINCIPAL CONTROL TOTALS INC080*
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BUDANAL.
       AUTHOR. D P SHAW.

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
           SELECT REPORT-FILE ASSIGN TO "BUDANRPT"
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
                                           VALUE 20.000000.
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
      * WS-ADDON-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-ADDON-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000093000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000672000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000350000000581000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001350000000657000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006350000000436000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006450000000401000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006500000000105000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007500000000516500'.
       01  WS-ADDON-TIER-TBL REDEFINES WS-ADDON-TIER-VALUES.
           05  WS-ADDON-TIER-ROW OCCURS 8 TIMES.
               10  WS-ADDON-TIER-LOW       PIC 9(09)V99.
               10  WS-ADDON-TIER-RATE      PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-PRINCIPA-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PRINCIPA-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000413000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000423000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005500000000675000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010500000000700000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010750000000357000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011000000000668500'.
       01  WS-PRINCIPA-GRID-TBL REDEFINES WS-PRINCIPA-GRID-VALUES.
           05  WS-PRINCIPA-GRID-ROW OCCURS 6 TIMES.
               10  WS-PRINCIPA-GRID-LOW    PIC 9(09)V99.
               10  WS-PRINCIPA-GRID-RATE   PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  BUDANAL  BUMP-UP CERTIFICATES'.
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
               PERFORM 3000-ASSESS-EXEMPT
               PERFORM 3010-EDIT-CUST-ID
               PERFORM 3020-COUNT-RETRY
               PERFORM 3030-AGE-LAST-MAINT
               PERFORM 3040-AGE-NEXT-DUE
               PERFORM 3050-COUNT-EXCEPT
               PERFORM 3060-COUNT-RETRY
               PERFORM 3070-FLAG-BUMP
               PERFORM 3080-SPLIT-PRINCIPAL
               PERFORM 3090-AGE-CLOSE
               PERFORM 3100-LIMIT-ADDON
               PERFORM 3110-SPLIT-INTEREST
               PERFORM 3120-SELECT-MARGIN
               PERFORM 3130-CHECK-POST
               PERFORM 3140-EDIT-CONTACT
               PERFORM 3150-CHECK-EFF
               PERFORM 3160-ACCUM-CYCLE
               PERFORM 3170-ACCUM-SOURCE
               PERFORM 3180-FLAG-PENALTY
               PERFORM 3190-SPLIT-INTEREST
               PERFORM 3200-SELECT-RATE
               PERFORM 3210-SET-REGION
               PERFORM 3220-AGE-EFFECTIVE
               PERFORM 3230-ACCRUE-INTEREST
               PERFORM 3240-ASSESS-REVERSAL
               PERFORM 3250-FLAG-ITEM
               PERFORM 3260-ACCUM-STEP-PLAN
               PERFORM 3270-DESCRIBE-ELECTION
               PERFORM 3280-LIMIT-PENALTY
               PERFORM 3290-ACCRUE-PENALTY
               PERFORM 3300-SPLIT-INTEREST
               PERFORM 3310-SELECT-PENALTY-PCT
               PERFORM 3320-SET-REGION
               PERFORM 3330-SELECT-MARGIN
               PERFORM 3340-ASSESS-STOP
               PERFORM 3350-AGE-LAST-PAY
               PERFORM 3360-SELECT-PENALTY-PCT
               PERFORM 3370-DESCRIBE-REGION
               PERFORM 3380-SET-REGION
               PERFORM 3390-COUNT-ITEM
               PERFORM 3400-ASSESS-SUSPEND
               PERFORM 3410-ASSESS-STOP
               PERFORM 3420-AGE-NEXT-REVIEW
               PERFORM 3430-ACCUM-CYCLE
               PERFORM 3440-SELECT-RATE
               PERFORM 3450-SPLIT-ADDON
               PERFORM 3460-AGE-LAST-STMT
               PERFORM 3470-DESCRIBE-CYCLE
               PERFORM 3480-ACCRUE-MARGIN
               PERFORM 3490-ASSESS-REVERSAL
               PERFORM 3500-COUNT-CYCLE
               PERFORM 3510-AGE-EFFECTIVE
               PERFORM 3520-FLAG-INTEREST
               PERFORM 3530-DESCRIBE-STEP-PLAN
               PERFORM 3540-DESCRIBE-ELECTION
               PERFORM 3550-DESCRIBE-CHANNEL
               PERFORM 3560-CHECK-LAST-PAY
               PERFORM 3570-DESCRIBE-REGION
               PERFORM 3580-ASSESS-REVERSAL
               PERFORM 3590-ASSESS-STOP
               PERFORM 3600-ACCRUE-INTEREST
               PERFORM 3610-CHECK-NEXT-DUE
               PERFORM 3620-CHECK-LAST-STMT
               PERFORM 3630-SET-TRAN
               PERFORM 3640-ACCUM-CHANNEL
               PERFORM 3650-COUNT-SEQ
               PERFORM 3660-AGE-EFFECTIVE
               PERFORM 3670-SET-CYCLE
               PERFORM 3680-FLAG-PRINCIPAL
               PERFORM 3690-SET-TRAN
               PERFORM 3700-PRORATE-INTEREST
               PERFORM 3710-EDIT-NAME
               PERFORM 3720-ASSESS-EXEMPT
               PERFORM 3730-SET-SOURCE
               PERFORM 3740-SET-TRAN
               PERFORM 3750-CHECK-EFFECTIVE
               PERFORM 3760-AGE-CLOSE
               PERFORM 3770-ACCRUE-PENALTY-PCT
               PERFORM 3780-EDIT-OPERATOR
               PERFORM 3790-PRORATE-PENALTY
               PERFORM 3800-ACCRUE-INTEREST
               PERFORM 3810-ACCUM-SOURCE
               PERFORM 3820-ASSESS-REVERSAL
               PERFORM 3830-LIMIT-PRINCIPAL
               PERFORM 3840-ASSESS-SUSPEND
               PERFORM 3850-SPLIT-ADDON
               PERFORM 3860-AGE-LAST-PAY
               PERFORM 3870-COUNT-SEQ
               PERFORM 3880-CHECK-EXPIRY
               PERFORM 3890-ACCUM-SOURCE
               PERFORM 3900-LIMIT-PRINCIPAL
               PERFORM 3910-AGE-LAST-MAINT
               PERFORM 3920-EDIT-CUST-ID
               PERFORM 3930-DESCRIBE-CHANNEL
               PERFORM 3940-SPLIT-ADDON
               PERFORM 3950-SET-CHANNEL
               PERFORM 3960-SET-ELECTION
               PERFORM 3970-FLAG-PENALTY
               PERFORM 3980-SPLIT-PENALTY
               PERFORM 3990-DESCRIBE-ELECTION
               PERFORM 4000-CHECK-CLOSE
               PERFORM 4010-EDIT-NAME
               PERFORM 4020-SPLIT-PENALTY
               PERFORM 4030-EDIT-NAME
               PERFORM 4040-SELECT-RATE
               PERFORM 4050-ASSESS-SUSPEND
               PERFORM 4060-AGE-NEXT-DUE
               PERFORM 4070-PRORATE-ADDON
               PERFORM 7030-CALL-BUSTAT
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
      * ASSESS THE PRINCIPAL YTD FEE ON THE INTEREST YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3000-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN BCDM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO BCDM-PRINCIPAL-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BCDM-OVERRIDE-FLAG = 'Y'
                   COMPUTE BCDM-PRINCIPAL-YTD ROUNDED =
                       BCDM-INTEREST-YTD * BCDM-MARGIN / 200
               WHEN BCDT-CHANNEL-CD = 'BRN'
                   MOVE 10.00 TO BCDM-PRINCIPAL-YTD
               WHEN BCDT-CHANNEL-CD = 'BAT'
                   MOVE 5.00 TO BCDM-PRINCIPAL-YTD
               WHEN OTHER
                   COMPUTE BCDM-PRINCIPAL-YTD ROUNDED =
                       BCDM-INTEREST-YTD * BCDM-MARGIN / 100
           END-EVALUATE
           IF BCDM-PRINCIPAL-YTD > 0 AND BCDM-PRINCIPAL-YTD < 1.00
               MOVE 1.00 TO BCDM-PRINCIPAL-YTD
           END-IF
           IF BCDM-PRINCIPAL-YTD > 250.00
               MOVE 250.00 TO BCDM-PRINCIPAL-YTD
           END-IF
           ADD BCDM-PRINCIPAL-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E371 CUST ID REQUIRED
      * E882 PRINCIPAL YTD NEGATIVE
      * E625 PRINCIPAL YTD OVER 10000
      * E819 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3010-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BCDM-CUST-ID = SPACES
                   MOVE 'E371' TO WS-REJECT-CODE
               WHEN BCDM-PRINCIPAL-YTD < 0
                   MOVE 'E882' TO WS-REJECT-CODE
               WHEN BCDM-PRINCIPAL-YTD > 10000.00
                   MOVE 'E625' TO WS-REJECT-CODE
               WHEN BCDM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E819' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3020-COUNT-RETRY.
           IF BCDM-RETRY-CNT < 9999999
               ADD 1 TO BCDM-RETRY-CNT
           END-IF
           IF BCDM-RETRY-CNT NOT < 6
               MOVE 'Y' TO BCDM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3030-AGE-LAST-MAINT.
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
                   ADD BCDM-BUMP-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD BCDM-BUMP-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD BCDM-BUMP-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD BCDM-BUMP-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BCDM-BUMP-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3040-AGE-NEXT-DUE.
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
                   ADD BCDM-ADDON-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD BCDM-ADDON-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD BCDM-ADDON-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD BCDM-ADDON-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BCDM-ADDON-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3050-COUNT-EXCEPT.
           IF BCDM-EXCEPT-CNT < 9999999
               ADD 1 TO BCDM-EXCEPT-CNT
           END-IF
           IF BCDM-EXCEPT-CNT NOT < 3
               MOVE 'Y' TO BCDM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3060-COUNT-RETRY.
           IF BCDM-RETRY-CNT < 9999999
               ADD 1 TO BCDM-RETRY-CNT
           END-IF
           IF BCDM-RETRY-CNT NOT < 10
               MOVE 'Y' TO BCDM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * BUMP AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3070-FLAG-BUMP.
           IF BCDT-BUMP-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE BCDT-BUMP-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PRINCIPAL YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3080-SPLIT-PRINCIPAL.
           COMPUTE WS-WORK-AMT = BCDM-PRINCIPAL-YTD / 12
           COMPUTE WS-WORK-AMT-2 = BCDM-PRINCIPAL-YTD - (WS-WORK-AMT *
               12)
           COMPUTE BCDT-BUMP-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3090-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF BCDM-CLOSE-DATE > 0 AND BCDM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BCDT-BUMP-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD BCDT-BUMP-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD BCDT-BUMP-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD BCDT-BUMP-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BCDT-BUMP-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ADDON AMT IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3100-LIMIT-ADDON.
           IF BCDT-ADDON-AMT < 10.00
               MOVE 10.00 TO BCDT-ADDON-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BCDT-ADDON-AMT > 50000.00
               MOVE 50000.00 TO BCDT-ADDON-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT INTEREST AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3110-SPLIT-INTEREST.
           COMPUTE WS-WORK-AMT = BCDT-INTEREST-AMT / 3
           COMPUTE WS-WORK-AMT-2 = BCDT-INTEREST-AMT - (WS-WORK-AMT * 3)
           COMPUTE BCDM-ADDON-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PRINCIPA GRID TIER FOR THE PRINCIPAL YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3120-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PRINCIPA-GRID-RATE(1) TO BCDM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF BCDM-PRINCIPAL-YTD NOT < WS-PRINCIPA-GRID-LOW(WS-TX)
                   MOVE WS-PRINCIPA-GRID-RATE(WS-TX) TO BCDM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BCDM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BCDM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 90 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3130-CHECK-POST.
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
               MOVE 'Y' TO BCDM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E769 CONTACT REQUIRED
      * E205 BUMP AMT NEGATIVE
      * E777 BUMP AMT OVER 5000
      * E770 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3140-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BCDM-CONTACT = SPACES
                   MOVE 'E769' TO WS-REJECT-CODE
               WHEN BCDM-BUMP-AMT < 0
                   MOVE 'E205' TO WS-REJECT-CODE
               WHEN BCDM-BUMP-AMT > 5000.00
                   MOVE 'E777' TO WS-REJECT-CODE
               WHEN BCDT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E770' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 365 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3150-CHECK-EFF.
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
               MOVE 'Y' TO BCDM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE BUMP YTD BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3160-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BCDM-CYCLE-CD
                   ADD BCDM-BUMP-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BCDM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BCDM-BUMP-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ADDON YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3170-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BCDM-SOURCE-CD
                   ADD BCDM-ADDON-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BCDM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BCDM-ADDON-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PENALTY AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3180-FLAG-PENALTY.
           IF BCDM-PENALTY-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE BCDM-PENALTY-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3190-SPLIT-INTEREST.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 12)
           COMPUTE BCDT-INTEREST-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PRINCIPA GRID TIER FOR THE INTEREST AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3200-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PRINCIPA-GRID-RATE(1) TO BCDM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF BCDM-INTEREST-AMT NOT < WS-PRINCIPA-GRID-LOW(WS-TX)
                   MOVE WS-PRINCIPA-GRID-RATE(WS-TX) TO BCDM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BCDM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BCDM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * BGJ -> LFX WHEN EFFECTIVE DATE IS PAST
      * LFX -> MLA WHEN EFFECTIVE DATE IS PAST
      * MLA -> ULA WHEN EFFECTIVE DATE IS PAST
      * ULA -> BGJ WHEN EFFECTIVE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3210-SET-REGION.
           EVALUATE BCDM-REGION-CD
               WHEN 'BGJ'
                   IF BCDM-EFFECTIVE-DATE > 0 AND BCDM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LFX' TO BCDM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LFX'
                   IF BCDM-EFFECTIVE-DATE > 0 AND BCDM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MLA' TO BCDM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MLA'
                   IF BCDM-EFFECTIVE-DATE > 0 AND BCDM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ULA' TO BCDM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ULA'
                   IF BCDM-EFFECTIVE-DATE > 0 AND BCDM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BGJ' TO BCDM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3220-AGE-EFFECTIVE.
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
                   ADD BCDT-INTEREST-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD BCDT-INTEREST-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD BCDT-INTEREST-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD BCDT-INTEREST-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BCDT-INTEREST-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INTEREST YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3230-ACCRUE-INTEREST.
           IF BCDM-INTEREST-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BCDM-INTEREST-YTD * BCDM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ADDON YTD FEE ON THE PENALTY YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3240-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN BCDT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO BCDM-ADDON-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BCDM-REVIEW-FLAG = 'Y'
                   COMPUTE BCDM-ADDON-YTD ROUNDED = BCDM-PENALTY-YTD *
                       BCDM-PENALTY-PCT / 200
               WHEN BCDM-CHANNEL-CD = 'EK'
                   MOVE 5.00 TO BCDM-ADDON-YTD
               WHEN BCDM-CHANNEL-CD = 'JE'
                   MOVE 5.00 TO BCDM-ADDON-YTD
               WHEN OTHER
                   COMPUTE BCDM-ADDON-YTD ROUNDED = BCDM-PENALTY-YTD *
                       BCDM-PENALTY-PCT / 100
           END-EVALUATE
           IF BCDM-ADDON-YTD > 0 AND BCDM-ADDON-YTD < 5.00
               MOVE 5.00 TO BCDM-ADDON-YTD
           END-IF
           IF BCDM-ADDON-YTD > 35.00
               MOVE 35.00 TO BCDM-ADDON-YTD
           END-IF
           ADD BCDM-ADDON-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3250-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE BUMP YTD BY STEP PLAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3260-ACCUM-STEP-PLAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BCDM-STEP-PLAN-CD
                   ADD BCDM-BUMP-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BCDM-STEP-PLAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BCDM-BUMP-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE ELECTION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3270-DESCRIBE-ELECTION.
           EVALUATE BCDM-ELECTION-CD
               WHEN 'L'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'M'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'N'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'R'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'V'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PENALTY AMT IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3280-LIMIT-PENALTY.
           IF BCDM-PENALTY-AMT < 0.00
               MOVE 0.00 TO BCDM-PENALTY-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BCDM-PENALTY-AMT > 50000.00
               MOVE 50000.00 TO BCDM-PENALTY-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PENALTY AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3290-ACCRUE-PENALTY.
           IF BCDT-PENALTY-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BCDT-PENALTY-AMT * BCDM-RATE / 100 / 365
               ADD WS-WORK-AMT TO BCDM-PENALTY-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT INTEREST AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3300-SPLIT-INTEREST.
           COMPUTE WS-WORK-AMT = BCDM-INTEREST-AMT / 3
           COMPUTE WS-WORK-AMT-2 = BCDM-INTEREST-AMT - (WS-WORK-AMT * 3)
           COMPUTE BCDM-BUMP-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PRINCIPA GRID TIER FOR THE BUMP YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3310-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PRINCIPA-GRID-RATE(1) TO BCDM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF BCDM-BUMP-YTD NOT < WS-PRINCIPA-GRID-LOW(WS-TX)
                   MOVE WS-PRINCIPA-GRID-RATE(WS-TX) TO BCDM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BCDM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BCDM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * BGJ -> LFX WHEN MATURITY DATE IS PAST
      * LFX -> MLA WHEN MATURITY DATE IS PAST
      * MLA -> ULA WHEN MATURITY DATE IS PAST
      * ULA -> BGJ WHEN MATURITY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3320-SET-REGION.
           EVALUATE BCDM-REGION-CD
               WHEN 'BGJ'
                   IF BCDM-MATURITY-DATE > 0 AND BCDM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LFX' TO BCDM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LFX'
                   IF BCDM-MATURITY-DATE > 0 AND BCDM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MLA' TO BCDM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MLA'
                   IF BCDM-MATURITY-DATE > 0 AND BCDM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ULA' TO BCDM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ULA'
                   IF BCDM-MATURITY-DATE > 0 AND BCDM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BGJ' TO BCDM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE ADDON TIER TIER FOR THE INTEREST AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3330-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-ADDON-TIER-RATE(1) TO BCDM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF BCDT-INTEREST-AMT NOT < WS-ADDON-TIER-LOW(WS-TX)
                   MOVE WS-ADDON-TIER-RATE(WS-TX) TO BCDM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BCDM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BCDM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE INTEREST FEE ON THE ADDON AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3340-ASSESS-STOP.
           EVALUATE TRUE
               WHEN BCDM-STOP-FLAG = 'Y'
                   MOVE ZERO TO BCDT-INTEREST-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BCDM-HOLD-FLAG = 'Y'
                   COMPUTE BCDT-INTEREST-AMT ROUNDED = BCDT-ADDON-AMT *
                       BCDM-MARGIN / 200
               WHEN BCDM-CYCLE-CD = 'A'
                   MOVE 3.00 TO BCDT-INTEREST-AMT
               WHEN BCDM-CYCLE-CD = 'S'
                   MOVE 10.00 TO BCDT-INTEREST-AMT
               WHEN OTHER
                   COMPUTE BCDT-INTEREST-AMT ROUNDED = BCDT-ADDON-AMT *
                       BCDM-MARGIN / 100
           END-EVALUATE
           IF BCDT-INTEREST-AMT > 0 AND BCDT-INTEREST-AMT < 5.00
               MOVE 5.00 TO BCDT-INTEREST-AMT
           END-IF
           IF BCDT-INTEREST-AMT > 150.00
               MOVE 150.00 TO BCDT-INTEREST-AMT
           END-IF
           ADD BCDT-INTEREST-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-90, 91-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3350-AGE-LAST-PAY.
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
                   ADD BCDT-BUMP-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD BCDT-BUMP-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD BCDT-BUMP-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD BCDT-BUMP-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BCDT-BUMP-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE ADDON TIER TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3360-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ADDON-TIER-RATE(1) TO BCDM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF WS-WORK-AMT-2 NOT < WS-ADDON-TIER-LOW(WS-TX)
                   MOVE WS-ADDON-TIER-RATE(WS-TX) TO BCDM-PENALTY-PCT
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
       3370-DESCRIBE-REGION.
           EVALUATE BCDM-REGION-CD
               WHEN 'BGJ'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'LFX'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'MLA'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'ULA'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * BGJ -> LFX WHEN LAST STMT DATE IS PAST
      * LFX -> MLA WHEN LAST STMT DATE IS PAST
      * MLA -> ULA WHEN LAST STMT DATE IS PAST
      * ULA -> BGJ WHEN LAST STMT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3380-SET-REGION.
           EVALUATE BCDM-REGION-CD
               WHEN 'BGJ'
                   IF BCDM-LAST-STMT-DATE > 0 AND BCDM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LFX' TO BCDM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LFX'
                   IF BCDM-LAST-STMT-DATE > 0 AND BCDM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MLA' TO BCDM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MLA'
                   IF BCDM-LAST-STMT-DATE > 0 AND BCDM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ULA' TO BCDM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ULA'
                   IF BCDM-LAST-STMT-DATE > 0 AND BCDM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BGJ' TO BCDM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3390-COUNT-ITEM.
           IF BCDM-ITEM-CNT < 9999999
               ADD 1 TO BCDM-ITEM-CNT
           END-IF
           IF BCDM-ITEM-CNT NOT < 10
               MOVE 'Y' TO BCDM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE BUMP YTD FEE ON THE ADDON AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3400-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN BCDM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO BCDM-BUMP-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BCDM-REVIEW-FLAG = 'Y'
                   COMPUTE BCDM-BUMP-YTD ROUNDED = BCDT-ADDON-AMT *
                       BCDM-RATE / 200
               WHEN BCDM-CHANNEL-CD = 'EK'
                   MOVE 7.00 TO BCDM-BUMP-YTD
               WHEN BCDM-CHANNEL-CD = 'JE'
                   MOVE 3.00 TO BCDM-BUMP-YTD
               WHEN OTHER
                   COMPUTE BCDM-BUMP-YTD ROUNDED = BCDT-ADDON-AMT *
                       BCDM-RATE / 100
           END-EVALUATE
           IF BCDM-BUMP-YTD > 0 AND BCDM-BUMP-YTD < 10.00
               MOVE 10.00 TO BCDM-BUMP-YTD
           END-IF
           IF BCDM-BUMP-YTD > 50.00
               MOVE 50.00 TO BCDM-BUMP-YTD
           END-IF
           ADD BCDM-BUMP-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE INTEREST FEE ON THE ADDON AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3410-ASSESS-STOP.
           EVALUATE TRUE
               WHEN BCDM-STOP-FLAG = 'Y'
                   MOVE ZERO TO BCDM-INTEREST-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BCDM-SUSPEND-FLAG = 'Y'
                   COMPUTE BCDM-INTEREST-AMT ROUNDED = BCDT-ADDON-AMT *
                       BCDM-RATE / 200
               WHEN BCDM-STEP-PLAN-CD = 'AYX'
                   MOVE 5.00 TO BCDM-INTEREST-AMT
               WHEN BCDM-STEP-PLAN-CD = 'DGU'
                   MOVE 3.00 TO BCDM-INTEREST-AMT
               WHEN OTHER
                   COMPUTE BCDM-INTEREST-AMT ROUNDED = BCDT-ADDON-AMT *
                       BCDM-RATE / 100
           END-EVALUATE
           IF BCDM-INTEREST-AMT > 0 AND BCDM-INTEREST-AMT < 2.00
               MOVE 2.00 TO BCDM-INTEREST-AMT
           END-IF
           IF BCDM-INTEREST-AMT > 50.00
               MOVE 50.00 TO BCDM-INTEREST-AMT
           END-IF
           ADD BCDM-INTEREST-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3420-AGE-NEXT-REVIEW.
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
                   ADD BCDM-INTEREST-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD BCDM-INTEREST-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD BCDM-INTEREST-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD BCDM-INTEREST-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BCDM-INTEREST-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE PENALTY AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3430-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BCDM-CYCLE-CD
                   ADD BCDM-PENALTY-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BCDM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BCDM-PENALTY-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ADDON TIER TIER FOR THE PRINCIPAL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3440-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-ADDON-TIER-RATE(1) TO BCDM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF BCDM-PRINCIPAL-AMT NOT < WS-ADDON-TIER-LOW(WS-TX)
                   MOVE WS-ADDON-TIER-RATE(WS-TX) TO BCDM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BCDM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BCDM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ADDON AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3450-SPLIT-ADDON.
           COMPUTE WS-WORK-AMT = BCDM-ADDON-AMT / 2
           COMPUTE WS-WORK-AMT-2 = BCDM-ADDON-AMT - (WS-WORK-AMT * 2)
           COMPUTE BCDM-PRINCIPAL-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3460-AGE-LAST-STMT.
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
               WHEN WS-DAYS NOT > 30
                   ADD BCDM-INTEREST-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD BCDM-INTEREST-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD BCDM-INTEREST-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BCDM-INTEREST-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3470-DESCRIBE-CYCLE.
           EVALUATE BCDM-CYCLE-CD
               WHEN 'A'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'S'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'T'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'U'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'X'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'Y'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3480-ACCRUE-MARGIN.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * BCDM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO BCDM-ADDON-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PENALTY FEE ON THE BUMP YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3490-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN BCDT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO BCDT-PENALTY-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BCDM-HOLD-FLAG = 'Y'
                   COMPUTE BCDT-PENALTY-AMT ROUNDED = BCDM-BUMP-YTD *
                       BCDM-MARGIN / 200
               WHEN BCDM-SOURCE-CD = 'AG'
                   MOVE 5.00 TO BCDT-PENALTY-AMT
               WHEN BCDM-SOURCE-CD = 'BF'
                   MOVE 5.00 TO BCDT-PENALTY-AMT
               WHEN OTHER
                   COMPUTE BCDT-PENALTY-AMT ROUNDED = BCDM-BUMP-YTD *
                       BCDM-MARGIN / 100
           END-EVALUATE
           IF BCDT-PENALTY-AMT > 0 AND BCDT-PENALTY-AMT < 1.00
               MOVE 1.00 TO BCDT-PENALTY-AMT
           END-IF
           IF BCDT-PENALTY-AMT > 100.00
               MOVE 100.00 TO BCDT-PENALTY-AMT
           END-IF
           ADD BCDT-PENALTY-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3500-COUNT-CYCLE.
           IF BCDM-CYCLE-CNT < 9999999
               ADD 1 TO BCDM-CYCLE-CNT
           END-IF
           IF BCDM-CYCLE-CNT NOT < 12
               MOVE 'Y' TO BCDM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3510-AGE-EFFECTIVE.
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
                   ADD BCDM-INTEREST-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD BCDM-INTEREST-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD BCDM-INTEREST-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD BCDM-INTEREST-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BCDM-INTEREST-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * INTEREST YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3520-FLAG-INTEREST.
           IF BCDM-INTEREST-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE BCDM-INTEREST-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STEP PLAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3530-DESCRIBE-STEP-PLAN.
           EVALUATE BCDM-STEP-PLAN-CD
               WHEN 'AYX'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'DGU'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'GWJ'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'RJL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'UPM'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'VCX'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'YPY'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE ELECTION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3540-DESCRIBE-ELECTION.
           EVALUATE BCDM-ELECTION-CD
               WHEN 'L'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'M'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'N'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'R'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'V'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3550-DESCRIBE-CHANNEL.
           EVALUATE BCDM-CHANNEL-CD
               WHEN 'EK'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'JE'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'PN'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'WV'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3560-CHECK-LAST-PAY.
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
           IF WS-DAYS > 180
               MOVE 'Y' TO BCDM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3570-DESCRIBE-REGION.
           EVALUATE BCDM-REGION-CD
               WHEN 'BGJ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'LFX'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'MLA'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ULA'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE INTEREST FEE ON THE PENALTY YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3580-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN BCDT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO BCDT-INTEREST-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BCDM-HOLD-FLAG = 'Y'
                   COMPUTE BCDT-INTEREST-AMT ROUNDED = BCDM-PENALTY-YTD
                       * BCDM-MARGIN / 200
               WHEN BCDM-ELECTION-CD = 'L'
                   MOVE 10.00 TO BCDT-INTEREST-AMT
               WHEN BCDM-ELECTION-CD = 'M'
                   MOVE 15.00 TO BCDT-INTEREST-AMT
               WHEN OTHER
                   COMPUTE BCDT-INTEREST-AMT ROUNDED = BCDM-PENALTY-YTD
                       * BCDM-MARGIN / 100
           END-EVALUATE
           IF BCDT-INTEREST-AMT > 0 AND BCDT-INTEREST-AMT < 10.00
               MOVE 10.00 TO BCDT-INTEREST-AMT
           END-IF
           IF BCDT-INTEREST-AMT > 75.00
               MOVE 75.00 TO BCDT-INTEREST-AMT
           END-IF
           ADD BCDT-INTEREST-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE INTEREST FEE ON THE PRINCIPAL AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3590-ASSESS-STOP.
           EVALUATE TRUE
               WHEN BCDM-STOP-FLAG = 'Y'
                   MOVE ZERO TO BCDM-INTEREST-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BCDM-HOLD-FLAG = 'Y'
                   COMPUTE BCDM-INTEREST-AMT ROUNDED =
                       BCDM-PRINCIPAL-AMT * BCDM-RATE / 200
               WHEN BCDT-CHANNEL-CD = 'BRN'
                   MOVE 12.00 TO BCDM-INTEREST-AMT
               WHEN BCDT-CHANNEL-CD = 'BAT'
                   MOVE 15.00 TO BCDM-INTEREST-AMT
               WHEN OTHER
                   COMPUTE BCDM-INTEREST-AMT ROUNDED =
                       BCDM-PRINCIPAL-AMT * BCDM-RATE / 100
           END-EVALUATE
           IF BCDM-INTEREST-AMT > 0 AND BCDM-INTEREST-AMT < 1.00
               MOVE 1.00 TO BCDM-INTEREST-AMT
           END-IF
           IF BCDM-INTEREST-AMT > 150.00
               MOVE 150.00 TO BCDM-INTEREST-AMT
           END-IF
           ADD BCDM-INTEREST-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INTEREST YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3600-ACCRUE-INTEREST.
           IF BCDM-INTEREST-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BCDM-INTEREST-YTD * BCDM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3610-CHECK-NEXT-DUE.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO BCDM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 30 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3620-CHECK-LAST-STMT.
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
           IF WS-DAYS > 30
               MOVE 'Y' TO BCDM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * BU -> AD WHEN NEXT DUE DATE IS PAST
      * AD -> WD WHEN NEXT DUE DATE IS PAST
      * WD -> MT WHEN NEXT DUE DATE IS PAST
      * MT -> BU WHEN NEXT DUE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3630-SET-TRAN.
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
      * ACCUMULATE BUMP AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3640-ACCUM-CHANNEL.
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
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3650-COUNT-SEQ.
           IF BCDT-SEQ < 9999999
               ADD 1 TO BCDT-SEQ
           END-IF
           IF BCDT-SEQ NOT < 6
               MOVE 'Y' TO BCDM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3660-AGE-EFFECTIVE.
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
                   ADD BCDM-INTEREST-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD BCDM-INTEREST-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD BCDM-INTEREST-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD BCDM-INTEREST-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BCDM-INTEREST-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * A -> S WHEN MATURITY DATE IS PAST
      * S -> T WHEN MATURITY DATE IS PAST
      * T -> U WHEN MATURITY DATE IS PAST
      * U -> X WHEN MATURITY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3670-SET-CYCLE.
           EVALUATE BCDM-CYCLE-CD
               WHEN 'A'
                   IF BCDM-MATURITY-DATE > 0 AND BCDM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO BCDM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF BCDM-MATURITY-DATE > 0 AND BCDM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO BCDM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF BCDM-MATURITY-DATE > 0 AND BCDM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO BCDM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF BCDM-MATURITY-DATE > 0 AND BCDM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'X' TO BCDM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRINCIPAL YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3680-FLAG-PRINCIPAL.
           IF BCDM-PRINCIPAL-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE BCDM-PRINCIPAL-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * BU -> AD WHEN LAST MAINT DATE IS PAST
      * AD -> WD WHEN LAST MAINT DATE IS PAST
      * WD -> MT WHEN LAST MAINT DATE IS PAST
      * MT -> BU WHEN LAST MAINT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3690-SET-TRAN.
           EVALUATE BCDT-TRAN-CD
               WHEN 'BU'
                   IF BCDM-LAST-MAINT-DATE > 0 AND BCDM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'AD' TO BCDT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AD'
                   IF BCDM-LAST-MAINT-DATE > 0 AND BCDM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'WD' TO BCDT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WD'
                   IF BCDM-LAST-MAINT-DATE > 0 AND BCDM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'MT' TO BCDT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MT'
                   IF BCDM-LAST-MAINT-DATE > 0 AND BCDM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'BU' TO BCDT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE BUMP AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO INTEREST AMT.
      *----------------------------------------------------------------*
       3700-PRORATE-INTEREST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BCDM-INTEREST-AMT ROUNDED =
                   BCDM-BUMP-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BCDM-INTEREST-AMT
           END-IF
           ADD BCDM-INTEREST-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E220 NAME REQUIRED
      * E245 WORK AMT NEGATIVE
      * E598 WORK AMT OVER 50000
      * E608 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3710-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BCDM-NAME = SPACES
                   MOVE 'E220' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E245' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 50000.00
                   MOVE 'E598' TO WS-REJECT-CODE
               WHEN BCDM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E608' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PENALTY YTD FEE ON THE ADDON AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3720-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN BCDM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO BCDM-PENALTY-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BCDM-HOLD-FLAG = 'Y'
                   COMPUTE BCDM-PENALTY-YTD ROUNDED = BCDM-ADDON-AMT *
                       BCDM-RATE / 200
               WHEN BCDM-REGION-CD = 'BGJ'
                   MOVE 12.00 TO BCDM-PENALTY-YTD
               WHEN BCDM-REGION-CD = 'LFX'
                   MOVE 15.00 TO BCDM-PENALTY-YTD
               WHEN OTHER
                   COMPUTE BCDM-PENALTY-YTD ROUNDED = BCDM-ADDON-AMT *
                       BCDM-RATE / 100
           END-EVALUATE
           IF BCDM-PENALTY-YTD > 0 AND BCDM-PENALTY-YTD < 10.00
               MOVE 10.00 TO BCDM-PENALTY-YTD
           END-IF
           IF BCDM-PENALTY-YTD > 50.00
               MOVE 50.00 TO BCDM-PENALTY-YTD
           END-IF
           ADD BCDM-PENALTY-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * AG -> BF WHEN EXPIRY DATE IS PAST
      * BF -> HF WHEN EXPIRY DATE IS PAST
      * HF -> HR WHEN EXPIRY DATE IS PAST
      * HR -> UF WHEN EXPIRY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3730-SET-SOURCE.
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
      * STATUS TRANSITIONS FOR TRAN CD:
      * BU -> AD WHEN EFF DATE IS PAST
      * AD -> WD WHEN EFF DATE IS PAST
      * WD -> MT WHEN EFF DATE IS PAST
      * MT -> BU WHEN EFF DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3740-SET-TRAN.
           EVALUATE BCDT-TRAN-CD
               WHEN 'BU'
                   IF BCDT-EFF-DATE > 0 AND BCDT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AD' TO BCDT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AD'
                   IF BCDT-EFF-DATE > 0 AND BCDT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WD' TO BCDT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WD'
                   IF BCDT-EFF-DATE > 0 AND BCDT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MT' TO BCDT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MT'
                   IF BCDT-EFF-DATE > 0 AND BCDT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BU' TO BCDT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3750-CHECK-EFFECTIVE.
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
               MOVE 'Y' TO BCDM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3760-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF BCDM-CLOSE-DATE > 0 AND BCDM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-CLOSE-DATE TO DTU-DATE-1
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
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3770-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * BCDM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO BCDM-BUMP-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E470 OPERATOR REQUIRED
      * E398 ADDON YTD NEGATIVE
      * E045 ADDON YTD OVER 50000
      * E998 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3780-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BCDT-OPERATOR = SPACES
                   MOVE 'E470' TO WS-REJECT-CODE
               WHEN BCDM-ADDON-YTD < 0
                   MOVE 'E398' TO WS-REJECT-CODE
               WHEN BCDM-ADDON-YTD > 50000.00
                   MOVE 'E045' TO WS-REJECT-CODE
               WHEN BCDM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E998' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PENALTY YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3790-PRORATE-PENALTY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   BCDM-PENALTY-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INTEREST AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3800-ACCRUE-INTEREST.
           IF BCDT-INTEREST-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BCDT-INTEREST-AMT * BCDM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO BCDM-INTEREST-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE INTEREST AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3810-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BCDM-SOURCE-CD
                   ADD BCDT-INTEREST-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BCDM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BCDT-INTEREST-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ADDON YTD FEE ON THE ADDON AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3820-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN BCDT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO BCDM-ADDON-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BCDM-STOP-FLAG = 'Y'
                   COMPUTE BCDM-ADDON-YTD ROUNDED = BCDT-ADDON-AMT *
                       BCDM-PENALTY-PCT / 200
               WHEN BCDM-CHANNEL-CD = 'EK'
                   MOVE 12.00 TO BCDM-ADDON-YTD
               WHEN BCDM-CHANNEL-CD = 'JE'
                   MOVE 12.00 TO BCDM-ADDON-YTD
               WHEN OTHER
                   COMPUTE BCDM-ADDON-YTD ROUNDED = BCDT-ADDON-AMT *
                       BCDM-PENALTY-PCT / 100
           END-EVALUATE
           IF BCDM-ADDON-YTD > 0 AND BCDM-ADDON-YTD < 1.00
               MOVE 1.00 TO BCDM-ADDON-YTD
           END-IF
           IF BCDM-ADDON-YTD > 25.00
               MOVE 25.00 TO BCDM-ADDON-YTD
           END-IF
           ADD BCDM-ADDON-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRINCIPAL AMT IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3830-LIMIT-PRINCIPAL.
           IF BCDM-PRINCIPAL-AMT < 50.00
               MOVE 50.00 TO BCDM-PRINCIPAL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BCDM-PRINCIPAL-AMT > 10000.00
               MOVE 10000.00 TO BCDM-PRINCIPAL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PRINCIPAL FEE ON THE INTEREST YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3840-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN BCDM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO BCDM-PRINCIPAL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BCDT-REVERSAL-FLAG = 'Y'
                   COMPUTE BCDM-PRINCIPAL-AMT ROUNDED =
                       BCDM-INTEREST-YTD * BCDM-PENALTY-PCT / 200
               WHEN BCDM-SOURCE-CD = 'AG'
                   MOVE 5.00 TO BCDM-PRINCIPAL-AMT
               WHEN BCDM-SOURCE-CD = 'BF'
                   MOVE 12.00 TO BCDM-PRINCIPAL-AMT
               WHEN OTHER
                   COMPUTE BCDM-PRINCIPAL-AMT ROUNDED =
                       BCDM-INTEREST-YTD * BCDM-PENALTY-PCT / 100
           END-EVALUATE
           IF BCDM-PRINCIPAL-AMT > 0 AND BCDM-PRINCIPAL-AMT < 5.00
               MOVE 5.00 TO BCDM-PRINCIPAL-AMT
           END-IF
           IF BCDM-PRINCIPAL-AMT > 100.00
               MOVE 100.00 TO BCDM-PRINCIPAL-AMT
           END-IF
           ADD BCDM-PRINCIPAL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT ADDON AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3850-SPLIT-ADDON.
           COMPUTE WS-WORK-AMT = BCDM-ADDON-AMT / 3
           COMPUTE WS-WORK-AMT-2 = BCDM-ADDON-AMT - (WS-WORK-AMT * 3)
           COMPUTE BCDM-INTEREST-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3860-AGE-LAST-PAY.
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
                   ADD BCDM-BUMP-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD BCDM-BUMP-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD BCDM-BUMP-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD BCDM-BUMP-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BCDM-BUMP-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3870-COUNT-SEQ.
           IF BCDT-SEQ < 9999999
               ADD 1 TO BCDT-SEQ
           END-IF
           IF BCDT-SEQ NOT < 12
               MOVE 'Y' TO BCDM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3880-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF BCDM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO BCDM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE INTEREST YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3890-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BCDM-SOURCE-CD
                   ADD BCDM-INTEREST-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BCDM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BCDM-INTEREST-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRINCIPAL AMT IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3900-LIMIT-PRINCIPAL.
           IF BCDM-PRINCIPAL-AMT < 50.00
               MOVE 50.00 TO BCDM-PRINCIPAL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BCDM-PRINCIPAL-AMT > 50000.00
               MOVE 50000.00 TO BCDM-PRINCIPAL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3910-AGE-LAST-MAINT.
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
                   ADD BCDM-INTEREST-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD BCDM-INTEREST-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD BCDM-INTEREST-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD BCDM-INTEREST-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BCDM-INTEREST-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E340 CUST ID REQUIRED
      * E604 WORK AMT 2 NEGATIVE
      * E191 WORK AMT 2 OVER 10000
      * E266 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3920-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BCDM-CUST-ID = SPACES
                   MOVE 'E340' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E604' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 10000.00
                   MOVE 'E191' TO WS-REJECT-CODE
               WHEN BCDM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E266' TO WS-REJECT-CODE
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
       3930-DESCRIBE-CHANNEL.
           EVALUATE BCDT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT ADDON YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3940-SPLIT-ADDON.
           COMPUTE WS-WORK-AMT = BCDM-ADDON-YTD / 3
           COMPUTE WS-WORK-AMT-2 = BCDM-ADDON-YTD - (WS-WORK-AMT * 3)
           COMPUTE BCDM-BUMP-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * EK -> JE WHEN MATURITY DATE IS PAST
      * JE -> PN WHEN MATURITY DATE IS PAST
      * PN -> WV WHEN MATURITY DATE IS PAST
      * WV -> EK WHEN MATURITY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3950-SET-CHANNEL.
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
      * STATUS TRANSITIONS FOR ELECTION CD:
      * L -> M WHEN LAST MAINT DATE IS PAST
      * M -> N WHEN LAST MAINT DATE IS PAST
      * N -> R WHEN LAST MAINT DATE IS PAST
      * R -> V WHEN LAST MAINT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3960-SET-ELECTION.
           EVALUATE BCDM-ELECTION-CD
               WHEN 'L'
                   IF BCDM-LAST-MAINT-DATE > 0 AND BCDM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'M' TO BCDM-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF BCDM-LAST-MAINT-DATE > 0 AND BCDM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'N' TO BCDM-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'N'
                   IF BCDM-LAST-MAINT-DATE > 0 AND BCDM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'R' TO BCDM-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF BCDM-LAST-MAINT-DATE > 0 AND BCDM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'V' TO BCDM-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PENALTY YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3970-FLAG-PENALTY.
           IF BCDM-PENALTY-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE BCDM-PENALTY-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3980-SPLIT-PENALTY.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 3)
           COMPUTE BCDM-PENALTY-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE ELECTION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3990-DESCRIBE-ELECTION.
           EVALUATE BCDM-ELECTION-CD
               WHEN 'L'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'M'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'N'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'R'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'V'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4000-CHECK-CLOSE.
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
               MOVE 'Y' TO BCDM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E827 NAME REQUIRED
      * E439 BUMP AMT NEGATIVE
      * E806 BUMP AMT OVER 5000
      * E556 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       4010-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BCDM-NAME = SPACES
                   MOVE 'E827' TO WS-REJECT-CODE
               WHEN BCDT-BUMP-AMT < 0
                   MOVE 'E439' TO WS-REJECT-CODE
               WHEN BCDT-BUMP-AMT > 5000.00
                   MOVE 'E806' TO WS-REJECT-CODE
               WHEN BCDM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E556' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PENALTY AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4020-SPLIT-PENALTY.
           COMPUTE WS-WORK-AMT = BCDM-PENALTY-AMT / 3
           COMPUTE WS-WORK-AMT-2 = BCDM-PENALTY-AMT - (WS-WORK-AMT * 3)
           COMPUTE BCDM-PRINCIPAL-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E907 NAME REQUIRED
      * E244 BUMP YTD NEGATIVE
      * E148 BUMP YTD OVER 10000
      * E960 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       4030-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BCDM-NAME = SPACES
                   MOVE 'E907' TO WS-REJECT-CODE
               WHEN BCDM-BUMP-YTD < 0
                   MOVE 'E244' TO WS-REJECT-CODE
               WHEN BCDM-BUMP-YTD > 10000.00
                   MOVE 'E148' TO WS-REJECT-CODE
               WHEN BCDT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E960' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PRINCIPA GRID TIER FOR THE PRINCIPAL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4040-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PRINCIPA-GRID-RATE(1) TO BCDM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF BCDM-PRINCIPAL-AMT NOT < WS-PRINCIPA-GRID-LOW(WS-TX)
                   MOVE WS-PRINCIPA-GRID-RATE(WS-TX) TO BCDM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BCDM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BCDM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE INTEREST YTD FEE ON THE BUMP YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       4050-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN BCDM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO BCDM-INTEREST-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BCDM-EXEMPT-FLAG = 'Y'
                   COMPUTE BCDM-INTEREST-YTD ROUNDED = BCDM-BUMP-YTD *
                       BCDM-RATE / 200
               WHEN BCDM-STEP-PLAN-CD = 'AYX'
                   MOVE 3.00 TO BCDM-INTEREST-YTD
               WHEN BCDM-STEP-PLAN-CD = 'DGU'
                   MOVE 15.00 TO BCDM-INTEREST-YTD
               WHEN OTHER
                   COMPUTE BCDM-INTEREST-YTD ROUNDED = BCDM-BUMP-YTD *
                       BCDM-RATE / 100
           END-EVALUATE
           IF BCDM-INTEREST-YTD > 0 AND BCDM-INTEREST-YTD < 5.00
               MOVE 5.00 TO BCDM-INTEREST-YTD
           END-IF
           IF BCDM-INTEREST-YTD > 150.00
               MOVE 150.00 TO BCDM-INTEREST-YTD
           END-IF
           ADD BCDM-INTEREST-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4060-AGE-NEXT-DUE.
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
                   ADD BCDT-ADDON-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD BCDT-ADDON-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD BCDT-ADDON-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD BCDT-ADDON-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BCDT-ADDON-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE ADDON AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       4070-PRORATE-ADDON.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   BCDM-ADDON-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

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
                   MOVE BUSTAT-BUMP-AMT
                     TO BCDM-BUMP-AMT
                   MOVE BUSTAT-PENALTY-AMT
                     TO BCDM-PENALTY-AMT
                   MOVE BUSTAT-INTEREST-YTD
                     TO BCDM-INTEREST-YTD
                   MOVE BUSTAT-OVERRIDE-FLAG
                     TO BCDM-OVERRIDE-FLAG
                   MOVE BUSTAT-REVIEW-FLAG
                     TO BCDM-REVIEW-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RTAT' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7030-CALL-BUSTAT' TO ABND-PARAGRAPH
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
           DISPLAY 'BUDANAL READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'BUDANAL' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
