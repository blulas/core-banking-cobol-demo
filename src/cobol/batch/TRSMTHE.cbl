      *================================================================*
      * PROGRAM  : TRSMTHE                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : PERSONAL TRUST - MONTH-END. READS THE TRS           *
      *            TRANSACTIONS, UPDATES THE TRS MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. PERSONAL TRUST ACCOUNTING,      *
      *            INCOME DISTRIBUTION AND TRUSTEE FEES                *
      * INPUT    : TRSMST (KSDS), TRSTRN, BANKCTL                      *
      * OUTPUT   : TRSOUT, TRSMTRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, TRPEN                   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1986-01-15 CRP  ORIGINAL                                       *
      * 1989-07-11 RJT  TAX CALCULATION CORRECTED CHG1741              *
      * 1991-05-06 RJT  PRINCIPAL RERUN PROTECTION CHG8110             *
      * 1992-11-13 PQR  PRINCIPAL ROUNDING - HALF UP PRB0858           *
      * 1996-03-03 JLM  INCOME EXCEPTIONS TO OPS QUEUE CHG4171         *
      * 1998-11-22 TWH  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1998-01-14 GAF  INCOME CALCULATION CORRECTED REQ9342           *
      * 2000-03-08 SKN  DISTRIB ROUNDING - HALF UP REQ1205             *
      * 2002-09-17 CRP  AUDIT FINDING - MARKET CONTROL TOTALS PRB4007  *
      * 2004-09-22 GAF  MARKET STATUS CODES ADDED PRB1876              *
      * 2005-02-01 TWH  INCOME LIMIT REVISED CHG2815                   *
      * 2007-02-20 RJT  TRUSTEE-FEE CALCULATION CORRECTED INC5797      *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRSMTHE.
       AUTHOR. C R PATEL.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "TRSMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS TRSM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "TRSTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "TRSOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "TRSMTRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY TRSMST.
       FD  TRAN-FILE.
       COPY TRSTRN.
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
                                           VALUE 24.000000.
           05  WS-DAYS                     PIC S9(07) VALUE 0.
           05  WS-PERIOD-DAYS              PIC S9(05) VALUE 28.
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
      * WS-TAX-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-TAX-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000888500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000299500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011000000000223500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011500000000235500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011550000000456000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012050000000599500'.
       01  WS-TAX-TIER-TBL REDEFINES WS-TAX-TIER-VALUES.
           05  WS-TAX-TIER-ROW OCCURS 6 TIMES.
               10  WS-TAX-TIER-LOW         PIC 9(09)V99.
               10  WS-TAX-TIER-RATE        PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-DISTRIB-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-DISTRIB-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000422000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000870500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000179500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006000000000219000'.
       01  WS-DISTRIB-BAND-TBL REDEFINES WS-DISTRIB-BAND-VALUES.
           05  WS-DISTRIB-BAND-ROW OCCURS 4 TIMES.
               10  WS-DISTRIB-BAND-LOW     PIC 9(09)V99.
               10  WS-DISTRIB-BAND-RATE    PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  TRSMTHE  PERSONAL TRUST'.
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
       COPY TRPENP.

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
               MOVE 'TRSMST' TO ABND-FILE-NAME
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
           MOVE TRST-KEY TO TRSM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-LIMIT-TAX
               PERFORM 3010-PRORATE-MARKET
               PERFORM 3020-CHECK-LAST-MAINT
               PERFORM 3030-SET-TRUST-TYPE
               PERFORM 3040-COUNT-RETRY
               PERFORM 3050-SELECT-RATE
               PERFORM 3060-AGE-MATURITY
               PERFORM 3070-SET-DIST-FREQ
               PERFORM 3080-PRORATE-INCOME
               PERFORM 3090-SELECT-MARGIN
               PERFORM 3100-ACCUM-TRUST-TYPE
               PERFORM 3110-PRORATE-PRINCIPAL
               PERFORM 3120-SET-STATUS
               PERFORM 3130-DESCRIBE-TRAN
               PERFORM 3140-ASSESS-NOTICE-SENT
               PERFORM 3150-SELECT-RATE
               PERFORM 3160-ASSESS-REVIEW
               PERFORM 3170-SPLIT-DISTRIB
               PERFORM 3180-AGE-POST
               PERFORM 3190-SELECT-MARGIN
               PERFORM 3200-EDIT-DESC
               PERFORM 3210-SET-TRAN
               PERFORM 3220-SPLIT-DISTRIB
               PERFORM 3230-SPLIT-MARKET
               PERFORM 3240-ACCRUE-MARKET
               PERFORM 3250-AGE-LAST-MAINT
               PERFORM 3260-SPLIT-DISTRIB
               PERFORM 3270-ACCRUE-MARGIN
               PERFORM 3280-SPLIT-INCOME
               PERFORM 3290-ASSESS-NOTICE-SENT
               PERFORM 3300-CHECK-CLOSE
               PERFORM 3310-FLAG-TRUSTEE-FEE
               PERFORM 3320-DESCRIBE-DIST-FREQ
               PERFORM 3330-ASSESS-STOP
               PERFORM 3340-AGE-LAST-MAINT
               PERFORM 3350-AGE-POST
               PERFORM 3360-DESCRIBE-STATUS
               PERFORM 3370-PRORATE-TAX
               PERFORM 3380-SELECT-DISCOUNT
               PERFORM 3390-FLAG-MARKET
               PERFORM 3400-SET-TRUST-TYPE
               PERFORM 3410-ACCRUE-DISTRIB
               PERFORM 3420-SPLIT-PRINCIPAL
               PERFORM 3430-EDIT-MEMO
               PERFORM 3440-FLAG-MARKET
               PERFORM 3450-SPLIT-TRUSTEE-FEE
               PERFORM 3460-FLAG-PRINCIPAL
               PERFORM 3470-EDIT-CONTACT
               PERFORM 3480-PRORATE-TAX
               PERFORM 3490-EDIT-REJECT
               PERFORM 3500-LIMIT-ITEM
               PERFORM 3510-ASSESS-REVIEW
               PERFORM 3520-FLAG-PRINCIPAL
               PERFORM 3530-ACCUM-CYCLE
               PERFORM 3540-ACCRUE-DISCOUNT
               PERFORM 3550-COUNT-SEQ
               PERFORM 3560-FLAG-MARKET
               PERFORM 3570-PRORATE-TRUSTEE-FEE
               PERFORM 3580-DESCRIBE-REGION
               PERFORM 3590-PRORATE-DISTRIB
               PERFORM 3600-DESCRIBE-REGION
               PERFORM 3610-DESCRIBE-CYCLE
               PERFORM 3620-COUNT-SEQ
               PERFORM 3630-COUNT-RETRY
               PERFORM 3640-EDIT-REJECT
               PERFORM 3650-AGE-LAST-STMT
               PERFORM 3660-COUNT-NOTICE
               PERFORM 3670-AGE-LAST-PAY
               PERFORM 3680-CHECK-LAST-MAINT
               PERFORM 3690-COUNT-RETRY
               PERFORM 3700-ACCRUE-DISTRIB
               PERFORM 3710-SPLIT-MARKET
               PERFORM 3720-EDIT-MEMO
               PERFORM 3730-ASSESS-NOTICE-SENT
               PERFORM 3740-LIMIT-TAX
               PERFORM 3750-COUNT-SEQ
               PERFORM 3760-ACCUM-CHANNEL
               PERFORM 3770-EDIT-REJECT
               PERFORM 3780-SELECT-RATE
               PERFORM 3790-SELECT-RATE
               PERFORM 3800-AGE-LAST-PAY
               PERFORM 3810-ACCUM-DIST-FREQ
               PERFORM 3820-CHECK-EFF
               PERFORM 3830-DESCRIBE-CHANNEL
               PERFORM 3840-ACCRUE-PRINCIPAL
               PERFORM 3850-ASSESS-REVERSAL
               PERFORM 3860-LIMIT-PRINCIPAL
               PERFORM 3870-ASSESS-NOTICE-SENT
               PERFORM 3880-CHECK-CLOSE
               PERFORM 3890-ACCUM-STATUS
               PERFORM 3900-CHECK-NOTICE
               PERFORM 3910-EDIT-CONTACT
               PERFORM 3920-DESCRIBE-REGION
               PERFORM 3930-AGE-CLOSE
               PERFORM 3940-SET-CYCLE
               PERFORM 3950-SET-STATUS
               PERFORM 3960-PRORATE-TAX
               PERFORM 3970-SET-REASON
               PERFORM 3980-CHECK-LAST-STMT
               PERFORM 3990-AGE-OPEN
               PERFORM 4000-COUNT-CYCLE
               PERFORM 4010-EDIT-REJECT
               PERFORM 4020-AGE-LAST-STMT
               PERFORM 4030-SET-REGION
               PERFORM 4040-SPLIT-PRINCIPAL
               PERFORM 4050-SET-CHANNEL
               PERFORM 4060-SPLIT-PRINCIPAL
               PERFORM 4070-ACCRUE-PRINCIPAL
               PERFORM 4080-ACCUM-TRAN
               PERFORM 4090-SELECT-DISCOUNT
               PERFORM 4100-LIMIT-PRINCIPAL
               PERFORM 4110-DESCRIBE-STATUS
               PERFORM 4120-CHECK-EXPIRY
               PERFORM 4130-COUNT-CYCLE
               PERFORM 4140-DESCRIBE-CYCLE
               PERFORM 4150-FLAG-ITEM
               PERFORM 4160-PRORATE-MARKET
               PERFORM 4170-PRORATE-TRUSTEE-FEE
               PERFORM 4180-COUNT-CYCLE
               PERFORM 4190-CHECK-LAST-MAINT
               PERFORM 4200-EDIT-CONTACT
               PERFORM 4210-CHECK-LAST-STMT
               PERFORM 4220-COUNT-CYCLE
               PERFORM 4230-CHECK-EFF
               PERFORM 4240-CHECK-NOTICE
               PERFORM 4250-DESCRIBE-TRAN
               PERFORM 4260-CHECK-EXPIRY
               PERFORM 4270-ACCRUE-INCOME
               PERFORM 4280-ACCRUE-TAX
               PERFORM 4290-DESCRIBE-REGION
               PERFORM 4300-SPLIT-MARKET
               PERFORM 4310-AGE-LAST-STMT
               PERFORM 4320-AGE-EXPIRY
               PERFORM 7010-CALL-TRPEN
               REWRITE TRS-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'TRSMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM TRS-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * TAX AMT IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3000-LIMIT-TAX.
           IF TRSM-TAX-AMT < 50.00
               MOVE 50.00 TO TRSM-TAX-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRSM-TAX-AMT > 10000.00
               MOVE 10000.00 TO TRSM-TAX-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO MARKET AMT.
      *----------------------------------------------------------------*
       3010-PRORATE-MARKET.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRSM-MARKET-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TRSM-MARKET-AMT
           END-IF
           ADD TRSM-MARKET-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 120 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3020-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF TRSM-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO TRSM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRUST TYPE CD:
      * FLJ -> MNG WHEN POST DATE IS PAST
      * MNG -> VFN WHEN POST DATE IS PAST
      * VFN -> XPL WHEN POST DATE IS PAST
      * XPL -> FLJ WHEN POST DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3030-SET-TRUST-TYPE.
           EVALUATE TRSM-TRUST-TYPE-CD
               WHEN 'FLJ'
                   IF TRST-POST-DATE > 0 AND TRST-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MNG' TO TRSM-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MNG'
                   IF TRST-POST-DATE > 0 AND TRST-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VFN' TO TRSM-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VFN'
                   IF TRST-POST-DATE > 0 AND TRST-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XPL' TO TRSM-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XPL'
                   IF TRST-POST-DATE > 0 AND TRST-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FLJ' TO TRSM-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3040-COUNT-RETRY.
           IF TRSM-RETRY-CNT < 9999999
               ADD 1 TO TRSM-RETRY-CNT
           END-IF
           IF TRSM-RETRY-CNT NOT < 3
               MOVE 'Y' TO TRSM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE TAX TIER TIER FOR THE TAX AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3050-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-TAX-TIER-RATE(1) TO TRSM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF TRSM-TAX-AMT NOT < WS-TAX-TIER-LOW(WS-TX)
                   MOVE WS-TAX-TIER-RATE(WS-TX) TO TRSM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRSM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRSM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3060-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF TRSM-MATURITY-DATE > 0 AND TRSM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRSM-DISTRIB-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD TRSM-DISTRIB-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD TRSM-DISTRIB-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TRSM-DISTRIB-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRSM-DISTRIB-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DIST FREQ CD:
      * E -> K WHEN NOTICE DATE IS PAST
      * K -> P WHEN NOTICE DATE IS PAST
      * P -> W WHEN NOTICE DATE IS PAST
      * W -> Y WHEN NOTICE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3070-SET-DIST-FREQ.
           EVALUATE TRSM-DIST-FREQ-CD
               WHEN 'E'
                   IF TRSM-NOTICE-DATE > 0 AND TRSM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'K' TO TRSM-DIST-FREQ-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF TRSM-NOTICE-DATE > 0 AND TRSM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'P' TO TRSM-DIST-FREQ-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'P'
                   IF TRSM-NOTICE-DATE > 0 AND TRSM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO TRSM-DIST-FREQ-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'W'
                   IF TRSM-NOTICE-DATE > 0 AND TRSM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'Y' TO TRSM-DIST-FREQ-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE TRUSTEE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO INCOME AMT.
      *----------------------------------------------------------------*
       3080-PRORATE-INCOME.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRST-INCOME-AMT ROUNDED =
                   TRSM-TRUSTEE-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TRST-INCOME-AMT
           END-IF
           ADD TRST-INCOME-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE DISTRIB BAND TIER FOR THE INCOME YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3090-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-DISTRIB-BAND-RATE(1) TO TRSM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF TRSM-INCOME-YTD NOT < WS-DISTRIB-BAND-LOW(WS-TX)
                   MOVE WS-DISTRIB-BAND-RATE(WS-TX) TO TRSM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRSM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRSM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE MARKET AMT BY TRUST TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3100-ACCUM-TRUST-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRSM-TRUST-TYPE-CD
                   ADD TRSM-MARKET-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRSM-TRUST-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRSM-MARKET-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DISTRIB AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PRINCIPAL AMT.
      *----------------------------------------------------------------*
       3110-PRORATE-PRINCIPAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRSM-PRINCIPAL-AMT ROUNDED =
                   TRST-DISTRIB-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TRSM-PRINCIPAL-AMT
           END-IF
           ADD TRSM-PRINCIPAL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * EH -> GG WHEN LAST STMT DATE IS PAST
      * GG -> JG WHEN LAST STMT DATE IS PAST
      * JG -> EH WHEN LAST STMT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3120-SET-STATUS.
           EVALUATE TRSM-STATUS-CD
               WHEN 'EH'
                   IF TRSM-LAST-STMT-DATE > 0 AND TRSM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GG' TO TRSM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GG'
                   IF TRSM-LAST-STMT-DATE > 0 AND TRSM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JG' TO TRSM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JG'
                   IF TRSM-LAST-STMT-DATE > 0 AND TRSM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EH' TO TRSM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3130-DESCRIBE-TRAN.
           EVALUATE TRST-TRAN-CD
               WHEN 'IN'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'DS'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'FE'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'RB'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'TX'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE DISTRIB FEE ON THE TRUSTEE FEE AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3140-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN TRSM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO TRSM-DISTRIB-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-STOP-FLAG = 'Y'
                   COMPUTE TRSM-DISTRIB-AMT ROUNDED =
                       TRST-TRUSTEE-FEE-AMT * TRSM-MARGIN / 200
               WHEN TRSM-REGION-CD = 'A'
                   MOVE 15.00 TO TRSM-DISTRIB-AMT
               WHEN TRSM-REGION-CD = 'D'
                   MOVE 7.00 TO TRSM-DISTRIB-AMT
               WHEN OTHER
                   COMPUTE TRSM-DISTRIB-AMT ROUNDED =
                       TRST-TRUSTEE-FEE-AMT * TRSM-MARGIN / 100
           END-EVALUATE
           IF TRSM-DISTRIB-AMT > 0 AND TRSM-DISTRIB-AMT < 1.00
               MOVE 1.00 TO TRSM-DISTRIB-AMT
           END-IF
           IF TRSM-DISTRIB-AMT > 100.00
               MOVE 100.00 TO TRSM-DISTRIB-AMT
           END-IF
           ADD TRSM-DISTRIB-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE TAX TIER TIER FOR THE PRINCIPAL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3150-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-TAX-TIER-RATE(1) TO TRSM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF TRST-PRINCIPAL-AMT NOT < WS-TAX-TIER-LOW(WS-TX)
                   MOVE WS-TAX-TIER-RATE(WS-TX) TO TRSM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRSM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRSM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE TRUSTEE FEE AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3160-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN TRSM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = TRSM-TRUSTEE-FEE-AMT
                       * TRSM-RATE / 200
               WHEN TRSM-STATUS-CD = 'EH'
                   MOVE 5.00 TO WS-WORK-AMT-2
               WHEN TRSM-STATUS-CD = 'GG'
                   MOVE 15.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = TRSM-TRUSTEE-FEE-AMT
                       * TRSM-RATE / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 1.00
               MOVE 1.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 75.00
               MOVE 75.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT DISTRIB YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3170-SPLIT-DISTRIB.
           COMPUTE WS-WORK-AMT = TRSM-DISTRIB-YTD / 3
           COMPUTE WS-WORK-AMT-2 = TRSM-DISTRIB-YTD - (WS-WORK-AMT * 3)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-60, 61-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3180-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF TRST-POST-DATE > 0 AND TRST-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRST-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRSM-MARKET-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD TRSM-MARKET-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD TRSM-MARKET-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TRSM-MARKET-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRSM-MARKET-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE TAX TIER TIER FOR THE TAX YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3190-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-TAX-TIER-RATE(1) TO TRSM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF TRSM-TAX-YTD NOT < WS-TAX-TIER-LOW(WS-TX)
                   MOVE WS-TAX-TIER-RATE(WS-TX) TO TRSM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRSM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRSM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E459 DESC REQUIRED
      * E235 WORK AMT 2 NEGATIVE
      * E795 WORK AMT 2 OVER 99999
      * E609 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3200-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRSM-DESC = SPACES
                   MOVE 'E459' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E235' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 99999.00
                   MOVE 'E795' TO WS-REJECT-CODE
               WHEN TRSM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E609' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * IN -> DS WHEN POST DATE IS PAST
      * DS -> FE WHEN POST DATE IS PAST
      * FE -> RB WHEN POST DATE IS PAST
      * RB -> TX WHEN POST DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3210-SET-TRAN.
           EVALUATE TRST-TRAN-CD
               WHEN 'IN'
                   IF TRST-POST-DATE > 0 AND TRST-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DS' TO TRST-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DS'
                   IF TRST-POST-DATE > 0 AND TRST-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FE' TO TRST-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FE'
                   IF TRST-POST-DATE > 0 AND TRST-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RB' TO TRST-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RB'
                   IF TRST-POST-DATE > 0 AND TRST-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TX' TO TRST-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT DISTRIB AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3220-SPLIT-DISTRIB.
           COMPUTE WS-WORK-AMT = TRST-DISTRIB-AMT / 12
           COMPUTE WS-WORK-AMT-2 = TRST-DISTRIB-AMT - (WS-WORK-AMT * 12)
           COMPUTE TRSM-MARKET-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT MARKET AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3230-SPLIT-MARKET.
           COMPUTE WS-WORK-AMT = TRSM-MARKET-AMT / 12
           COMPUTE WS-WORK-AMT-2 = TRSM-MARKET-AMT - (WS-WORK-AMT * 12)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON MARKET AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3240-ACCRUE-MARKET.
           IF TRSM-MARKET-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRSM-MARKET-AMT * TRSM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO TRST-PRINCIPAL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3250-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF TRSM-LAST-MAINT-DATE > 0 AND TRSM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-LAST-MAINT-DATE TO DTU-DATE-1
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
      * SPLIT DISTRIB YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3260-SPLIT-DISTRIB.
           COMPUTE WS-WORK-AMT = TRSM-DISTRIB-YTD / 3
           COMPUTE WS-WORK-AMT-2 = TRSM-DISTRIB-YTD - (WS-WORK-AMT * 3)
           COMPUTE TRSM-PRINCIPAL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3270-ACCRUE-MARGIN.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * TRSM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO TRSM-INCOME-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT INCOME YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3280-SPLIT-INCOME.
           COMPUTE WS-WORK-AMT = TRSM-INCOME-YTD / 4
           COMPUTE WS-WORK-AMT-2 = TRSM-INCOME-YTD - (WS-WORK-AMT * 4)
           COMPUTE TRSM-MARKET-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE TRUSTEE FEE FEE ON THE PRINCIPAL AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3290-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN TRSM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO TRSM-TRUSTEE-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-PREFERRED-FLAG = 'Y'
                   COMPUTE TRSM-TRUSTEE-FEE-AMT ROUNDED =
                       TRSM-PRINCIPAL-AMT * TRSM-RATE / 200
               WHEN TRST-CHANNEL-CD = 'BRN'
                   MOVE 20.00 TO TRSM-TRUSTEE-FEE-AMT
               WHEN TRST-CHANNEL-CD = 'BAT'
                   MOVE 10.00 TO TRSM-TRUSTEE-FEE-AMT
               WHEN OTHER
                   COMPUTE TRSM-TRUSTEE-FEE-AMT ROUNDED =
                       TRSM-PRINCIPAL-AMT * TRSM-RATE / 100
           END-EVALUATE
           IF TRSM-TRUSTEE-FEE-AMT > 0 AND TRSM-TRUSTEE-FEE-AMT < 2.00
               MOVE 2.00 TO TRSM-TRUSTEE-FEE-AMT
           END-IF
           IF TRSM-TRUSTEE-FEE-AMT > 50.00
               MOVE 50.00 TO TRSM-TRUSTEE-FEE-AMT
           END-IF
           ADD TRSM-TRUSTEE-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3300-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF TRSM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO TRSM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRUSTEE FEE AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3310-FLAG-TRUSTEE-FEE.
           IF TRSM-TRUSTEE-FEE-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TRSM-TRUSTEE-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE DIST FREQ CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3320-DESCRIBE-DIST-FREQ.
           EVALUATE TRSM-DIST-FREQ-CD
               WHEN 'E'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'K'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'P'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'W'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'Y'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'Z'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE TAX YTD FEE ON THE MARKET YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3330-ASSESS-STOP.
           EVALUATE TRUE
               WHEN TRSM-STOP-FLAG = 'Y'
                   MOVE ZERO TO TRSM-TAX-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-SUSPEND-FLAG = 'Y'
                   COMPUTE TRSM-TAX-YTD ROUNDED = TRSM-MARKET-YTD *
                       TRSM-MARGIN / 200
               WHEN TRSM-DIST-FREQ-CD = 'E'
                   MOVE 20.00 TO TRSM-TAX-YTD
               WHEN TRSM-DIST-FREQ-CD = 'K'
                   MOVE 15.00 TO TRSM-TAX-YTD
               WHEN OTHER
                   COMPUTE TRSM-TAX-YTD ROUNDED = TRSM-MARKET-YTD *
                       TRSM-MARGIN / 100
           END-EVALUATE
           IF TRSM-TAX-YTD > 0 AND TRSM-TAX-YTD < 2.00
               MOVE 2.00 TO TRSM-TAX-YTD
           END-IF
           IF TRSM-TAX-YTD > 75.00
               MOVE 75.00 TO TRSM-TAX-YTD
           END-IF
           ADD TRSM-TAX-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3340-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF TRSM-LAST-MAINT-DATE > 0 AND TRSM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRSM-TRUSTEE-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TRSM-TRUSTEE-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD TRSM-TRUSTEE-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TRSM-TRUSTEE-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRSM-TRUSTEE-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3350-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF TRST-POST-DATE > 0 AND TRST-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRST-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRSM-DISTRIB-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TRSM-DISTRIB-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD TRSM-DISTRIB-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD TRSM-DISTRIB-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRSM-DISTRIB-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3360-DESCRIBE-STATUS.
           EVALUATE TRSM-STATUS-CD
               WHEN 'EH'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'GG'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'JG'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE MARKET YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO TAX AMT.
      *----------------------------------------------------------------*
       3370-PRORATE-TAX.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRSM-TAX-AMT ROUNDED =
                   TRSM-MARKET-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TRSM-TAX-AMT
           END-IF
           ADD TRSM-TAX-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE DISTRIB BAND TIER FOR THE DISTRIB YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3380-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-DISTRIB-BAND-RATE(1) TO TRSM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF TRSM-DISTRIB-YTD NOT < WS-DISTRIB-BAND-LOW(WS-TX)
                   MOVE WS-DISTRIB-BAND-RATE(WS-TX) TO TRSM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRSM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRSM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * MARKET YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3390-FLAG-MARKET.
           IF TRSM-MARKET-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TRSM-MARKET-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRUST TYPE CD:
      * FLJ -> MNG WHEN CLOSE DATE IS PAST
      * MNG -> VFN WHEN CLOSE DATE IS PAST
      * VFN -> XPL WHEN CLOSE DATE IS PAST
      * XPL -> FLJ WHEN CLOSE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3400-SET-TRUST-TYPE.
           EVALUATE TRSM-TRUST-TYPE-CD
               WHEN 'FLJ'
                   IF TRSM-CLOSE-DATE > 0 AND TRSM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MNG' TO TRSM-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MNG'
                   IF TRSM-CLOSE-DATE > 0 AND TRSM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VFN' TO TRSM-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VFN'
                   IF TRSM-CLOSE-DATE > 0 AND TRSM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XPL' TO TRSM-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XPL'
                   IF TRSM-CLOSE-DATE > 0 AND TRSM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FLJ' TO TRSM-TRUST-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DISTRIB AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3410-ACCRUE-DISTRIB.
           IF TRSM-DISTRIB-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRSM-DISTRIB-AMT * TRSM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO TRSM-MARKET-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PRINCIPAL AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3420-SPLIT-PRINCIPAL.
           COMPUTE WS-WORK-AMT = TRSM-PRINCIPAL-AMT / 12
           COMPUTE WS-WORK-AMT-2 = TRSM-PRINCIPAL-AMT - (WS-WORK-AMT *
               12)
           COMPUTE TRSM-DISTRIB-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E551 MEMO REQUIRED
      * E507 WORK AMT 2 NEGATIVE
      * E011 WORK AMT 2 OVER 10000
      * E091 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3430-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRST-MEMO = SPACES
                   MOVE 'E551' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E507' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 10000.00
                   MOVE 'E011' TO WS-REJECT-CODE
               WHEN TRSM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E091' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * MARKET AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3440-FLAG-MARKET.
           IF TRSM-MARKET-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TRSM-MARKET-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT TRUSTEE FEE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3450-SPLIT-TRUSTEE-FEE.
           COMPUTE WS-WORK-AMT = TRST-TRUSTEE-FEE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = TRST-TRUSTEE-FEE-AMT - (WS-WORK-AMT *
               3)
           COMPUTE TRSM-DISTRIB-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRINCIPAL YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3460-FLAG-PRINCIPAL.
           IF TRSM-PRINCIPAL-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TRSM-PRINCIPAL-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E599 CONTACT REQUIRED
      * E556 MARKET YTD NEGATIVE
      * E208 MARKET YTD OVER 25000
      * E317 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3470-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRSM-CONTACT = SPACES
                   MOVE 'E599' TO WS-REJECT-CODE
               WHEN TRSM-MARKET-YTD < 0
                   MOVE 'E556' TO WS-REJECT-CODE
               WHEN TRSM-MARKET-YTD > 25000.00
                   MOVE 'E208' TO WS-REJECT-CODE
               WHEN TRSM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E317' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE TAX AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3480-PRORATE-TAX.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRSM-TAX-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E610 REJECT CD REQUIRED
      * E849 TRUSTEE FEE AMT NEGATIVE
      * E214 TRUSTEE FEE AMT OVER 99999
      * E488 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3490-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRST-REJECT-CD = SPACES
                   MOVE 'E610' TO WS-REJECT-CODE
               WHEN TRST-TRUSTEE-FEE-AMT < 0
                   MOVE 'E849' TO WS-REJECT-CODE
               WHEN TRST-TRUSTEE-FEE-AMT > 99999.00
                   MOVE 'E214' TO WS-REJECT-CODE
               WHEN TRSM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E488' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3500-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 50.00
               MOVE 50.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 1000.00
               MOVE 1000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PRINCIPAL FEE ON THE INCOME AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3510-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN TRSM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO TRST-PRINCIPAL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-PREFERRED-FLAG = 'Y'
                   COMPUTE TRST-PRINCIPAL-AMT ROUNDED = TRST-INCOME-AMT
                       * TRSM-DISCOUNT / 200
               WHEN TRST-TRAN-CD = 'IN'
                   MOVE 15.00 TO TRST-PRINCIPAL-AMT
               WHEN TRST-TRAN-CD = 'DS'
                   MOVE 5.00 TO TRST-PRINCIPAL-AMT
               WHEN OTHER
                   COMPUTE TRST-PRINCIPAL-AMT ROUNDED = TRST-INCOME-AMT
                       * TRSM-DISCOUNT / 100
           END-EVALUATE
           IF TRST-PRINCIPAL-AMT > 0 AND TRST-PRINCIPAL-AMT < 1.00
               MOVE 1.00 TO TRST-PRINCIPAL-AMT
           END-IF
           IF TRST-PRINCIPAL-AMT > 100.00
               MOVE 100.00 TO TRST-PRINCIPAL-AMT
           END-IF
           ADD TRST-PRINCIPAL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRINCIPAL AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3520-FLAG-PRINCIPAL.
           IF TRST-PRINCIPAL-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TRST-PRINCIPAL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE INCOME AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3530-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRSM-CYCLE-CD
                   ADD TRST-INCOME-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRSM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRST-INCOME-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3540-ACCRUE-DISCOUNT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * TRSM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO TRSM-DISTRIB-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3550-COUNT-SEQ.
           IF TRST-SEQ < 9999999
               ADD 1 TO TRST-SEQ
           END-IF
           IF TRST-SEQ NOT < 12
               MOVE 'Y' TO TRSM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * MARKET AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3560-FLAG-MARKET.
           IF TRSM-MARKET-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TRSM-MARKET-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO TRUSTEE FEE AMT.
      *----------------------------------------------------------------*
       3570-PRORATE-TRUSTEE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRSM-TRUSTEE-FEE-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TRSM-TRUSTEE-FEE-AMT
           END-IF
           ADD TRSM-TRUSTEE-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3580-DESCRIBE-REGION.
           EVALUATE TRSM-REGION-CD
               WHEN 'A'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'D'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'N'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'T'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE TRUSTEE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DISTRIB YTD.
      *----------------------------------------------------------------*
       3590-PRORATE-DISTRIB.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRSM-DISTRIB-YTD ROUNDED =
                   TRST-TRUSTEE-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TRSM-DISTRIB-YTD
           END-IF
           ADD TRSM-DISTRIB-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3600-DESCRIBE-REGION.
           EVALUATE TRSM-REGION-CD
               WHEN 'A'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'D'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'N'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'T'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3610-DESCRIBE-CYCLE.
           EVALUATE TRSM-CYCLE-CD
               WHEN 'EN'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'FY'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'HP'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'JB'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'KZ'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'LD'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'RY'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3620-COUNT-SEQ.
           IF TRST-SEQ < 9999999
               ADD 1 TO TRST-SEQ
           END-IF
           IF TRST-SEQ NOT < 5
               MOVE 'Y' TO TRSM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3630-COUNT-RETRY.
           IF TRSM-RETRY-CNT < 9999999
               ADD 1 TO TRSM-RETRY-CNT
           END-IF
           IF TRSM-RETRY-CNT NOT < 12
               MOVE 'Y' TO TRST-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E189 REJECT CD REQUIRED
      * E763 INCOME AMT NEGATIVE
      * E999 INCOME AMT OVER 50000
      * E972 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3640-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRST-REJECT-CD = SPACES
                   MOVE 'E189' TO WS-REJECT-CODE
               WHEN TRST-INCOME-AMT < 0
                   MOVE 'E763' TO WS-REJECT-CODE
               WHEN TRST-INCOME-AMT > 50000.00
                   MOVE 'E999' TO WS-REJECT-CODE
               WHEN TRSM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E972' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-60, 61-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3650-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF TRSM-LAST-STMT-DATE > 0 AND TRSM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3660-COUNT-NOTICE.
           IF TRSM-NOTICE-CNT < 9999999
               ADD 1 TO TRSM-NOTICE-CNT
           END-IF
           IF TRSM-NOTICE-CNT NOT < 6
               MOVE 'Y' TO TRSM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3670-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF TRSM-LAST-PAY-DATE > 0 AND TRSM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRSM-DISTRIB-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD TRSM-DISTRIB-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TRSM-DISTRIB-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD TRSM-DISTRIB-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRSM-DISTRIB-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 180 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3680-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF TRSM-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO TRSM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3690-COUNT-RETRY.
           IF TRSM-RETRY-CNT < 9999999
               ADD 1 TO TRSM-RETRY-CNT
           END-IF
           IF TRSM-RETRY-CNT NOT < 6
               MOVE 'Y' TO TRSM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DISTRIB YTD AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3700-ACCRUE-DISTRIB.
           IF TRSM-DISTRIB-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRSM-DISTRIB-YTD * TRSM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO TRSM-MARKET-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT MARKET AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3710-SPLIT-MARKET.
           COMPUTE WS-WORK-AMT = TRSM-MARKET-AMT / 12
           COMPUTE WS-WORK-AMT-2 = TRSM-MARKET-AMT - (WS-WORK-AMT * 12)
           COMPUTE TRST-PRINCIPAL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E511 MEMO REQUIRED
      * E535 DISTRIB AMT NEGATIVE
      * E987 DISTRIB AMT OVER 1000000
      * E330 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3720-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRST-MEMO = SPACES
                   MOVE 'E511' TO WS-REJECT-CODE
               WHEN TRST-DISTRIB-AMT < 0
                   MOVE 'E535' TO WS-REJECT-CODE
               WHEN TRST-DISTRIB-AMT > 1000000.00
                   MOVE 'E987' TO WS-REJECT-CODE
               WHEN TRST-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E330' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE INCOME YTD FEE ON THE DISTRIB AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3730-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN TRSM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO TRSM-INCOME-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRST-REVERSAL-FLAG = 'Y'
                   COMPUTE TRSM-INCOME-YTD ROUNDED = TRST-DISTRIB-AMT *
                       TRSM-DISCOUNT / 200
               WHEN TRSM-TRUST-TYPE-CD = 'FLJ'
                   MOVE 15.00 TO TRSM-INCOME-YTD
               WHEN TRSM-TRUST-TYPE-CD = 'MNG'
                   MOVE 5.00 TO TRSM-INCOME-YTD
               WHEN OTHER
                   COMPUTE TRSM-INCOME-YTD ROUNDED = TRST-DISTRIB-AMT *
                       TRSM-DISCOUNT / 100
           END-EVALUATE
           IF TRSM-INCOME-YTD > 0 AND TRSM-INCOME-YTD < 1.00
               MOVE 1.00 TO TRSM-INCOME-YTD
           END-IF
           IF TRSM-INCOME-YTD > 25.00
               MOVE 25.00 TO TRSM-INCOME-YTD
           END-IF
           ADD TRSM-INCOME-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TAX YTD IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3740-LIMIT-TAX.
           IF TRSM-TAX-YTD < 0.00
               MOVE 0.00 TO TRSM-TAX-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRSM-TAX-YTD > 1000.00
               MOVE 1000.00 TO TRSM-TAX-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3750-COUNT-SEQ.
           IF TRST-SEQ < 9999999
               ADD 1 TO TRST-SEQ
           END-IF
           IF TRST-SEQ NOT < 10
               MOVE 'Y' TO TRSM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE TAX AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3760-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRST-CHANNEL-CD
                   ADD TRSM-TAX-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRST-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRSM-TAX-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E946 REJECT CD REQUIRED
      * E772 DISTRIB YTD NEGATIVE
      * E258 DISTRIB YTD OVER 99999
      * E029 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3770-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRST-REJECT-CD = SPACES
                   MOVE 'E946' TO WS-REJECT-CODE
               WHEN TRSM-DISTRIB-YTD < 0
                   MOVE 'E772' TO WS-REJECT-CODE
               WHEN TRSM-DISTRIB-YTD > 99999.00
                   MOVE 'E258' TO WS-REJECT-CODE
               WHEN TRSM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E029' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE TAX TIER TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3780-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-TAX-TIER-RATE(1) TO TRSM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF WS-WORK-AMT NOT < WS-TAX-TIER-LOW(WS-TX)
                   MOVE WS-TAX-TIER-RATE(WS-TX) TO TRSM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRSM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRSM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DISTRIB BAND TIER FOR THE DISTRIB AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3790-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-DISTRIB-BAND-RATE(1) TO TRSM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF TRSM-DISTRIB-AMT NOT < WS-DISTRIB-BAND-LOW(WS-TX)
                   MOVE WS-DISTRIB-BAND-RATE(WS-TX) TO TRSM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRSM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRSM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3800-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF TRSM-LAST-PAY-DATE > 0 AND TRSM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRSM-PRINCIPAL-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD TRSM-PRINCIPAL-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TRSM-PRINCIPAL-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD TRSM-PRINCIPAL-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRSM-PRINCIPAL-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE INCOME YTD BY DIST FREQ CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3810-ACCUM-DIST-FREQ.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRSM-DIST-FREQ-CD
                   ADD TRSM-INCOME-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRSM-DIST-FREQ-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRSM-INCOME-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 90 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3820-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF TRST-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRST-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO TRSM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3830-DESCRIBE-CHANNEL.
           EVALUATE TRST-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PRINCIPAL AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3840-ACCRUE-PRINCIPAL.
           IF TRSM-PRINCIPAL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRSM-PRINCIPAL-AMT * TRSM-RATE / 100 / 365
               ADD WS-WORK-AMT TO TRSM-INCOME-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE TAX YTD FEE ON THE PRINCIPAL AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3850-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN TRST-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO TRSM-TAX-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-SUSPEND-FLAG = 'Y'
                   COMPUTE TRSM-TAX-YTD ROUNDED = TRST-PRINCIPAL-AMT *
                       TRSM-MARGIN / 200
               WHEN TRST-CHANNEL-CD = 'BRN'
                   MOVE 20.00 TO TRSM-TAX-YTD
               WHEN TRST-CHANNEL-CD = 'BAT'
                   MOVE 20.00 TO TRSM-TAX-YTD
               WHEN OTHER
                   COMPUTE TRSM-TAX-YTD ROUNDED = TRST-PRINCIPAL-AMT *
                       TRSM-MARGIN / 100
           END-EVALUATE
           IF TRSM-TAX-YTD > 0 AND TRSM-TAX-YTD < 10.00
               MOVE 10.00 TO TRSM-TAX-YTD
           END-IF
           IF TRSM-TAX-YTD > 25.00
               MOVE 25.00 TO TRSM-TAX-YTD
           END-IF
           ADD TRSM-TAX-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRINCIPAL AMT IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3860-LIMIT-PRINCIPAL.
           IF TRST-PRINCIPAL-AMT < 0.00
               MOVE 0.00 TO TRST-PRINCIPAL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRST-PRINCIPAL-AMT > 5000.00
               MOVE 5000.00 TO TRST-PRINCIPAL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PRINCIPAL FEE ON THE INCOME AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3870-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN TRSM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO TRSM-PRINCIPAL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRSM-SUSPEND-FLAG = 'Y'
                   COMPUTE TRSM-PRINCIPAL-AMT ROUNDED = TRST-INCOME-AMT
                       * TRSM-MARGIN / 200
               WHEN TRSM-TRUST-TYPE-CD = 'FLJ'
                   MOVE 20.00 TO TRSM-PRINCIPAL-AMT
               WHEN TRSM-TRUST-TYPE-CD = 'MNG'
                   MOVE 12.00 TO TRSM-PRINCIPAL-AMT
               WHEN OTHER
                   COMPUTE TRSM-PRINCIPAL-AMT ROUNDED = TRST-INCOME-AMT
                       * TRSM-MARGIN / 100
           END-EVALUATE
           IF TRSM-PRINCIPAL-AMT > 0 AND TRSM-PRINCIPAL-AMT < 2.00
               MOVE 2.00 TO TRSM-PRINCIPAL-AMT
           END-IF
           IF TRSM-PRINCIPAL-AMT > 25.00
               MOVE 25.00 TO TRSM-PRINCIPAL-AMT
           END-IF
           ADD TRSM-PRINCIPAL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3880-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF TRSM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO TRSM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE INCOME YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3890-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRSM-STATUS-CD
                   ADD TRSM-INCOME-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRSM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRSM-INCOME-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3900-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF TRSM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO TRSM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E625 CONTACT REQUIRED
      * E691 TRUSTEE FEE AMT NEGATIVE
      * E409 TRUSTEE FEE AMT OVER 250000
      * E051 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3910-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRSM-CONTACT = SPACES
                   MOVE 'E625' TO WS-REJECT-CODE
               WHEN TRSM-TRUSTEE-FEE-AMT < 0
                   MOVE 'E691' TO WS-REJECT-CODE
               WHEN TRSM-TRUSTEE-FEE-AMT > 250000.00
                   MOVE 'E409' TO WS-REJECT-CODE
               WHEN TRST-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E051' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3920-DESCRIBE-REGION.
           EVALUATE TRSM-REGION-CD
               WHEN 'A'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'D'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'N'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'T'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3930-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF TRSM-CLOSE-DATE > 0 AND TRSM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRST-TRUSTEE-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TRST-TRUSTEE-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TRST-TRUSTEE-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD TRST-TRUSTEE-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRST-TRUSTEE-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * EN -> FY WHEN OPEN DATE IS PAST
      * FY -> HP WHEN OPEN DATE IS PAST
      * HP -> JB WHEN OPEN DATE IS PAST
      * JB -> KZ WHEN OPEN DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3940-SET-CYCLE.
           EVALUATE TRSM-CYCLE-CD
               WHEN 'EN'
                   IF TRSM-OPEN-DATE > 0 AND TRSM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FY' TO TRSM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FY'
                   IF TRSM-OPEN-DATE > 0 AND TRSM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HP' TO TRSM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HP'
                   IF TRSM-OPEN-DATE > 0 AND TRSM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JB' TO TRSM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JB'
                   IF TRSM-OPEN-DATE > 0 AND TRSM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KZ' TO TRSM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * EH -> GG WHEN POST DATE IS PAST
      * GG -> JG WHEN POST DATE IS PAST
      * JG -> EH WHEN POST DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3950-SET-STATUS.
           EVALUATE TRSM-STATUS-CD
               WHEN 'EH'
                   IF TRST-POST-DATE > 0 AND TRST-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GG' TO TRSM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GG'
                   IF TRST-POST-DATE > 0 AND TRST-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JG' TO TRSM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JG'
                   IF TRST-POST-DATE > 0 AND TRST-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EH' TO TRSM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE TAX YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO TAX AMT.
      *----------------------------------------------------------------*
       3960-PRORATE-TAX.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRSM-TAX-AMT ROUNDED =
                   TRSM-TAX-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TRSM-TAX-AMT
           END-IF
           ADD TRSM-TAX-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * H -> M WHEN LAST MAINT DATE IS PAST
      * M -> T WHEN LAST MAINT DATE IS PAST
      * T -> H WHEN LAST MAINT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3970-SET-REASON.
           EVALUATE TRSM-REASON-CD
               WHEN 'H'
                   IF TRSM-LAST-MAINT-DATE > 0 AND TRSM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'M' TO TRSM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF TRSM-LAST-MAINT-DATE > 0 AND TRSM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'T' TO TRSM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF TRSM-LAST-MAINT-DATE > 0 AND TRSM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'H' TO TRSM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 45 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3980-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF TRSM-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO TRST-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-45, 46-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3990-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF TRSM-OPEN-DATE > 0 AND TRSM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRSM-MARKET-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD TRSM-MARKET-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD TRSM-MARKET-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TRSM-MARKET-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRSM-MARKET-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       4000-COUNT-CYCLE.
           IF TRSM-CYCLE-CNT < 9999999
               ADD 1 TO TRSM-CYCLE-CNT
           END-IF
           IF TRSM-CYCLE-CNT NOT < 3
               MOVE 'Y' TO TRSM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E396 REJECT CD REQUIRED
      * E432 TRUSTEE FEE AMT NEGATIVE
      * E310 TRUSTEE FEE AMT OVER 5000
      * E664 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       4010-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRST-REJECT-CD = SPACES
                   MOVE 'E396' TO WS-REJECT-CODE
               WHEN TRSM-TRUSTEE-FEE-AMT < 0
                   MOVE 'E432' TO WS-REJECT-CODE
               WHEN TRSM-TRUSTEE-FEE-AMT > 5000.00
                   MOVE 'E310' TO WS-REJECT-CODE
               WHEN TRSM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E664' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4020-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF TRSM-LAST-STMT-DATE > 0 AND TRSM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRST-INCOME-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TRST-INCOME-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD TRST-INCOME-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TRST-INCOME-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRST-INCOME-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * A -> D WHEN NOTICE DATE IS PAST
      * D -> N WHEN NOTICE DATE IS PAST
      * N -> T WHEN NOTICE DATE IS PAST
      * T -> A WHEN NOTICE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4030-SET-REGION.
           EVALUATE TRSM-REGION-CD
               WHEN 'A'
                   IF TRSM-NOTICE-DATE > 0 AND TRSM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO TRSM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF TRSM-NOTICE-DATE > 0 AND TRSM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'N' TO TRSM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'N'
                   IF TRSM-NOTICE-DATE > 0 AND TRSM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO TRSM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF TRSM-NOTICE-DATE > 0 AND TRSM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'A' TO TRSM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT PRINCIPAL YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4040-SPLIT-PRINCIPAL.
           COMPUTE WS-WORK-AMT = TRSM-PRINCIPAL-YTD / 2
           COMPUTE WS-WORK-AMT-2 = TRSM-PRINCIPAL-YTD - (WS-WORK-AMT *
               2)
           COMPUTE TRSM-DISTRIB-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN POST DATE IS PAST
      * BAT -> MAL WHEN POST DATE IS PAST
      * MAL -> TEL WHEN POST DATE IS PAST
      * TEL -> ONL WHEN POST DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4050-SET-CHANNEL.
           EVALUATE TRST-CHANNEL-CD
               WHEN 'BRN'
                   IF TRST-POST-DATE > 0 AND TRST-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO TRST-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF TRST-POST-DATE > 0 AND TRST-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO TRST-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF TRST-POST-DATE > 0 AND TRST-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO TRST-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF TRST-POST-DATE > 0 AND TRST-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO TRST-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4060-SPLIT-PRINCIPAL.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 4)
           COMPUTE TRSM-PRINCIPAL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PRINCIPAL AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4070-ACCRUE-PRINCIPAL.
           IF TRST-PRINCIPAL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRST-PRINCIPAL-AMT * TRSM-RATE / 100 / 360
               ADD WS-WORK-AMT TO TRST-TRUSTEE-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PRINCIPAL YTD BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4080-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRST-TRAN-CD
                   ADD TRSM-PRINCIPAL-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRST-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRSM-PRINCIPAL-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE TAX TIER TIER FOR THE DISTRIB YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4090-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-TAX-TIER-RATE(1) TO TRSM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF TRSM-DISTRIB-YTD NOT < WS-TAX-TIER-LOW(WS-TX)
                   MOVE WS-TAX-TIER-RATE(WS-TX) TO TRSM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRSM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRSM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRINCIPAL AMT IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4100-LIMIT-PRINCIPAL.
           IF TRSM-PRINCIPAL-AMT < 25.00
               MOVE 25.00 TO TRSM-PRINCIPAL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRSM-PRINCIPAL-AMT > 500.00
               MOVE 500.00 TO TRSM-PRINCIPAL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4110-DESCRIBE-STATUS.
           EVALUATE TRSM-STATUS-CD
               WHEN 'EH'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'GG'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'JG'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4120-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF TRSM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO TRSM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4130-COUNT-CYCLE.
           IF TRSM-CYCLE-CNT < 9999999
               ADD 1 TO TRSM-CYCLE-CNT
           END-IF
           IF TRSM-CYCLE-CNT NOT < 10
               MOVE 'Y' TO TRST-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4140-DESCRIBE-CYCLE.
           EVALUATE TRSM-CYCLE-CD
               WHEN 'EN'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'FY'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'HP'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'JB'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'KZ'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'LD'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'RY'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4150-FLAG-ITEM.
           IF WS-WORK-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DISTRIB YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO MARKET YTD.
      *----------------------------------------------------------------*
       4160-PRORATE-MARKET.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRSM-MARKET-YTD ROUNDED =
                   TRSM-DISTRIB-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TRSM-MARKET-YTD
           END-IF
           ADD TRSM-MARKET-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE PRINCIPAL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO TRUSTEE FEE YTD.
      *----------------------------------------------------------------*
       4170-PRORATE-TRUSTEE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRSM-TRUSTEE-FEE-YTD ROUNDED =
                   TRSM-PRINCIPAL-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TRSM-TRUSTEE-FEE-YTD
           END-IF
           ADD TRSM-TRUSTEE-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       4180-COUNT-CYCLE.
           IF TRSM-CYCLE-CNT < 9999999
               ADD 1 TO TRSM-CYCLE-CNT
           END-IF
           IF TRSM-CYCLE-CNT NOT < 5
               MOVE 'Y' TO TRSM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 10 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4190-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF TRSM-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO TRST-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E893 CONTACT REQUIRED
      * E939 MARKET YTD NEGATIVE
      * E625 MARKET YTD OVER 10000
      * E035 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       4200-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRSM-CONTACT = SPACES
                   MOVE 'E893' TO WS-REJECT-CODE
               WHEN TRSM-MARKET-YTD < 0
                   MOVE 'E939' TO WS-REJECT-CODE
               WHEN TRSM-MARKET-YTD > 10000.00
                   MOVE 'E625' TO WS-REJECT-CODE
               WHEN TRSM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E035' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 45 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4210-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF TRSM-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO TRSM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4220-COUNT-CYCLE.
           IF TRSM-CYCLE-CNT < 9999999
               ADD 1 TO TRSM-CYCLE-CNT
           END-IF
           IF TRSM-CYCLE-CNT NOT < 12
               MOVE 'Y' TO TRST-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 180 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4230-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF TRST-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRST-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO TRSM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4240-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF TRSM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO TRSM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4250-DESCRIBE-TRAN.
           EVALUATE TRST-TRAN-CD
               WHEN 'IN'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'DS'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'FE'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'RB'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'TX'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 10 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4260-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF TRSM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO TRSM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INCOME AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4270-ACCRUE-INCOME.
           IF TRSM-INCOME-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRSM-INCOME-AMT * TRSM-RATE / 100 / 360
               ADD WS-WORK-AMT TO TRSM-TRUSTEE-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON TAX YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4280-ACCRUE-TAX.
           IF TRSM-TAX-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRSM-TAX-YTD * TRSM-RATE / 100 / 360
               ADD WS-WORK-AMT TO TRSM-DISTRIB-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4290-DESCRIBE-REGION.
           EVALUATE TRSM-REGION-CD
               WHEN 'A'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'D'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'N'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'T'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT MARKET AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4300-SPLIT-MARKET.
           COMPUTE WS-WORK-AMT = TRSM-MARKET-AMT / 4
           COMPUTE WS-WORK-AMT-2 = TRSM-MARKET-AMT - (WS-WORK-AMT * 4)
           COMPUTE TRSM-TRUSTEE-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       4310-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF TRSM-LAST-STMT-DATE > 0 AND TRSM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRSM-INCOME-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TRSM-INCOME-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD TRSM-INCOME-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD TRSM-INCOME-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRSM-INCOME-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4320-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF TRSM-EXPIRY-DATE > 0 AND TRSM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRSM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRST-TRUSTEE-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD TRST-TRUSTEE-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD TRST-TRUSTEE-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TRST-TRUSTEE-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRST-TRUSTEE-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE TRS FIELDS TO TRPEN AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7010-CALL-TRPEN.
           INITIALIZE TRPEN-PARMS
           MOVE TRSM-DISTRIB-YTD
             TO TRPEN-DISTRIB-YTD
           MOVE TRSM-TRUSTEE-FEE-AMT
             TO TRPEN-TRUSTEE-FEE-AMT
           MOVE TRSM-INCOME-AMT
             TO TRPEN-INCOME-AMT
           MOVE TRSM-INCOME-YTD
             TO TRPEN-INCOME-YTD
           MOVE TRSM-TAX-AMT
             TO TRPEN-TAX-AMT
           MOVE TRSM-MARKET-YTD
             TO TRPEN-MARKET-YTD
           MOVE TRSM-CLOSE-DATE
             TO TRPEN-CLOSE-DATE
           MOVE TRSM-NOTICE-DATE
             TO TRPEN-NOTICE-DATE
           MOVE TRSM-LAST-STMT-DATE
             TO TRPEN-LAST-STMT-DATE
           MOVE TRSM-OPEN-DATE
             TO TRPEN-OPEN-DATE
           MOVE TRSM-REGION-CD
             TO TRPEN-REGION-CD
           MOVE TRSM-CYCLE-CD
             TO TRPEN-CYCLE-CD
           MOVE TRSM-STATUS-CD
             TO TRPEN-STATUS-CD
           MOVE TRSM-TRUST-TYPE-CD
             TO TRPEN-TRUST-TYPE-CD
           MOVE TRSM-SUSPEND-FLAG
             TO TRPEN-SUSPEND-FLAG
           MOVE TRSM-EXEMPT-FLAG
             TO TRPEN-EXEMPT-FLAG
           MOVE TRSM-PREFERRED-FLAG
             TO TRPEN-PREFERRED-FLAG
           MOVE TRSM-RETRY-CNT
             TO TRPEN-RETRY-CNT
           MOVE TRSM-PAYMENT-CNT
             TO TRPEN-PAYMENT-CNT
           MOVE TRSM-DISCOUNT
             TO TRPEN-DISCOUNT
           MOVE TRSM-RATE
             TO TRPEN-RATE
           MOVE TRSM-ADDR
             TO TRPEN-ADDR
           MOVE TRSM-REF-NO
             TO TRPEN-REF-NO
           CALL 'TRPEN' USING TRPEN-PARMS
           EVALUATE TRPEN-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE TRPEN-TRUSTEE-FEE-AMT
                     TO TRSM-TRUSTEE-FEE-AMT
                   MOVE TRPEN-INCOME-AMT
                     TO TRSM-INCOME-AMT
                   MOVE TRPEN-TAX-AMT
                     TO TRSM-TAX-AMT
                   MOVE TRPEN-MARKET-YTD
                     TO TRSM-MARKET-YTD
                   MOVE TRPEN-SUSPEND-FLAG
                     TO TRSM-SUSPEND-FLAG
                   MOVE TRPEN-EXEMPT-FLAG
                     TO TRSM-EXEMPT-FLAG
                   MOVE TRPEN-PREFERRED-FLAG
                     TO TRSM-PREFERRED-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RPEN' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-TRPEN' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE TRST-KEY TO RD-KEY
           MOVE TRST-TRAN-CD TO RD-CODE
           MOVE TRST-INCOME-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE TRST-KEY TO RD-KEY
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
           DISPLAY 'TRSMTHE READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'TRSMTHE' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
