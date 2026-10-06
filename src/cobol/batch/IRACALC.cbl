      *================================================================*
      * PROGRAM  : IRACALC                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : IRA CUSTODY - PERIODIC CALCULATION. READS THE IRA   *
      *            TRANSACTIONS, UPDATES THE IRA MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. INDIVIDUAL RETIREMENT ACCOUNT   *
      *            CONTRIBUTIONS, DISTRIBUTIONS AND 5498               *
      * INPUT    : IRAMST (KSDS), IRATRN, BANKCTL                      *
      * OUTPUT   : IRAOUT, IRACARPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, IRVAL, IRPRC, IRELG     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-05-12 JLM  ORIGINAL                                       *
      * 2004-10-27 LMB  FMV CALCULATION CORRECTED INC9600              *
      * 2006-04-18 EWS  PERFORMANCE - RMD READ AHEAD PRB0851           *
      * 2011-12-26 LMB  ROLLOVER STATUS CODES ADDED REQ0856            *
      * 2011-07-09 GAF  NEW EXCESS REPORT COLUMNS PRB2085              *
      * 2011-07-22 SKN  FMV EXCEPTIONS TO OPS QUEUE REQ5409            *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. IRACALC.
       AUTHOR. R J TRAVERS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "IRAMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS IRAM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "IRATRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "IRAOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "IRACARPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY IRAMST.
       FD  TRAN-FILE.
       COPY IRATRN.
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
      * WS-FMV-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FMV-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000857000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000164000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002600000000033500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012600000000685000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012700000000490000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022700000000256500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025200000000894000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026200000000599500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00028700000000057000'.
       01  WS-FMV-TIER-TBL REDEFINES WS-FMV-TIER-VALUES.
           05  WS-FMV-TIER-ROW OCCURS 9 TIMES.
               10  WS-FMV-TIER-LOW         PIC 9(09)V99.
               10  WS-FMV-TIER-RATE        PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-ROLLOVER-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-ROLLOVER-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000092500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000644000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001500000000857000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001550000000388000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002050000000355500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012050000000416500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012550000000234000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012600000000543500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012700000000547000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012800000000672000'.
       01  WS-ROLLOVER-TIER-TBL REDEFINES WS-ROLLOVER-TIER-VALUES.
           05  WS-ROLLOVER-TIER-ROW OCCURS 10 TIMES.
               10  WS-ROLLOVER-TIER-LOW    PIC 9(09)V99.
               10  WS-ROLLOVER-TIER-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-DISTRIB-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-DISTRIB-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000284000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000662000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010050000000196500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011050000000516500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021050000000259000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023550000000218500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00033550000000640000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00033650000000756500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00038650000000226000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00039650000000241500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00039750000000069500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00044750000000569500'.
       01  WS-DISTRIB-GRID-TBL REDEFINES WS-DISTRIB-GRID-VALUES.
           05  WS-DISTRIB-GRID-ROW OCCURS 12 TIMES.
               10  WS-DISTRIB-GRID-LOW     PIC 9(09)V99.
               10  WS-DISTRIB-GRID-RATE    PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  IRACALC  IRA CUSTODY'.
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
       COPY IRVALP.
       COPY IRPRCP.
       COPY IRELGP.

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
               MOVE 'IRAMST' TO ABND-FILE-NAME
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
           MOVE IRAT-KEY TO IRAM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-SET-REGION
               PERFORM 3010-DESCRIBE-IRA-TYPE
               PERFORM 3020-COUNT-RETRY
               PERFORM 3030-SELECT-RATE
               PERFORM 3040-COUNT-RETRY
               PERFORM 3050-LIMIT-FMV
               PERFORM 3060-FLAG-ROLLOVER
               PERFORM 3070-AGE-EXPIRY
               PERFORM 3080-COUNT-SEQ
               PERFORM 3090-SELECT-FEE-PCT
               PERFORM 3100-SPLIT-FMV
               PERFORM 3110-COUNT-PAYMENT
               PERFORM 3120-EDIT-REF-NO
               PERFORM 3130-ASSESS-HOLD
               PERFORM 3140-ASSESS-REVERSAL
               PERFORM 3150-LIMIT-DISTRIB
               PERFORM 3160-AGE-LAST-STMT
               PERFORM 3170-COUNT-PAYMENT
               PERFORM 3180-COUNT-RETRY
               PERFORM 3190-COUNT-RETRY
               PERFORM 3200-FLAG-ITEM
               PERFORM 3210-COUNT-SEQ
               PERFORM 3220-ACCRUE-MARGIN
               PERFORM 3230-PRORATE-DISTRIB
               PERFORM 3240-CHECK-NOTICE
               PERFORM 3250-COUNT-CYCLE
               PERFORM 3260-PRORATE-DISTRIB
               PERFORM 3270-LIMIT-RMD
               PERFORM 3280-SET-IRA-TYPE
               PERFORM 3290-SELECT-MARGIN
               PERFORM 3300-ACCUM-REGION
               PERFORM 3310-FLAG-FMV
               PERFORM 3320-LIMIT-FMV
               PERFORM 3330-AGE-MATURITY
               PERFORM 3340-LIMIT-FMV
               PERFORM 3350-FLAG-EXCESS
               PERFORM 3360-SPLIT-WITHHOLD
               PERFORM 3370-LIMIT-FMV
               PERFORM 3380-SPLIT-FMV
               PERFORM 3390-ASSESS-AUTO-RENEW
               PERFORM 3400-COUNT-PAYMENT
               PERFORM 3410-AGE-NEXT-REVIEW
               PERFORM 3420-LIMIT-ITEM
               PERFORM 3430-AGE-EFF
               PERFORM 3440-ACCUM-CHANNEL
               PERFORM 3450-ASSESS-STOP
               PERFORM 3460-SET-TYPE
               PERFORM 3470-SET-CHANNEL
               PERFORM 3480-SELECT-RATE
               PERFORM 3490-PRORATE-DISTRIB
               PERFORM 3500-CHECK-NOTICE
               PERFORM 3510-EDIT-MEMO
               PERFORM 3520-DESCRIBE-CHANNEL
               PERFORM 3530-SELECT-RATE
               PERFORM 3540-FLAG-DISTRIB
               PERFORM 3550-PRORATE-RMD
               PERFORM 3560-CHECK-LAST-ACT
               PERFORM 3570-COUNT-CYCLE
               PERFORM 3580-AGE-EFFECTIVE
               PERFORM 3590-SET-IRA-TYPE
               PERFORM 3600-COUNT-SEQ
               PERFORM 3610-SELECT-RATE
               PERFORM 3620-CHECK-NOTICE
               PERFORM 3630-PRORATE-RMD
               PERFORM 3640-LIMIT-EXCESS
               PERFORM 3650-CHECK-OPEN
               PERFORM 3660-PRORATE-CONTRIB
               PERFORM 3670-LIMIT-CONTRIB
               PERFORM 3680-AGE-EFF
               PERFORM 3690-COUNT-ITEM
               PERFORM 3700-ACCRUE-RMD
               PERFORM 3710-SELECT-MARGIN
               PERFORM 3720-ACCRUE-DISTRIB
               PERFORM 3730-DESCRIBE-REGION
               PERFORM 3740-SELECT-RATE
               PERFORM 3750-SET-REGION
               PERFORM 3760-AGE-OPEN
               PERFORM 3770-SPLIT-FMV
               PERFORM 3780-AGE-POST
               PERFORM 3790-DESCRIBE-TYPE
               PERFORM 3800-PRORATE-ROLLOVER
               PERFORM 3810-SET-TRAN
               PERFORM 3820-SPLIT-ROLLOVER
               PERFORM 3830-ASSESS-HOLD
               PERFORM 3840-LIMIT-RMD
               PERFORM 3850-AGE-LAST-STMT
               PERFORM 3860-DESCRIBE-IRA-TYPE
               PERFORM 3870-LIMIT-RMD
               PERFORM 3880-ASSESS-STOP
               PERFORM 3890-SELECT-FEE-PCT
               PERFORM 3900-ACCUM-TRAN
               PERFORM 3910-AGE-LAST-ACT
               PERFORM 3920-DESCRIBE-REGION
               PERFORM 3930-COUNT-ITEM
               PERFORM 3940-SET-TYPE
               PERFORM 3950-SPLIT-ROLLOVER
               PERFORM 3960-LIMIT-DISTRIB
               PERFORM 3970-CHECK-NOTICE
               PERFORM 3980-ACCRUE-EXCESS
               PERFORM 3990-PRORATE-DISTRIB
               PERFORM 4000-SET-IRA-TYPE
               PERFORM 4010-ACCUM-DIST-CODE
               PERFORM 4020-FLAG-FMV
               PERFORM 4030-FLAG-ROLLOVER
               PERFORM 4040-CHECK-NEXT-REVIEW
               PERFORM 4050-ACCUM-TRAN
               PERFORM 4060-FLAG-WITHHOLD
               PERFORM 4070-ACCUM-TYPE
               PERFORM 4080-ACCRUE-DISTRIB
               PERFORM 4090-PRORATE-FMV
               PERFORM 4100-DESCRIBE-REGION
               PERFORM 4110-AGE-EFF
               PERFORM 4120-ACCRUE-RMD
               PERFORM 4130-SPLIT-WITHHOLD
               PERFORM 4140-DESCRIBE-DIST-CODE
               PERFORM 4150-PRORATE-RMD
               PERFORM 4160-FLAG-WITHHOLD
               PERFORM 4170-LIMIT-EXCESS
               PERFORM 4180-ACCRUE-RMD
               PERFORM 4190-SPLIT-DISTRIB
               PERFORM 4200-FLAG-RMD
               PERFORM 4210-DESCRIBE-TYPE
               PERFORM 4220-COUNT-RETRY
               PERFORM 4230-SET-DIST-CODE
               PERFORM 4240-FLAG-EXCESS
               PERFORM 4250-ACCRUE-DISTRIB
               PERFORM 4260-ACCUM-CHANNEL
               PERFORM 4270-SELECT-FEE-PCT
               PERFORM 4280-LIMIT-CONTRIB
               PERFORM 4290-PRORATE-EXCESS
               PERFORM 4300-FLAG-RMD
               PERFORM 4310-SPLIT-ROLLOVER
               PERFORM 4320-FLAG-ITEM
               PERFORM 4330-SET-REGION
               PERFORM 4340-PRORATE-RMD
               PERFORM 4350-SET-TYPE
               PERFORM 4360-COUNT-PAYMENT
               PERFORM 4370-SELECT-FEE-PCT
               PERFORM 4380-DESCRIBE-TRAN
               PERFORM 4390-ASSESS-REVIEW
               PERFORM 7010-CALL-IRVAL
               PERFORM 7000-CALL-IRPRC
               PERFORM 7020-CALL-IRELG
               REWRITE IRA-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'IRAMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM IRA-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * CE -> FL WHEN MATURITY DATE IS PAST
      * FL -> GY WHEN MATURITY DATE IS PAST
      * GY -> HU WHEN MATURITY DATE IS PAST
      * HU -> KA WHEN MATURITY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3000-SET-REGION.
           EVALUATE IRAM-REGION-CD
               WHEN 'CE'
                   IF IRAM-MATURITY-DATE > 0 AND IRAM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FL' TO IRAM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FL'
                   IF IRAM-MATURITY-DATE > 0 AND IRAM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GY' TO IRAM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GY'
                   IF IRAM-MATURITY-DATE > 0 AND IRAM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HU' TO IRAM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HU'
                   IF IRAM-MATURITY-DATE > 0 AND IRAM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KA' TO IRAM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE IRA TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3010-DESCRIBE-IRA-TYPE.
           EVALUATE IRAM-IRA-TYPE-CD
               WHEN 'F'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'V'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'W'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'Y'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3020-COUNT-RETRY.
           IF IRAM-RETRY-CNT < 9999999
               ADD 1 TO IRAM-RETRY-CNT
           END-IF
           IF IRAM-RETRY-CNT NOT < 3
               MOVE 'Y' TO IRAM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FMV TIER TIER FOR THE EXCESS YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3030-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-FMV-TIER-RATE(1) TO IRAM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF IRAM-EXCESS-YTD NOT < WS-FMV-TIER-LOW(WS-TX)
                   MOVE WS-FMV-TIER-RATE(WS-TX) TO IRAM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF IRAM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO IRAM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3040-COUNT-RETRY.
           IF IRAM-RETRY-CNT < 9999999
               ADD 1 TO IRAM-RETRY-CNT
           END-IF
           IF IRAM-RETRY-CNT NOT < 5
               MOVE 'Y' TO IRAM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FMV AMT IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3050-LIMIT-FMV.
           IF IRAM-FMV-AMT < 25.00
               MOVE 25.00 TO IRAM-FMV-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF IRAM-FMV-AMT > 50000.00
               MOVE 50000.00 TO IRAM-FMV-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ROLLOVER AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3060-FLAG-ROLLOVER.
           IF IRAT-ROLLOVER-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE IRAT-ROLLOVER-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3070-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF IRAM-EXPIRY-DATE > 0 AND IRAM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD IRAM-EXCESS-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD IRAM-EXCESS-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD IRAM-EXCESS-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD IRAM-EXCESS-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD IRAM-EXCESS-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3080-COUNT-SEQ.
           IF IRAT-SEQ < 9999999
               ADD 1 TO IRAT-SEQ
           END-IF
           IF IRAT-SEQ NOT < 12
               MOVE 'Y' TO IRAM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ROLLOVER TIER TIER FOR THE RMD YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3090-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ROLLOVER-TIER-RATE(1) TO IRAM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF IRAM-RMD-YTD NOT < WS-ROLLOVER-TIER-LOW(WS-TX)
                   MOVE WS-ROLLOVER-TIER-RATE(WS-TX) TO IRAM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF IRAM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO IRAM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FMV YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3100-SPLIT-FMV.
           COMPUTE WS-WORK-AMT = IRAM-FMV-YTD / 3
           COMPUTE WS-WORK-AMT-2 = IRAM-FMV-YTD - (WS-WORK-AMT * 3)
           COMPUTE IRAM-EXCESS-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3110-COUNT-PAYMENT.
           IF IRAM-PAYMENT-CNT < 9999999
               ADD 1 TO IRAM-PAYMENT-CNT
           END-IF
           IF IRAM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO IRAM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E074 REF NO REQUIRED
      * E493 EXCESS YTD NEGATIVE
      * E299 EXCESS YTD OVER 1000000
      * E810 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3120-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN IRAM-REF-NO = SPACES
                   MOVE 'E074' TO WS-REJECT-CODE
               WHEN IRAM-EXCESS-YTD < 0
                   MOVE 'E493' TO WS-REJECT-CODE
               WHEN IRAM-EXCESS-YTD > 1000000.00
                   MOVE 'E299' TO WS-REJECT-CODE
               WHEN IRAM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E810' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ROLLOVER YTD FEE ON THE WITHHOLD AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3130-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN IRAM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO IRAM-ROLLOVER-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN IRAT-REVERSAL-FLAG = 'Y'
                   COMPUTE IRAM-ROLLOVER-YTD ROUNDED = IRAM-WITHHOLD-AMT
                       * IRAM-FEE-PCT / 200
               WHEN IRAM-TYPE-CD = 'EA'
                   MOVE 10.00 TO IRAM-ROLLOVER-YTD
               WHEN IRAM-TYPE-CD = 'HR'
                   MOVE 15.00 TO IRAM-ROLLOVER-YTD
               WHEN OTHER
                   COMPUTE IRAM-ROLLOVER-YTD ROUNDED = IRAM-WITHHOLD-AMT
                       * IRAM-FEE-PCT / 100
           END-EVALUATE
           IF IRAM-ROLLOVER-YTD > 0 AND IRAM-ROLLOVER-YTD < 2.00
               MOVE 2.00 TO IRAM-ROLLOVER-YTD
           END-IF
           IF IRAM-ROLLOVER-YTD > 25.00
               MOVE 25.00 TO IRAM-ROLLOVER-YTD
           END-IF
           ADD IRAM-ROLLOVER-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE DISTRIB FEE ON THE DISTRIB YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3140-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN IRAT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO IRAM-DISTRIB-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN IRAM-REVIEW-FLAG = 'Y'
                   COMPUTE IRAM-DISTRIB-AMT ROUNDED = IRAM-DISTRIB-YTD *
                       IRAM-FEE-PCT / 200
               WHEN IRAM-IRA-TYPE-CD = 'F'
                   MOVE 12.00 TO IRAM-DISTRIB-AMT
               WHEN IRAM-IRA-TYPE-CD = 'V'
                   MOVE 12.00 TO IRAM-DISTRIB-AMT
               WHEN OTHER
                   COMPUTE IRAM-DISTRIB-AMT ROUNDED = IRAM-DISTRIB-YTD *
                       IRAM-FEE-PCT / 100
           END-EVALUATE
           IF IRAM-DISTRIB-AMT > 0 AND IRAM-DISTRIB-AMT < 10.00
               MOVE 10.00 TO IRAM-DISTRIB-AMT
           END-IF
           IF IRAM-DISTRIB-AMT > 50.00
               MOVE 50.00 TO IRAM-DISTRIB-AMT
           END-IF
           ADD IRAM-DISTRIB-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DISTRIB YTD IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3150-LIMIT-DISTRIB.
           IF IRAM-DISTRIB-YTD < 25.00
               MOVE 25.00 TO IRAM-DISTRIB-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF IRAM-DISTRIB-YTD > 2500.00
               MOVE 2500.00 TO IRAM-DISTRIB-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3160-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF IRAM-LAST-STMT-DATE > 0 AND IRAM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD IRAM-DISTRIB-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD IRAM-DISTRIB-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD IRAM-DISTRIB-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD IRAM-DISTRIB-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD IRAM-DISTRIB-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3170-COUNT-PAYMENT.
           IF IRAM-PAYMENT-CNT < 9999999
               ADD 1 TO IRAM-PAYMENT-CNT
           END-IF
           IF IRAM-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO IRAM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3180-COUNT-RETRY.
           IF IRAM-RETRY-CNT < 9999999
               ADD 1 TO IRAM-RETRY-CNT
           END-IF
           IF IRAM-RETRY-CNT NOT < 12
               MOVE 'Y' TO IRAM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3190-COUNT-RETRY.
           IF IRAM-RETRY-CNT < 9999999
               ADD 1 TO IRAM-RETRY-CNT
           END-IF
           IF IRAM-RETRY-CNT NOT < 12
               MOVE 'Y' TO IRAM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3200-FLAG-ITEM.
           IF WS-WORK-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3210-COUNT-SEQ.
           IF IRAT-SEQ < 9999999
               ADD 1 TO IRAT-SEQ
           END-IF
           IF IRAT-SEQ NOT < 10
               MOVE 'Y' TO IRAM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3220-ACCRUE-MARGIN.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * IRAM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO IRAM-DISTRIB-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CONTRIB YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO DISTRIB AMT.
      *----------------------------------------------------------------*
       3230-PRORATE-DISTRIB.
           IF WS-PERIOD-DAYS > 0
               COMPUTE IRAT-DISTRIB-AMT ROUNDED =
                   IRAM-CONTRIB-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO IRAT-DISTRIB-AMT
           END-IF
           ADD IRAT-DISTRIB-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3240-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF IRAM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO IRAM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3250-COUNT-CYCLE.
           IF IRAM-CYCLE-CNT < 9999999
               ADD 1 TO IRAM-CYCLE-CNT
           END-IF
           IF IRAM-CYCLE-CNT NOT < 12
               MOVE 'Y' TO IRAM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WITHHOLD AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO DISTRIB YTD.
      *----------------------------------------------------------------*
       3260-PRORATE-DISTRIB.
           IF WS-PERIOD-DAYS > 0
               COMPUTE IRAM-DISTRIB-YTD ROUNDED =
                   IRAM-WITHHOLD-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO IRAM-DISTRIB-YTD
           END-IF
           ADD IRAM-DISTRIB-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * RMD AMT IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3270-LIMIT-RMD.
           IF IRAM-RMD-AMT < 50.00
               MOVE 50.00 TO IRAM-RMD-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF IRAM-RMD-AMT > 5000.00
               MOVE 5000.00 TO IRAM-RMD-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR IRA TYPE CD:
      * F -> V WHEN NEXT DUE DATE IS PAST
      * V -> W WHEN NEXT DUE DATE IS PAST
      * W -> Y WHEN NEXT DUE DATE IS PAST
      * Y -> F WHEN NEXT DUE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3280-SET-IRA-TYPE.
           EVALUATE IRAM-IRA-TYPE-CD
               WHEN 'F'
                   IF IRAM-NEXT-DUE-DATE > 0 AND IRAM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'V' TO IRAM-IRA-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'V'
                   IF IRAM-NEXT-DUE-DATE > 0 AND IRAM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO IRAM-IRA-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'W'
                   IF IRAM-NEXT-DUE-DATE > 0 AND IRAM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'Y' TO IRAM-IRA-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'Y'
                   IF IRAM-NEXT-DUE-DATE > 0 AND IRAM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'F' TO IRAM-IRA-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE ROLLOVER TIER TIER FOR THE RMD YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3290-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-ROLLOVER-TIER-RATE(1) TO IRAM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF IRAM-RMD-YTD NOT < WS-ROLLOVER-TIER-LOW(WS-TX)
                   MOVE WS-ROLLOVER-TIER-RATE(WS-TX) TO IRAM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF IRAM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO IRAM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE DISTRIB AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3300-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRAM-REGION-CD
                   ADD IRAT-DISTRIB-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRAM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE IRAT-DISTRIB-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FMV YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3310-FLAG-FMV.
           IF IRAM-FMV-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE IRAM-FMV-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FMV AMT IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3320-LIMIT-FMV.
           IF IRAM-FMV-AMT < 0.00
               MOVE 0.00 TO IRAM-FMV-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF IRAM-FMV-AMT > 50000.00
               MOVE 50000.00 TO IRAM-FMV-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3330-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF IRAM-MATURITY-DATE > 0 AND IRAM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD IRAM-WITHHOLD-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD IRAM-WITHHOLD-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD IRAM-WITHHOLD-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD IRAM-WITHHOLD-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD IRAM-WITHHOLD-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FMV AMT IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3340-LIMIT-FMV.
           IF IRAM-FMV-AMT < 100.00
               MOVE 100.00 TO IRAM-FMV-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF IRAM-FMV-AMT > 2500.00
               MOVE 2500.00 TO IRAM-FMV-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCESS AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3350-FLAG-EXCESS.
           IF IRAT-EXCESS-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE IRAT-EXCESS-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WITHHOLD YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3360-SPLIT-WITHHOLD.
           COMPUTE WS-WORK-AMT = IRAM-WITHHOLD-YTD / 12
           COMPUTE WS-WORK-AMT-2 = IRAM-WITHHOLD-YTD - (WS-WORK-AMT *
               12)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FMV AMT IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3370-LIMIT-FMV.
           IF IRAM-FMV-AMT < 25.00
               MOVE 25.00 TO IRAM-FMV-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF IRAM-FMV-AMT > 5000.00
               MOVE 5000.00 TO IRAM-FMV-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FMV YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3380-SPLIT-FMV.
           COMPUTE WS-WORK-AMT = IRAM-FMV-YTD / 12
           COMPUTE WS-WORK-AMT-2 = IRAM-FMV-YTD - (WS-WORK-AMT * 12)
           COMPUTE IRAM-DISTRIB-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE ROLLOVER AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3390-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN IRAM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN IRAM-REVIEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = IRAM-ROLLOVER-AMT *
                       IRAM-FEE-PCT / 200
               WHEN IRAM-TYPE-CD = 'EA'
                   MOVE 12.00 TO WS-WORK-AMT
               WHEN IRAM-TYPE-CD = 'HR'
                   MOVE 20.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = IRAM-ROLLOVER-AMT *
                       IRAM-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 250.00
               MOVE 250.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3400-COUNT-PAYMENT.
           IF IRAM-PAYMENT-CNT < 9999999
               ADD 1 TO IRAM-PAYMENT-CNT
           END-IF
           IF IRAM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO IRAM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3410-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF IRAM-NEXT-REVIEW-DATE > 0 AND IRAM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD IRAM-FMV-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD IRAM-FMV-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD IRAM-FMV-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD IRAM-FMV-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD IRAM-FMV-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3420-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 25.00
               MOVE 25.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 50000.00
               MOVE 50000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3430-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF IRAT-EFF-DATE > 0 AND IRAT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD IRAM-ROLLOVER-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD IRAM-ROLLOVER-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD IRAM-ROLLOVER-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD IRAM-ROLLOVER-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD IRAM-ROLLOVER-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE CONTRIB AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3440-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRAM-CHANNEL-CD
                   ADD IRAM-CONTRIB-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRAM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE IRAM-CONTRIB-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DISTRIB FEE ON THE EXCESS YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3450-ASSESS-STOP.
           EVALUATE TRUE
               WHEN IRAM-STOP-FLAG = 'Y'
                   MOVE ZERO TO IRAT-DISTRIB-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN IRAM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE IRAT-DISTRIB-AMT ROUNDED = IRAM-EXCESS-YTD *
                       IRAM-FEE-PCT / 200
               WHEN IRAM-IRA-TYPE-CD = 'F'
                   MOVE 3.00 TO IRAT-DISTRIB-AMT
               WHEN IRAM-IRA-TYPE-CD = 'V'
                   MOVE 12.00 TO IRAT-DISTRIB-AMT
               WHEN OTHER
                   COMPUTE IRAT-DISTRIB-AMT ROUNDED = IRAM-EXCESS-YTD *
                       IRAM-FEE-PCT / 100
           END-EVALUATE
           IF IRAT-DISTRIB-AMT > 0 AND IRAT-DISTRIB-AMT < 5.00
               MOVE 5.00 TO IRAT-DISTRIB-AMT
           END-IF
           IF IRAT-DISTRIB-AMT > 75.00
               MOVE 75.00 TO IRAT-DISTRIB-AMT
           END-IF
           ADD IRAT-DISTRIB-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * EA -> HR WHEN EXPIRY DATE IS PAST
      * HR -> XZ WHEN EXPIRY DATE IS PAST
      * XZ -> EA WHEN EXPIRY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3460-SET-TYPE.
           EVALUATE IRAM-TYPE-CD
               WHEN 'EA'
                   IF IRAM-EXPIRY-DATE > 0 AND IRAM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HR' TO IRAM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HR'
                   IF IRAM-EXPIRY-DATE > 0 AND IRAM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XZ' TO IRAM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XZ'
                   IF IRAM-EXPIRY-DATE > 0 AND IRAM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EA' TO IRAM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * AS -> AY WHEN POST DATE IS PAST
      * AY -> JR WHEN POST DATE IS PAST
      * JR -> TA WHEN POST DATE IS PAST
      * TA -> UM WHEN POST DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3470-SET-CHANNEL.
           EVALUATE IRAM-CHANNEL-CD
               WHEN 'AS'
                   IF IRAT-POST-DATE > 0 AND IRAT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AY' TO IRAM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AY'
                   IF IRAT-POST-DATE > 0 AND IRAT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JR' TO IRAM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JR'
                   IF IRAT-POST-DATE > 0 AND IRAT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TA' TO IRAM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TA'
                   IF IRAT-POST-DATE > 0 AND IRAT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UM' TO IRAM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE DISTRIB GRID TIER FOR THE DISTRIB YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3480-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-DISTRIB-GRID-RATE(1) TO IRAM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF IRAM-DISTRIB-YTD NOT < WS-DISTRIB-GRID-LOW(WS-TX)
                   MOVE WS-DISTRIB-GRID-RATE(WS-TX) TO IRAM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF IRAM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO IRAM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE RMD YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO DISTRIB YTD.
      *----------------------------------------------------------------*
       3490-PRORATE-DISTRIB.
           IF WS-PERIOD-DAYS > 0
               COMPUTE IRAM-DISTRIB-YTD ROUNDED =
                   IRAM-RMD-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO IRAM-DISTRIB-YTD
           END-IF
           ADD IRAM-DISTRIB-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3500-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF IRAM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO IRAT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E812 MEMO REQUIRED
      * E126 ROLLOVER AMT NEGATIVE
      * E948 ROLLOVER AMT OVER 10000
      * E916 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3510-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN IRAT-MEMO = SPACES
                   MOVE 'E812' TO WS-REJECT-CODE
               WHEN IRAT-ROLLOVER-AMT < 0
                   MOVE 'E126' TO WS-REJECT-CODE
               WHEN IRAT-ROLLOVER-AMT > 10000.00
                   MOVE 'E948' TO WS-REJECT-CODE
               WHEN IRAM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E916' TO WS-REJECT-CODE
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
       3520-DESCRIBE-CHANNEL.
           EVALUATE IRAT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE FMV TIER TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3530-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-FMV-TIER-RATE(1) TO IRAM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF WS-WORK-AMT NOT < WS-FMV-TIER-LOW(WS-TX)
                   MOVE WS-FMV-TIER-RATE(WS-TX) TO IRAM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF IRAM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO IRAM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DISTRIB AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3540-FLAG-DISTRIB.
           IF IRAM-DISTRIB-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE IRAM-DISTRIB-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE RMD YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3550-PRORATE-RMD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   IRAM-RMD-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 10 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3560-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF IRAM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO IRAM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3570-COUNT-CYCLE.
           IF IRAM-CYCLE-CNT < 9999999
               ADD 1 TO IRAM-CYCLE-CNT
           END-IF
           IF IRAM-CYCLE-CNT NOT < 5
               MOVE 'Y' TO IRAM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3580-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF IRAM-EFFECTIVE-DATE > 0 AND IRAM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD IRAM-DISTRIB-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD IRAM-DISTRIB-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD IRAM-DISTRIB-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD IRAM-DISTRIB-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD IRAM-DISTRIB-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR IRA TYPE CD:
      * F -> V WHEN EFF DATE IS PAST
      * V -> W WHEN EFF DATE IS PAST
      * W -> Y WHEN EFF DATE IS PAST
      * Y -> F WHEN EFF DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3590-SET-IRA-TYPE.
           EVALUATE IRAM-IRA-TYPE-CD
               WHEN 'F'
                   IF IRAT-EFF-DATE > 0 AND IRAT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'V' TO IRAM-IRA-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'V'
                   IF IRAT-EFF-DATE > 0 AND IRAT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO IRAM-IRA-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'W'
                   IF IRAT-EFF-DATE > 0 AND IRAT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'Y' TO IRAM-IRA-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'Y'
                   IF IRAT-EFF-DATE > 0 AND IRAT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'F' TO IRAM-IRA-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3600-COUNT-SEQ.
           IF IRAT-SEQ < 9999999
               ADD 1 TO IRAT-SEQ
           END-IF
           IF IRAT-SEQ NOT < 6
               MOVE 'Y' TO IRAM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ROLLOVER TIER TIER FOR THE WITHHOLD AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3610-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-ROLLOVER-TIER-RATE(1) TO IRAM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF IRAM-WITHHOLD-AMT NOT < WS-ROLLOVER-TIER-LOW(WS-TX)
                   MOVE WS-ROLLOVER-TIER-RATE(WS-TX) TO IRAM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF IRAM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO IRAM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3620-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF IRAM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO IRAM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DISTRIB YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO RMD AMT.
      *----------------------------------------------------------------*
       3630-PRORATE-RMD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE IRAT-RMD-AMT ROUNDED =
                   IRAM-DISTRIB-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO IRAT-RMD-AMT
           END-IF
           ADD IRAT-RMD-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * EXCESS AMT IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3640-LIMIT-EXCESS.
           IF IRAT-EXCESS-AMT < 0.00
               MOVE 0.00 TO IRAT-EXCESS-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF IRAT-EXCESS-AMT > 5000.00
               MOVE 5000.00 TO IRAT-EXCESS-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 15 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3650-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF IRAM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO IRAM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FMV YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CONTRIB AMT.
      *----------------------------------------------------------------*
       3660-PRORATE-CONTRIB.
           IF WS-PERIOD-DAYS > 0
               COMPUTE IRAM-CONTRIB-AMT ROUNDED =
                   IRAM-FMV-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO IRAM-CONTRIB-AMT
           END-IF
           ADD IRAM-CONTRIB-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * CONTRIB AMT IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3670-LIMIT-CONTRIB.
           IF IRAM-CONTRIB-AMT < 100.00
               MOVE 100.00 TO IRAM-CONTRIB-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF IRAM-CONTRIB-AMT > 10000.00
               MOVE 10000.00 TO IRAM-CONTRIB-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3680-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF IRAT-EFF-DATE > 0 AND IRAT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD IRAM-FMV-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD IRAM-FMV-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD IRAM-FMV-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD IRAM-FMV-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD IRAM-FMV-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3690-COUNT-ITEM.
           IF IRAM-ITEM-CNT < 9999999
               ADD 1 TO IRAM-ITEM-CNT
           END-IF
           IF IRAM-ITEM-CNT NOT < 5
               MOVE 'Y' TO IRAM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RMD AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3700-ACCRUE-RMD.
           IF IRAT-RMD-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   IRAT-RMD-AMT * IRAM-RATE / 100 / 365
               ADD WS-WORK-AMT TO IRAM-EXCESS-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DISTRIB GRID TIER FOR THE CONTRIB AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3710-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-DISTRIB-GRID-RATE(1) TO IRAM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF IRAM-CONTRIB-AMT NOT < WS-DISTRIB-GRID-LOW(WS-TX)
                   MOVE WS-DISTRIB-GRID-RATE(WS-TX) TO IRAM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF IRAM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO IRAM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DISTRIB AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3720-ACCRUE-DISTRIB.
           IF IRAT-DISTRIB-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   IRAT-DISTRIB-AMT * IRAM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO IRAM-CONTRIB-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3730-DESCRIBE-REGION.
           EVALUATE IRAM-REGION-CD
               WHEN 'CE'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'FL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'GY'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'HU'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'KA'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'SC'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'VB'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE ROLLOVER TIER TIER FOR THE CONTRIB AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3740-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-ROLLOVER-TIER-RATE(1) TO IRAM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF IRAM-CONTRIB-AMT NOT < WS-ROLLOVER-TIER-LOW(WS-TX)
                   MOVE WS-ROLLOVER-TIER-RATE(WS-TX) TO IRAM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF IRAM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO IRAM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * CE -> FL WHEN EFF DATE IS PAST
      * FL -> GY WHEN EFF DATE IS PAST
      * GY -> HU WHEN EFF DATE IS PAST
      * HU -> KA WHEN EFF DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3750-SET-REGION.
           EVALUATE IRAM-REGION-CD
               WHEN 'CE'
                   IF IRAT-EFF-DATE > 0 AND IRAT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FL' TO IRAM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FL'
                   IF IRAT-EFF-DATE > 0 AND IRAT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GY' TO IRAM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GY'
                   IF IRAT-EFF-DATE > 0 AND IRAT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HU' TO IRAM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HU'
                   IF IRAT-EFF-DATE > 0 AND IRAT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KA' TO IRAM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3760-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF IRAM-OPEN-DATE > 0 AND IRAM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT FMV AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3770-SPLIT-FMV.
           COMPUTE WS-WORK-AMT = IRAM-FMV-AMT / 4
           COMPUTE WS-WORK-AMT-2 = IRAM-FMV-AMT - (WS-WORK-AMT * 4)
           COMPUTE IRAM-DISTRIB-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3780-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF IRAT-POST-DATE > 0 AND IRAT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD IRAT-EXCESS-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD IRAT-EXCESS-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD IRAT-EXCESS-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD IRAT-EXCESS-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD IRAT-EXCESS-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3790-DESCRIBE-TYPE.
           EVALUATE IRAM-TYPE-CD
               WHEN 'EA'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'HR'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'XZ'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ROLLOVER AMT.
      *----------------------------------------------------------------*
       3800-PRORATE-ROLLOVER.
           IF WS-PERIOD-DAYS > 0
               COMPUTE IRAM-ROLLOVER-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO IRAM-ROLLOVER-AMT
           END-IF
           ADD IRAM-ROLLOVER-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * CN -> DS WHEN EFFECTIVE DATE IS PAST
      * DS -> RO WHEN EFFECTIVE DATE IS PAST
      * RO -> RC WHEN EFFECTIVE DATE IS PAST
      * RC -> WH WHEN EFFECTIVE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3810-SET-TRAN.
           EVALUATE IRAT-TRAN-CD
               WHEN 'CN'
                   IF IRAM-EFFECTIVE-DATE > 0 AND IRAM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DS' TO IRAT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DS'
                   IF IRAM-EFFECTIVE-DATE > 0 AND IRAM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RO' TO IRAT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RO'
                   IF IRAM-EFFECTIVE-DATE > 0 AND IRAM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RC' TO IRAT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RC'
                   IF IRAM-EFFECTIVE-DATE > 0 AND IRAM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WH' TO IRAT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT ROLLOVER AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3820-SPLIT-ROLLOVER.
           COMPUTE WS-WORK-AMT = IRAM-ROLLOVER-AMT / 4
           COMPUTE WS-WORK-AMT-2 = IRAM-ROLLOVER-AMT - (WS-WORK-AMT * 4)
           COMPUTE IRAM-FMV-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RMD FEE ON THE DISTRIB AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3830-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN IRAM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO IRAM-RMD-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN IRAT-REVERSAL-FLAG = 'Y'
                   COMPUTE IRAM-RMD-AMT ROUNDED = IRAT-DISTRIB-AMT *
                       IRAM-MARGIN / 200
               WHEN IRAT-CHANNEL-CD = 'BRN'
                   MOVE 12.00 TO IRAM-RMD-AMT
               WHEN IRAT-CHANNEL-CD = 'BAT'
                   MOVE 15.00 TO IRAM-RMD-AMT
               WHEN OTHER
                   COMPUTE IRAM-RMD-AMT ROUNDED = IRAT-DISTRIB-AMT *
                       IRAM-MARGIN / 100
           END-EVALUATE
           IF IRAM-RMD-AMT > 0 AND IRAM-RMD-AMT < 2.00
               MOVE 2.00 TO IRAM-RMD-AMT
           END-IF
           IF IRAM-RMD-AMT > 25.00
               MOVE 25.00 TO IRAM-RMD-AMT
           END-IF
           ADD IRAM-RMD-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * RMD AMT IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3840-LIMIT-RMD.
           IF IRAT-RMD-AMT < 0.00
               MOVE 0.00 TO IRAT-RMD-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF IRAT-RMD-AMT > 2500.00
               MOVE 2500.00 TO IRAT-RMD-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3850-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF IRAM-LAST-STMT-DATE > 0 AND IRAM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD IRAM-DISTRIB-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD IRAM-DISTRIB-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD IRAM-DISTRIB-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD IRAM-DISTRIB-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD IRAM-DISTRIB-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE IRA TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3860-DESCRIBE-IRA-TYPE.
           EVALUATE IRAM-IRA-TYPE-CD
               WHEN 'F'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'V'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'W'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'Y'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RMD AMT IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3870-LIMIT-RMD.
           IF IRAM-RMD-AMT < 10.00
               MOVE 10.00 TO IRAM-RMD-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF IRAM-RMD-AMT > 500.00
               MOVE 500.00 TO IRAM-RMD-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE EXCESS AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3880-ASSESS-STOP.
           EVALUATE TRUE
               WHEN IRAM-STOP-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN IRAM-HOLD-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = IRAM-EXCESS-AMT *
                       IRAM-MARGIN / 200
               WHEN IRAM-REGION-CD = 'CE'
                   MOVE 12.00 TO WS-WORK-AMT-2
               WHEN IRAM-REGION-CD = 'FL'
                   MOVE 5.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = IRAM-EXCESS-AMT *
                       IRAM-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 2.00
               MOVE 2.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 35.00
               MOVE 35.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE FMV TIER TIER FOR THE RMD AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3890-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FMV-TIER-RATE(1) TO IRAM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF IRAT-RMD-AMT NOT < WS-FMV-TIER-LOW(WS-TX)
                   MOVE WS-FMV-TIER-RATE(WS-TX) TO IRAM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF IRAM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO IRAM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE EXCESS AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3900-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRAT-TRAN-CD
                   ADD IRAT-EXCESS-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRAT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE IRAT-EXCESS-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3910-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF IRAM-LAST-ACT-DATE > 0 AND IRAM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD IRAM-RMD-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD IRAM-RMD-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD IRAM-RMD-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD IRAM-RMD-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD IRAM-RMD-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3920-DESCRIBE-REGION.
           EVALUATE IRAM-REGION-CD
               WHEN 'CE'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'FL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'GY'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'HU'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'KA'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'SC'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'VB'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3930-COUNT-ITEM.
           IF IRAM-ITEM-CNT < 9999999
               ADD 1 TO IRAM-ITEM-CNT
           END-IF
           IF IRAM-ITEM-CNT NOT < 12
               MOVE 'Y' TO IRAM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * EA -> HR WHEN MATURITY DATE IS PAST
      * HR -> XZ WHEN MATURITY DATE IS PAST
      * XZ -> EA WHEN MATURITY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3940-SET-TYPE.
           EVALUATE IRAM-TYPE-CD
               WHEN 'EA'
                   IF IRAM-MATURITY-DATE > 0 AND IRAM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HR' TO IRAM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HR'
                   IF IRAM-MATURITY-DATE > 0 AND IRAM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XZ' TO IRAM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XZ'
                   IF IRAM-MATURITY-DATE > 0 AND IRAM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EA' TO IRAM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT ROLLOVER AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3950-SPLIT-ROLLOVER.
           COMPUTE WS-WORK-AMT = IRAM-ROLLOVER-AMT / 2
           COMPUTE WS-WORK-AMT-2 = IRAM-ROLLOVER-AMT - (WS-WORK-AMT * 2)
           COMPUTE IRAM-FMV-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DISTRIB YTD IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3960-LIMIT-DISTRIB.
           IF IRAM-DISTRIB-YTD < 50.00
               MOVE 50.00 TO IRAM-DISTRIB-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF IRAM-DISTRIB-YTD > 10000.00
               MOVE 10000.00 TO IRAM-DISTRIB-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3970-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF IRAM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO IRAM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON EXCESS AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3980-ACCRUE-EXCESS.
           IF IRAM-EXCESS-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   IRAM-EXCESS-AMT * IRAM-RATE / 100 / 365
               ADD WS-WORK-AMT TO IRAM-WITHHOLD-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DISTRIB AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3990-PRORATE-DISTRIB.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   IRAM-DISTRIB-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR IRA TYPE CD:
      * F -> V WHEN LAST ACT DATE IS PAST
      * V -> W WHEN LAST ACT DATE IS PAST
      * W -> Y WHEN LAST ACT DATE IS PAST
      * Y -> F WHEN LAST ACT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4000-SET-IRA-TYPE.
           EVALUATE IRAM-IRA-TYPE-CD
               WHEN 'F'
                   IF IRAM-LAST-ACT-DATE > 0 AND IRAM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'V' TO IRAM-IRA-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'V'
                   IF IRAM-LAST-ACT-DATE > 0 AND IRAM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO IRAM-IRA-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'W'
                   IF IRAM-LAST-ACT-DATE > 0 AND IRAM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'Y' TO IRAM-IRA-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'Y'
                   IF IRAM-LAST-ACT-DATE > 0 AND IRAM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'F' TO IRAM-IRA-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE RMD AMT BY DIST CODE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4010-ACCUM-DIST-CODE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRAM-DIST-CODE-CD
                   ADD IRAM-RMD-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRAM-DIST-CODE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE IRAM-RMD-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FMV YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4020-FLAG-FMV.
           IF IRAM-FMV-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE IRAM-FMV-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ROLLOVER AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4030-FLAG-ROLLOVER.
           IF IRAM-ROLLOVER-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE IRAM-ROLLOVER-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 15 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4040-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF IRAM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO IRAM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4050-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRAT-TRAN-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRAT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * WITHHOLD AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4060-FLAG-WITHHOLD.
           IF IRAM-WITHHOLD-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE IRAM-WITHHOLD-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ROLLOVER YTD BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4070-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRAM-TYPE-CD
                   ADD IRAM-ROLLOVER-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRAM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE IRAM-ROLLOVER-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DISTRIB YTD AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4080-ACCRUE-DISTRIB.
           IF IRAM-DISTRIB-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   IRAM-DISTRIB-YTD * IRAM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO IRAM-CONTRIB-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE EXCESS YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FMV YTD.
      *----------------------------------------------------------------*
       4090-PRORATE-FMV.
           IF WS-PERIOD-DAYS > 0
               COMPUTE IRAM-FMV-YTD ROUNDED =
                   IRAM-EXCESS-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO IRAM-FMV-YTD
           END-IF
           ADD IRAM-FMV-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4100-DESCRIBE-REGION.
           EVALUATE IRAM-REGION-CD
               WHEN 'CE'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'FL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'GY'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'HU'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'KA'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'SC'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'VB'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4110-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF IRAT-EFF-DATE > 0 AND IRAT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD IRAM-ROLLOVER-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD IRAM-ROLLOVER-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD IRAM-ROLLOVER-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD IRAM-ROLLOVER-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD IRAM-ROLLOVER-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RMD AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4120-ACCRUE-RMD.
           IF IRAT-RMD-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   IRAT-RMD-AMT * IRAM-RATE / 100 / 365
               ADD WS-WORK-AMT TO IRAM-FMV-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WITHHOLD YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4130-SPLIT-WITHHOLD.
           COMPUTE WS-WORK-AMT = IRAM-WITHHOLD-YTD / 2
           COMPUTE WS-WORK-AMT-2 = IRAM-WITHHOLD-YTD - (WS-WORK-AMT * 2)
           COMPUTE IRAM-CONTRIB-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE DIST CODE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4140-DESCRIBE-DIST-CODE.
           EVALUATE IRAM-DIST-CODE-CD
               WHEN 'C'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'J'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'U'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'X'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE DISTRIB AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO RMD AMT.
      *----------------------------------------------------------------*
       4150-PRORATE-RMD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE IRAT-RMD-AMT ROUNDED =
                   IRAT-DISTRIB-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO IRAT-RMD-AMT
           END-IF
           ADD IRAT-RMD-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * WITHHOLD AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4160-FLAG-WITHHOLD.
           IF IRAM-WITHHOLD-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE IRAM-WITHHOLD-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * EXCESS AMT IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4170-LIMIT-EXCESS.
           IF IRAM-EXCESS-AMT < 10.00
               MOVE 10.00 TO IRAM-EXCESS-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF IRAM-EXCESS-AMT > 1000.00
               MOVE 1000.00 TO IRAM-EXCESS-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RMD AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4180-ACCRUE-RMD.
           IF IRAM-RMD-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   IRAM-RMD-AMT * IRAM-RATE / 100 / 365
               ADD WS-WORK-AMT TO IRAM-EXCESS-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DISTRIB AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4190-SPLIT-DISTRIB.
           COMPUTE WS-WORK-AMT = IRAT-DISTRIB-AMT / 12
           COMPUTE WS-WORK-AMT-2 = IRAT-DISTRIB-AMT - (WS-WORK-AMT * 12)
           COMPUTE IRAM-RMD-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RMD AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4200-FLAG-RMD.
           IF IRAM-RMD-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE IRAM-RMD-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4210-DESCRIBE-TYPE.
           EVALUATE IRAM-TYPE-CD
               WHEN 'EA'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'HR'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'XZ'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4220-COUNT-RETRY.
           IF IRAM-RETRY-CNT < 9999999
               ADD 1 TO IRAM-RETRY-CNT
           END-IF
           IF IRAM-RETRY-CNT NOT < 5
               MOVE 'Y' TO IRAT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DIST CODE CD:
      * C -> J WHEN NEXT DUE DATE IS PAST
      * J -> U WHEN NEXT DUE DATE IS PAST
      * U -> X WHEN NEXT DUE DATE IS PAST
      * X -> C WHEN NEXT DUE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4230-SET-DIST-CODE.
           EVALUATE IRAM-DIST-CODE-CD
               WHEN 'C'
                   IF IRAM-NEXT-DUE-DATE > 0 AND IRAM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'J' TO IRAM-DIST-CODE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'J'
                   IF IRAM-NEXT-DUE-DATE > 0 AND IRAM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO IRAM-DIST-CODE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF IRAM-NEXT-DUE-DATE > 0 AND IRAM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'X' TO IRAM-DIST-CODE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'X'
                   IF IRAM-NEXT-DUE-DATE > 0 AND IRAM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'C' TO IRAM-DIST-CODE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCESS AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4240-FLAG-EXCESS.
           IF IRAM-EXCESS-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE IRAM-EXCESS-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DISTRIB YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4250-ACCRUE-DISTRIB.
           IF IRAM-DISTRIB-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   IRAM-DISTRIB-YTD * IRAM-RATE / 100 / 365
               ADD WS-WORK-AMT TO IRAM-RMD-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FMV YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4260-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRAM-CHANNEL-CD
                   ADD IRAM-FMV-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRAM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE IRAM-FMV-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DISTRIB GRID TIER FOR THE DISTRIB AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4270-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DISTRIB-GRID-RATE(1) TO IRAM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF IRAT-DISTRIB-AMT NOT < WS-DISTRIB-GRID-LOW(WS-TX)
                   MOVE WS-DISTRIB-GRID-RATE(WS-TX) TO IRAM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF IRAM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO IRAM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CONTRIB AMT IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4280-LIMIT-CONTRIB.
           IF IRAM-CONTRIB-AMT < 50.00
               MOVE 50.00 TO IRAM-CONTRIB-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF IRAM-CONTRIB-AMT > 2500.00
               MOVE 2500.00 TO IRAM-CONTRIB-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE RMD AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO EXCESS AMT.
      *----------------------------------------------------------------*
       4290-PRORATE-EXCESS.
           IF WS-PERIOD-DAYS > 0
               COMPUTE IRAM-EXCESS-AMT ROUNDED =
                   IRAM-RMD-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO IRAM-EXCESS-AMT
           END-IF
           ADD IRAM-EXCESS-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * RMD AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4300-FLAG-RMD.
           IF IRAM-RMD-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE IRAM-RMD-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ROLLOVER AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4310-SPLIT-ROLLOVER.
           COMPUTE WS-WORK-AMT = IRAM-ROLLOVER-AMT / 4
           COMPUTE WS-WORK-AMT-2 = IRAM-ROLLOVER-AMT - (WS-WORK-AMT * 4)
           COMPUTE IRAM-FMV-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4320-FLAG-ITEM.
           IF WS-WORK-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * CE -> FL WHEN LAST ACT DATE IS PAST
      * FL -> GY WHEN LAST ACT DATE IS PAST
      * GY -> HU WHEN LAST ACT DATE IS PAST
      * HU -> KA WHEN LAST ACT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4330-SET-REGION.
           EVALUATE IRAM-REGION-CD
               WHEN 'CE'
                   IF IRAM-LAST-ACT-DATE > 0 AND IRAM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FL' TO IRAM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FL'
                   IF IRAM-LAST-ACT-DATE > 0 AND IRAM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GY' TO IRAM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GY'
                   IF IRAM-LAST-ACT-DATE > 0 AND IRAM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HU' TO IRAM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HU'
                   IF IRAM-LAST-ACT-DATE > 0 AND IRAM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KA' TO IRAM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE EXCESS AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO RMD YTD.
      *----------------------------------------------------------------*
       4340-PRORATE-RMD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE IRAM-RMD-YTD ROUNDED =
                   IRAT-EXCESS-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO IRAM-RMD-YTD
           END-IF
           ADD IRAM-RMD-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * EA -> HR WHEN EXPIRY DATE IS PAST
      * HR -> XZ WHEN EXPIRY DATE IS PAST
      * XZ -> EA WHEN EXPIRY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4350-SET-TYPE.
           EVALUATE IRAM-TYPE-CD
               WHEN 'EA'
                   IF IRAM-EXPIRY-DATE > 0 AND IRAM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HR' TO IRAM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HR'
                   IF IRAM-EXPIRY-DATE > 0 AND IRAM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XZ' TO IRAM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XZ'
                   IF IRAM-EXPIRY-DATE > 0 AND IRAM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EA' TO IRAM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       4360-COUNT-PAYMENT.
           IF IRAM-PAYMENT-CNT < 9999999
               ADD 1 TO IRAM-PAYMENT-CNT
           END-IF
           IF IRAM-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO IRAM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FMV TIER TIER FOR THE RMD AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4370-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FMV-TIER-RATE(1) TO IRAM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF IRAT-RMD-AMT NOT < WS-FMV-TIER-LOW(WS-TX)
                   MOVE WS-FMV-TIER-RATE(WS-TX) TO IRAM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF IRAM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO IRAM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4380-DESCRIBE-TRAN.
           EVALUATE IRAT-TRAN-CD
               WHEN 'CN'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'DS'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'RO'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'RC'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'WH'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE CONTRIB FEE ON THE DISTRIB AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4390-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN IRAM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO IRAM-CONTRIB-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN IRAM-HOLD-FLAG = 'Y'
                   COMPUTE IRAM-CONTRIB-AMT ROUNDED = IRAT-DISTRIB-AMT *
                       IRAM-FEE-PCT / 200
               WHEN IRAM-IRA-TYPE-CD = 'F'
                   MOVE 12.00 TO IRAM-CONTRIB-AMT
               WHEN IRAM-IRA-TYPE-CD = 'V'
                   MOVE 5.00 TO IRAM-CONTRIB-AMT
               WHEN OTHER
                   COMPUTE IRAM-CONTRIB-AMT ROUNDED = IRAT-DISTRIB-AMT *
                       IRAM-FEE-PCT / 100
           END-EVALUATE
           IF IRAM-CONTRIB-AMT > 0 AND IRAM-CONTRIB-AMT < 10.00
               MOVE 10.00 TO IRAM-CONTRIB-AMT
           END-IF
           IF IRAM-CONTRIB-AMT > 50.00
               MOVE 50.00 TO IRAM-CONTRIB-AMT
           END-IF
           ADD IRAM-CONTRIB-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PASS THE IRA FIELDS TO IRVAL AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7010-CALL-IRVAL.
           INITIALIZE IRVAL-PARMS
           MOVE IRAM-WITHHOLD-YTD
             TO IRVAL-WITHHOLD-YTD
           MOVE IRAM-EXCESS-AMT
             TO IRVAL-EXCESS-AMT
           MOVE IRAM-ROLLOVER-AMT
             TO IRVAL-ROLLOVER-AMT
           MOVE IRAM-RMD-YTD
             TO IRVAL-RMD-YTD
           MOVE IRAM-FMV-YTD
             TO IRVAL-FMV-YTD
           MOVE IRAM-FMV-AMT
             TO IRVAL-FMV-AMT
           MOVE IRAM-NEXT-DUE-DATE
             TO IRVAL-NEXT-DUE-DATE
           MOVE IRAM-EFFECTIVE-DATE
             TO IRVAL-EFFECTIVE-DATE
           MOVE IRAM-MATURITY-DATE
             TO IRVAL-MATURITY-DATE
           MOVE IRAM-LAST-STMT-DATE
             TO IRVAL-LAST-STMT-DATE
           MOVE IRAM-TYPE-CD
             TO IRVAL-TYPE-CD
           MOVE IRAM-DIST-CODE-CD
             TO IRVAL-DIST-CODE-CD
           MOVE IRAM-CHANNEL-CD
             TO IRVAL-CHANNEL-CD
           MOVE IRAM-REGION-CD
             TO IRVAL-REGION-CD
           MOVE IRAM-HOLD-FLAG
             TO IRVAL-HOLD-FLAG
           MOVE IRAM-OVERRIDE-FLAG
             TO IRVAL-OVERRIDE-FLAG
           MOVE IRAM-STOP-FLAG
             TO IRVAL-STOP-FLAG
           MOVE IRAM-ITEM-CNT
             TO IRVAL-ITEM-CNT
           MOVE IRAM-CYCLE-CNT
             TO IRVAL-CYCLE-CNT
           MOVE IRAM-RATE
             TO IRVAL-RATE
           MOVE IRAM-MARGIN
             TO IRVAL-MARGIN
           MOVE IRAM-CONTACT
             TO IRVAL-CONTACT
           MOVE IRAM-ADDR
             TO IRVAL-ADDR
           CALL 'IRVAL' USING IRVAL-PARMS
           EVALUATE IRVAL-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE IRVAL-WITHHOLD-YTD
                     TO IRAM-WITHHOLD-YTD
                   MOVE IRVAL-EXCESS-AMT
                     TO IRAM-EXCESS-AMT
                   MOVE IRVAL-ROLLOVER-AMT
                     TO IRAM-ROLLOVER-AMT
                   MOVE IRVAL-FMV-YTD
                     TO IRAM-FMV-YTD
                   MOVE IRVAL-HOLD-FLAG
                     TO IRAM-HOLD-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RVAL' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-IRVAL' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE IRA FIELDS TO IRPRC AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7000-CALL-IRPRC.
           INITIALIZE IRPRC-PARMS
           MOVE IRAM-RMD-YTD
             TO IRPRC-RMD-YTD
           MOVE IRAM-CONTRIB-YTD
             TO IRPRC-CONTRIB-YTD
           MOVE IRAM-EXCESS-YTD
             TO IRPRC-EXCESS-YTD
           MOVE IRAM-DISTRIB-YTD
             TO IRPRC-DISTRIB-YTD
           MOVE IRAM-FMV-AMT
             TO IRPRC-FMV-AMT
           MOVE IRAM-ROLLOVER-AMT
             TO IRPRC-ROLLOVER-AMT
           MOVE IRAM-NEXT-REVIEW-DATE
             TO IRPRC-NEXT-REVIEW-DATE
           MOVE IRAM-LAST-STMT-DATE
             TO IRPRC-LAST-STMT-DATE
           MOVE IRAM-LAST-ACT-DATE
             TO IRPRC-LAST-ACT-DATE
           MOVE IRAM-EXPIRY-DATE
             TO IRPRC-EXPIRY-DATE
           MOVE IRAM-IRA-TYPE-CD
             TO IRPRC-IRA-TYPE-CD
           MOVE IRAM-TYPE-CD
             TO IRPRC-TYPE-CD
           MOVE IRAM-CHANNEL-CD
             TO IRPRC-CHANNEL-CD
           MOVE IRAM-REASON-CD
             TO IRPRC-REASON-CD
           MOVE IRAM-OVERRIDE-FLAG
             TO IRPRC-OVERRIDE-FLAG
           MOVE IRAM-SUSPEND-FLAG
             TO IRPRC-SUSPEND-FLAG
           MOVE IRAM-AUTO-RENEW-FLAG
             TO IRPRC-AUTO-RENEW-FLAG
           MOVE IRAM-PAYMENT-CNT
             TO IRPRC-PAYMENT-CNT
           MOVE IRAM-RETRY-CNT
             TO IRPRC-RETRY-CNT
           MOVE IRAM-MARGIN
             TO IRPRC-MARGIN
           MOVE IRAM-FEE-PCT
             TO IRPRC-FEE-PCT
           MOVE IRAM-NAME
             TO IRPRC-NAME
           MOVE IRAM-ADDR
             TO IRPRC-ADDR
           CALL 'IRPRC' USING IRPRC-PARMS
           EVALUATE IRPRC-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE IRPRC-RMD-YTD
                     TO IRAM-RMD-YTD
                   MOVE IRPRC-DISTRIB-YTD
                     TO IRAM-DISTRIB-YTD
                   MOVE IRPRC-ROLLOVER-AMT
                     TO IRAM-ROLLOVER-AMT
                   MOVE IRPRC-OVERRIDE-FLAG
                     TO IRAM-OVERRIDE-FLAG
                   MOVE IRPRC-SUSPEND-FLAG
                     TO IRAM-SUSPEND-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RPRC' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-IRPRC' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE IRA FIELDS TO IRELG AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7020-CALL-IRELG.
           INITIALIZE IRELG-PARMS
           MOVE IRAM-FMV-AMT
             TO IRELG-FMV-AMT
           MOVE IRAM-EXCESS-AMT
             TO IRELG-EXCESS-AMT
           MOVE IRAM-ROLLOVER-YTD
             TO IRELG-ROLLOVER-YTD
           MOVE IRAM-RMD-AMT
             TO IRELG-RMD-AMT
           MOVE IRAM-ROLLOVER-AMT
             TO IRELG-ROLLOVER-AMT
           MOVE IRAM-FMV-YTD
             TO IRELG-FMV-YTD
           MOVE IRAM-LAST-ACT-DATE
             TO IRELG-LAST-ACT-DATE
           MOVE IRAM-LAST-STMT-DATE
             TO IRELG-LAST-STMT-DATE
           MOVE IRAM-EXPIRY-DATE
             TO IRELG-EXPIRY-DATE
           MOVE IRAM-NEXT-REVIEW-DATE
             TO IRELG-NEXT-REVIEW-DATE
           MOVE IRAM-CHANNEL-CD
             TO IRELG-CHANNEL-CD
           MOVE IRAM-REGION-CD
             TO IRELG-REGION-CD
           MOVE IRAM-REASON-CD
             TO IRELG-REASON-CD
           MOVE IRAM-TYPE-CD
             TO IRELG-TYPE-CD
           MOVE IRAM-STOP-FLAG
             TO IRELG-STOP-FLAG
           MOVE IRAM-REVIEW-FLAG
             TO IRELG-REVIEW-FLAG
           MOVE IRAM-AUTO-RENEW-FLAG
             TO IRELG-AUTO-RENEW-FLAG
           MOVE IRAM-CYCLE-CNT
             TO IRELG-CYCLE-CNT
           MOVE IRAM-RETRY-CNT
             TO IRELG-RETRY-CNT
           MOVE IRAM-MARGIN
             TO IRELG-MARGIN
           MOVE IRAM-FEE-PCT
             TO IRELG-FEE-PCT
           MOVE IRAM-ADDR
             TO IRELG-ADDR
           MOVE IRAM-NAME
             TO IRELG-NAME
           CALL 'IRELG' USING IRELG-PARMS
           EVALUATE IRELG-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE IRELG-FMV-AMT
                     TO IRAM-FMV-AMT
                   MOVE IRELG-EXCESS-AMT
                     TO IRAM-EXCESS-AMT
                   MOVE IRELG-RMD-AMT
                     TO IRAM-RMD-AMT
                   MOVE IRELG-ROLLOVER-AMT
                     TO IRAM-ROLLOVER-AMT
                   MOVE IRELG-STOP-FLAG
                     TO IRAM-STOP-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RELG' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-IRELG' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE IRAT-KEY TO RD-KEY
           MOVE IRAT-TRAN-CD TO RD-CODE
           MOVE IRAT-DISTRIB-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE IRAT-KEY TO RD-KEY
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
           DISPLAY 'IRACALC READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'IRACALC' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
