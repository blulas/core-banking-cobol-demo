      *================================================================*
      * PROGRAM  : PYLEXTR                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : PAYROLL SERVICES - NIGHTLY EXTRACT. READS THE PRL   *
      *            TRANSACTIONS, UPDATES THE PRL MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. CLIENT PAYROLL CALCULATION, TAX *
      *            DEPOSIT AND DIRECT DEPOSIT                          *
      * INPUT    : PRLMST (KSDS), PRLTRN, BANKCTL                      *
      * OUTPUT   : PRLOUT, PYLEXRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, PYPEN, PYRISK, PYPRC    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1989-09-12 RJT  ORIGINAL                                       *
      * 1992-08-27 MKL  NEW STATE-TAX REPORT COLUMNS CHG6214           *
      * 1998-01-13 SKN  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2002-05-22 DPS  DEDUCT STATUS CODES ADDED REQ0800              *
      * 2006-09-04 GAF  FICA FEE SCHEDULE UPDATE CHG0318               *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PYLEXTR.
       AUTHOR. T W HOLT.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "PRLMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS PRLM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "PRLTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "PRLOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "PYLEXRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY PRLMST.
       FD  TRAN-FILE.
       COPY PRLTRN.
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
           05  WS-PERIOD-DAYS              PIC S9(05) VALUE 31.
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
      * WS-NET-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-NET-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000191000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000384500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000182000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010050000000102000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011050000000217000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011300000000812000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011350000000119500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021350000000264500'.
       01  WS-NET-BAND-TBL REDEFINES WS-NET-BAND-VALUES.
           05  WS-NET-BAND-ROW OCCURS 8 TIMES.
               10  WS-NET-BAND-LOW         PIC 9(09)V99.
               10  WS-NET-BAND-RATE        PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-GROSS-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-GROSS-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000669000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000230000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000150000000291000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005150000000523000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005250000000799500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006250000000616000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006300000000772500'.
       01  WS-GROSS-BAND-TBL REDEFINES WS-GROSS-BAND-VALUES.
           05  WS-GROSS-BAND-ROW OCCURS 7 TIMES.
               10  WS-GROSS-BAND-LOW       PIC 9(09)V99.
               10  WS-GROSS-BAND-RATE      PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-DEDUCT-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-DEDUCT-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000379000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000556000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000730000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001500000000729500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001750000000366000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002250000000331500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002750000000073000'.
       01  WS-DEDUCT-SCHED-TBL REDEFINES WS-DEDUCT-SCHED-VALUES.
           05  WS-DEDUCT-SCHED-ROW OCCURS 7 TIMES.
               10  WS-DEDUCT-SCHED-LOW     PIC 9(09)V99.
               10  WS-DEDUCT-SCHED-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-DEDUCT-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-DEDUCT-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000055000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000016000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012500000000306500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013000000000505500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023000000000057000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00028000000000045500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00033000000000080000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00035500000000675500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00035750000000774000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00036250000000348000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00038750000000569000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00038850000000035000'.
       01  WS-DEDUCT-GRID-TBL REDEFINES WS-DEDUCT-GRID-VALUES.
           05  WS-DEDUCT-GRID-ROW OCCURS 12 TIMES.
               10  WS-DEDUCT-GRID-LOW      PIC 9(09)V99.
               10  WS-DEDUCT-GRID-RATE     PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  PYLEXTR  PAYROLL SERVICES'.
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
       COPY PYPENP.
       COPY PYRISKP.
       COPY PYPRCP.

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
               MOVE 'PRLMST' TO ABND-FILE-NAME
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
           MOVE PRLT-KEY TO PRLM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-SPLIT-DEDUCT
               PERFORM 3010-ACCUM-SOURCE
               PERFORM 3020-SET-PAY-FREQ
               PERFORM 3030-DESCRIBE-SOURCE
               PERFORM 3040-COUNT-NOTICE
               PERFORM 3050-FLAG-STATE-TAX
               PERFORM 3060-DESCRIBE-PAY-FREQ
               PERFORM 3070-AGE-CLOSE
               PERFORM 3080-ACCUM-REGION
               PERFORM 3090-FLAG-ITEM
               PERFORM 3100-CHECK-OPEN
               PERFORM 3110-PRORATE-NET
               PERFORM 3120-ACCRUE-NET
               PERFORM 3130-EDIT-ADDR
               PERFORM 3140-FLAG-ITEM
               PERFORM 3150-PRORATE-STATE-TAX
               PERFORM 3160-SELECT-RATE
               PERFORM 3170-PRORATE-FED-TAX
               PERFORM 3180-COUNT-CYCLE
               PERFORM 3190-SET-STATUS
               PERFORM 3200-CHECK-NOTICE
               PERFORM 3210-AGE-CLOSE
               PERFORM 3220-CHECK-EFF
               PERFORM 3230-SET-TRAN
               PERFORM 3240-PRORATE-NET
               PERFORM 3250-DESCRIBE-CHANNEL
               PERFORM 3260-CHECK-OPEN
               PERFORM 3270-EDIT-NAME
               PERFORM 3280-AGE-CLOSE
               PERFORM 3290-ASSESS-AUTO-RENEW
               PERFORM 3300-EDIT-ADDR
               PERFORM 3310-SPLIT-NET
               PERFORM 3320-ACCUM-REGION
               PERFORM 3330-SELECT-MARGIN
               PERFORM 3340-SPLIT-FICA
               PERFORM 3350-EDIT-REF-NO
               PERFORM 3360-EDIT-REF-NO
               PERFORM 3370-DESCRIBE-TRAN
               PERFORM 3380-LIMIT-DEDUCT
               PERFORM 3390-COUNT-SEQ
               PERFORM 3400-AGE-EXPIRY
               PERFORM 3410-LIMIT-FICA
               PERFORM 3420-SET-TRAN
               PERFORM 3430-CHECK-NEXT-DUE
               PERFORM 3440-CHECK-EFFECTIVE
               PERFORM 3450-LIMIT-ITEM
               PERFORM 3460-FLAG-NET
               PERFORM 3470-SET-STATUS
               PERFORM 3480-ASSESS-AUTO-RENEW
               PERFORM 3490-AGE-EFF
               PERFORM 3500-COUNT-NOTICE
               PERFORM 3510-FLAG-STATE-TAX
               PERFORM 3520-AGE-LAST-ACT
               PERFORM 3530-PRORATE-NET
               PERFORM 3540-EDIT-MEMO
               PERFORM 3550-ACCRUE-FED-TAX
               PERFORM 3560-LIMIT-FED-TAX
               PERFORM 3570-SPLIT-NET
               PERFORM 3580-SET-SOURCE
               PERFORM 3590-COUNT-CYCLE
               PERFORM 3600-COUNT-SEQ
               PERFORM 3610-ACCRUE-NET
               PERFORM 3620-CHECK-OPEN
               PERFORM 3630-PRORATE-NET
               PERFORM 3640-PRORATE-NET
               PERFORM 3650-PRORATE-FICA
               PERFORM 3660-FLAG-FICA
               PERFORM 3670-DESCRIBE-SOURCE
               PERFORM 3680-LIMIT-ITEM
               PERFORM 3690-DESCRIBE-SOURCE
               PERFORM 3700-ASSESS-AUTO-RENEW
               PERFORM 3710-FLAG-DEDUCT
               PERFORM 3720-ACCRUE-FICA
               PERFORM 3730-FLAG-STATE-TAX
               PERFORM 3740-COUNT-ITEM
               PERFORM 3750-ACCRUE-GROSS
               PERFORM 3760-FLAG-NET
               PERFORM 3770-SELECT-FEE-PCT
               PERFORM 3780-COUNT-NOTICE
               PERFORM 3790-FLAG-FICA
               PERFORM 3800-SET-REASON
               PERFORM 3810-DESCRIBE-REASON
               PERFORM 3820-COUNT-NOTICE
               PERFORM 3830-EDIT-REJECT
               PERFORM 3840-LIMIT-GROSS
               PERFORM 3850-ACCUM-TRAN
               PERFORM 3860-PRORATE-FED-TAX
               PERFORM 3870-EDIT-CUST-ID
               PERFORM 3880-SET-TAX-STATE
               PERFORM 3890-DESCRIBE-REGION
               PERFORM 3900-FLAG-ITEM
               PERFORM 3910-EDIT-ADDR
               PERFORM 3920-COUNT-NOTICE
               PERFORM 3930-ACCUM-SOURCE
               PERFORM 3940-LIMIT-STATE-TAX
               PERFORM 3950-DESCRIBE-CHANNEL
               PERFORM 3960-LIMIT-GROSS
               PERFORM 3970-SELECT-FEE-PCT
               PERFORM 3980-AGE-LAST-ACT
               PERFORM 3990-PRORATE-DEDUCT
               PERFORM 4000-DESCRIBE-REGION
               PERFORM 4010-EDIT-REJECT
               PERFORM 4020-LIMIT-STATE-TAX
               PERFORM 4030-FLAG-FICA
               PERFORM 4040-LIMIT-STATE-TAX
               PERFORM 4050-LIMIT-FICA
               PERFORM 4060-EDIT-NAME
               PERFORM 4070-SPLIT-GROSS
               PERFORM 4080-DESCRIBE-REASON
               PERFORM 4090-EDIT-OPERATOR
               PERFORM 7040-CALL-PYPEN
               PERFORM 7030-CALL-PYRISK
               PERFORM 7000-CALL-PYPRC
               REWRITE PRL-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'PRLMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM PRL-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * SPLIT DEDUCT YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3000-SPLIT-DEDUCT.
           COMPUTE WS-WORK-AMT = PRLM-DEDUCT-YTD / 4
           COMPUTE WS-WORK-AMT-2 = PRLM-DEDUCT-YTD - (WS-WORK-AMT * 4)
           COMPUTE PRLM-FICA-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3010-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PRLM-SOURCE-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PRLM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PAY FREQ CD:
      * JT -> XM WHEN OPEN DATE IS PAST
      * XM -> ZT WHEN OPEN DATE IS PAST
      * ZT -> JT WHEN OPEN DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3020-SET-PAY-FREQ.
           EVALUATE PRLM-PAY-FREQ-CD
               WHEN 'JT'
                   IF PRLM-OPEN-DATE > 0 AND PRLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XM' TO PRLM-PAY-FREQ-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XM'
                   IF PRLM-OPEN-DATE > 0 AND PRLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZT' TO PRLM-PAY-FREQ-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZT'
                   IF PRLM-OPEN-DATE > 0 AND PRLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JT' TO PRLM-PAY-FREQ-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3030-DESCRIBE-SOURCE.
           EVALUATE PRLM-SOURCE-CD
               WHEN 'NY'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'PL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'VK'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'WB'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'XL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3040-COUNT-NOTICE.
           IF PRLM-NOTICE-CNT < 9999999
               ADD 1 TO PRLM-NOTICE-CNT
           END-IF
           IF PRLM-NOTICE-CNT NOT < 3
               MOVE 'Y' TO PRLM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATE TAX AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3050-FLAG-STATE-TAX.
           IF PRLM-STATE-TAX-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PRLM-STATE-TAX-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PAY FREQ CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3060-DESCRIBE-PAY-FREQ.
           EVALUATE PRLM-PAY-FREQ-CD
               WHEN 'JT'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'XM'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'ZT'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3070-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF PRLM-CLOSE-DATE > 0 AND PRLM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PRLM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PRLM-FICA-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD PRLM-FICA-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD PRLM-FICA-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD PRLM-FICA-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PRLM-FICA-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE DEDUCT AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3080-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PRLM-REGION-CD
                   ADD PRLT-DEDUCT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PRLM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PRLT-DEDUCT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3090-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 120 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3100-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF PRLM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PRLM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO PRLM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DEDUCT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO NET AMT.
      *----------------------------------------------------------------*
       3110-PRORATE-NET.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PRLM-NET-AMT ROUNDED =
                   PRLT-DEDUCT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PRLM-NET-AMT
           END-IF
           ADD PRLM-NET-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON NET AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3120-ACCRUE-NET.
           IF PRLT-NET-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PRLT-NET-AMT * PRLM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO PRLM-FED-TAX-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E143 ADDR REQUIRED
      * E622 NET AMT NEGATIVE
      * E955 NET AMT OVER 25000
      * E033 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3130-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PRLM-ADDR = SPACES
                   MOVE 'E143' TO WS-REJECT-CODE
               WHEN PRLT-NET-AMT < 0
                   MOVE 'E622' TO WS-REJECT-CODE
               WHEN PRLT-NET-AMT > 25000.00
                   MOVE 'E955' TO WS-REJECT-CODE
               WHEN PRLM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E033' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3140-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DEDUCT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO STATE TAX AMT.
      *----------------------------------------------------------------*
       3150-PRORATE-STATE-TAX.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PRLT-STATE-TAX-AMT ROUNDED =
                   PRLM-DEDUCT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PRLT-STATE-TAX-AMT
           END-IF
           ADD PRLT-STATE-TAX-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE DEDUCT SCHED TIER FOR THE FICA YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3160-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-DEDUCT-SCHED-RATE(1) TO PRLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF PRLM-FICA-YTD NOT < WS-DEDUCT-SCHED-LOW(WS-TX)
                   MOVE WS-DEDUCT-SCHED-RATE(WS-TX) TO PRLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PRLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PRLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE NET YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FED TAX AMT.
      *----------------------------------------------------------------*
       3170-PRORATE-FED-TAX.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PRLM-FED-TAX-AMT ROUNDED =
                   PRLM-NET-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PRLM-FED-TAX-AMT
           END-IF
           ADD PRLM-FED-TAX-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3180-COUNT-CYCLE.
           IF PRLM-CYCLE-CNT < 9999999
               ADD 1 TO PRLM-CYCLE-CNT
           END-IF
           IF PRLM-CYCLE-CNT NOT < 3
               MOVE 'Y' TO PRLT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * MV -> RD WHEN LAST PAY DATE IS PAST
      * RD -> ST WHEN LAST PAY DATE IS PAST
      * ST -> TJ WHEN LAST PAY DATE IS PAST
      * TJ -> UC WHEN LAST PAY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3190-SET-STATUS.
           EVALUATE PRLM-STATUS-CD
               WHEN 'MV'
                   IF PRLM-LAST-PAY-DATE > 0 AND PRLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RD' TO PRLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RD'
                   IF PRLM-LAST-PAY-DATE > 0 AND PRLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ST' TO PRLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ST'
                   IF PRLM-LAST-PAY-DATE > 0 AND PRLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TJ' TO PRLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TJ'
                   IF PRLM-LAST-PAY-DATE > 0 AND PRLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UC' TO PRLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3200-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF PRLM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PRLM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO PRLM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3210-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF PRLM-CLOSE-DATE > 0 AND PRLM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PRLM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PRLM-FICA-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PRLM-FICA-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD PRLM-FICA-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PRLM-FICA-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PRLM-FICA-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 120 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3220-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF PRLT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PRLT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO PRLM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * PR -> AD WHEN LAST ACT DATE IS PAST
      * AD -> VD WHEN LAST ACT DATE IS PAST
      * VD -> TX WHEN LAST ACT DATE IS PAST
      * TX -> DP WHEN LAST ACT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3230-SET-TRAN.
           EVALUATE PRLT-TRAN-CD
               WHEN 'PR'
                   IF PRLM-LAST-ACT-DATE > 0 AND PRLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AD' TO PRLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AD'
                   IF PRLM-LAST-ACT-DATE > 0 AND PRLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VD' TO PRLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VD'
                   IF PRLM-LAST-ACT-DATE > 0 AND PRLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TX' TO PRLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TX'
                   IF PRLM-LAST-ACT-DATE > 0 AND PRLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DP' TO PRLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE FICA YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO NET YTD.
      *----------------------------------------------------------------*
       3240-PRORATE-NET.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PRLM-NET-YTD ROUNDED =
                   PRLM-FICA-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PRLM-NET-YTD
           END-IF
           ADD PRLM-NET-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3250-DESCRIBE-CHANNEL.
           EVALUATE PRLT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 365 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3260-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF PRLM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PRLM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO PRLM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E427 NAME REQUIRED
      * E850 FED TAX AMT NEGATIVE
      * E661 FED TAX AMT OVER 250000
      * E144 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3270-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PRLM-NAME = SPACES
                   MOVE 'E427' TO WS-REJECT-CODE
               WHEN PRLM-FED-TAX-AMT < 0
                   MOVE 'E850' TO WS-REJECT-CODE
               WHEN PRLM-FED-TAX-AMT > 250000.00
                   MOVE 'E661' TO WS-REJECT-CODE
               WHEN PRLM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E144' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3280-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF PRLM-CLOSE-DATE > 0 AND PRLM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PRLM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PRLT-STATE-TAX-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD PRLT-STATE-TAX-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD PRLT-STATE-TAX-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PRLT-STATE-TAX-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PRLT-STATE-TAX-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE FED TAX FEE ON THE NET YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3290-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN PRLM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO PRLM-FED-TAX-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PRLM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE PRLM-FED-TAX-AMT ROUNDED = PRLM-NET-YTD *
                       PRLM-FEE-PCT / 200
               WHEN PRLM-REGION-CD = 'DP'
                   MOVE 5.00 TO PRLM-FED-TAX-AMT
               WHEN PRLM-REGION-CD = 'TU'
                   MOVE 20.00 TO PRLM-FED-TAX-AMT
               WHEN OTHER
                   COMPUTE PRLM-FED-TAX-AMT ROUNDED = PRLM-NET-YTD *
                       PRLM-FEE-PCT / 100
           END-EVALUATE
           IF PRLM-FED-TAX-AMT > 0 AND PRLM-FED-TAX-AMT < 10.00
               MOVE 10.00 TO PRLM-FED-TAX-AMT
           END-IF
           IF PRLM-FED-TAX-AMT > 250.00
               MOVE 250.00 TO PRLM-FED-TAX-AMT
           END-IF
           ADD PRLM-FED-TAX-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E988 ADDR REQUIRED
      * E544 FICA AMT NEGATIVE
      * E581 FICA AMT OVER 250000
      * E760 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3300-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PRLM-ADDR = SPACES
                   MOVE 'E988' TO WS-REJECT-CODE
               WHEN PRLM-FICA-AMT < 0
                   MOVE 'E544' TO WS-REJECT-CODE
               WHEN PRLM-FICA-AMT > 250000.00
                   MOVE 'E581' TO WS-REJECT-CODE
               WHEN PRLM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E760' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT NET AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3310-SPLIT-NET.
           COMPUTE WS-WORK-AMT = PRLM-NET-AMT / 12
           COMPUTE WS-WORK-AMT-2 = PRLM-NET-AMT - (WS-WORK-AMT * 12)
           COMPUTE PRLM-FICA-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE NET AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3320-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PRLM-REGION-CD
                   ADD PRLM-NET-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PRLM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PRLM-NET-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE GROSS BAND TIER FOR THE GROSS AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3330-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-GROSS-BAND-RATE(1) TO PRLM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF PRLM-GROSS-AMT NOT < WS-GROSS-BAND-LOW(WS-TX)
                   MOVE WS-GROSS-BAND-RATE(WS-TX) TO PRLM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PRLM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PRLM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FICA AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3340-SPLIT-FICA.
           COMPUTE WS-WORK-AMT = PRLT-FICA-AMT / 12
           COMPUTE WS-WORK-AMT-2 = PRLT-FICA-AMT - (WS-WORK-AMT * 12)
           COMPUTE PRLM-FED-TAX-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E352 REF NO REQUIRED
      * E603 NET AMT NEGATIVE
      * E631 NET AMT OVER 50000
      * E562 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3350-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PRLM-REF-NO = SPACES
                   MOVE 'E352' TO WS-REJECT-CODE
               WHEN PRLT-NET-AMT < 0
                   MOVE 'E603' TO WS-REJECT-CODE
               WHEN PRLT-NET-AMT > 50000.00
                   MOVE 'E631' TO WS-REJECT-CODE
               WHEN PRLM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E562' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E658 REF NO REQUIRED
      * E105 NET YTD NEGATIVE
      * E146 NET YTD OVER 250000
      * E958 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3360-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PRLM-REF-NO = SPACES
                   MOVE 'E658' TO WS-REJECT-CODE
               WHEN PRLM-NET-YTD < 0
                   MOVE 'E105' TO WS-REJECT-CODE
               WHEN PRLM-NET-YTD > 250000.00
                   MOVE 'E146' TO WS-REJECT-CODE
               WHEN PRLM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E958' TO WS-REJECT-CODE
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
       3370-DESCRIBE-TRAN.
           EVALUATE PRLT-TRAN-CD
               WHEN 'PR'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'AD'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'VD'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'TX'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'DP'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DEDUCT AMT IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3380-LIMIT-DEDUCT.
           IF PRLT-DEDUCT-AMT < 50.00
               MOVE 50.00 TO PRLT-DEDUCT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PRLT-DEDUCT-AMT > 10000.00
               MOVE 10000.00 TO PRLT-DEDUCT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3390-COUNT-SEQ.
           IF PRLT-SEQ < 9999999
               ADD 1 TO PRLT-SEQ
           END-IF
           IF PRLT-SEQ NOT < 12
               MOVE 'Y' TO PRLM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3400-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF PRLM-EXPIRY-DATE > 0 AND PRLM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PRLM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PRLM-STATE-TAX-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD PRLM-STATE-TAX-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD PRLM-STATE-TAX-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PRLM-STATE-TAX-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PRLM-STATE-TAX-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FICA AMT IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3410-LIMIT-FICA.
           IF PRLT-FICA-AMT < 25.00
               MOVE 25.00 TO PRLT-FICA-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PRLT-FICA-AMT > 50000.00
               MOVE 50000.00 TO PRLT-FICA-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * PR -> AD WHEN EXPIRY DATE IS PAST
      * AD -> VD WHEN EXPIRY DATE IS PAST
      * VD -> TX WHEN EXPIRY DATE IS PAST
      * TX -> DP WHEN EXPIRY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3420-SET-TRAN.
           EVALUATE PRLT-TRAN-CD
               WHEN 'PR'
                   IF PRLM-EXPIRY-DATE > 0 AND PRLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AD' TO PRLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AD'
                   IF PRLM-EXPIRY-DATE > 0 AND PRLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VD' TO PRLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VD'
                   IF PRLM-EXPIRY-DATE > 0 AND PRLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TX' TO PRLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TX'
                   IF PRLM-EXPIRY-DATE > 0 AND PRLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DP' TO PRLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3430-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF PRLM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PRLM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO PRLM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3440-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF PRLM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PRLM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO PRLM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3450-LIMIT-ITEM.
           IF WS-WORK-AMT < 0.00
               MOVE 0.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 10000.00
               MOVE 10000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NET YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3460-FLAG-NET.
           IF PRLM-NET-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PRLM-NET-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * MV -> RD WHEN LAST ACT DATE IS PAST
      * RD -> ST WHEN LAST ACT DATE IS PAST
      * ST -> TJ WHEN LAST ACT DATE IS PAST
      * TJ -> UC WHEN LAST ACT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3470-SET-STATUS.
           EVALUATE PRLM-STATUS-CD
               WHEN 'MV'
                   IF PRLM-LAST-ACT-DATE > 0 AND PRLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RD' TO PRLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RD'
                   IF PRLM-LAST-ACT-DATE > 0 AND PRLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ST' TO PRLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ST'
                   IF PRLM-LAST-ACT-DATE > 0 AND PRLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TJ' TO PRLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TJ'
                   IF PRLM-LAST-ACT-DATE > 0 AND PRLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UC' TO PRLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE FICA YTD FEE ON THE STATE TAX AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3480-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN PRLM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO PRLM-FICA-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PRLM-SUSPEND-FLAG = 'Y'
                   COMPUTE PRLM-FICA-YTD ROUNDED = PRLT-STATE-TAX-AMT *
                       PRLM-FEE-PCT / 200
               WHEN PRLT-CHANNEL-CD = 'BRN'
                   MOVE 3.00 TO PRLM-FICA-YTD
               WHEN PRLT-CHANNEL-CD = 'BAT'
                   MOVE 5.00 TO PRLM-FICA-YTD
               WHEN OTHER
                   COMPUTE PRLM-FICA-YTD ROUNDED = PRLT-STATE-TAX-AMT *
                       PRLM-FEE-PCT / 100
           END-EVALUATE
           IF PRLM-FICA-YTD > 0 AND PRLM-FICA-YTD < 5.00
               MOVE 5.00 TO PRLM-FICA-YTD
           END-IF
           IF PRLM-FICA-YTD > 75.00
               MOVE 75.00 TO PRLM-FICA-YTD
           END-IF
           ADD PRLM-FICA-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3490-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF PRLT-EFF-DATE > 0 AND PRLT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PRLT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3500-COUNT-NOTICE.
           IF PRLM-NOTICE-CNT < 9999999
               ADD 1 TO PRLM-NOTICE-CNT
           END-IF
           IF PRLM-NOTICE-CNT NOT < 10
               MOVE 'Y' TO PRLM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATE TAX YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3510-FLAG-STATE-TAX.
           IF PRLM-STATE-TAX-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PRLM-STATE-TAX-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-45 AND OVER 45 DAYS.
      *----------------------------------------------------------------*
       3520-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF PRLM-LAST-ACT-DATE > 0 AND PRLM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PRLM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PRLT-NET-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PRLT-NET-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD PRLT-NET-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 45
                   ADD PRLT-NET-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PRLT-NET-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE NET AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3530-PRORATE-NET.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PRLM-NET-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E639 MEMO REQUIRED
      * E682 DEDUCT AMT NEGATIVE
      * E292 DEDUCT AMT OVER 25000
      * E304 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3540-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PRLT-MEMO = SPACES
                   MOVE 'E639' TO WS-REJECT-CODE
               WHEN PRLT-DEDUCT-AMT < 0
                   MOVE 'E682' TO WS-REJECT-CODE
               WHEN PRLT-DEDUCT-AMT > 25000.00
                   MOVE 'E292' TO WS-REJECT-CODE
               WHEN PRLM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E304' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FED TAX AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3550-ACCRUE-FED-TAX.
           IF PRLM-FED-TAX-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PRLM-FED-TAX-AMT * PRLM-RATE / 100 / 365
               ADD WS-WORK-AMT TO PRLM-DEDUCT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FED TAX YTD IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3560-LIMIT-FED-TAX.
           IF PRLM-FED-TAX-YTD < 25.00
               MOVE 25.00 TO PRLM-FED-TAX-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PRLM-FED-TAX-YTD > 1000.00
               MOVE 1000.00 TO PRLM-FED-TAX-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT NET AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3570-SPLIT-NET.
           COMPUTE WS-WORK-AMT = PRLM-NET-AMT / 12
           COMPUTE WS-WORK-AMT-2 = PRLM-NET-AMT - (WS-WORK-AMT * 12)
           COMPUTE PRLM-GROSS-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * NY -> PL WHEN OPEN DATE IS PAST
      * PL -> VK WHEN OPEN DATE IS PAST
      * VK -> WB WHEN OPEN DATE IS PAST
      * WB -> XL WHEN OPEN DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3580-SET-SOURCE.
           EVALUATE PRLM-SOURCE-CD
               WHEN 'NY'
                   IF PRLM-OPEN-DATE > 0 AND PRLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PL' TO PRLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PL'
                   IF PRLM-OPEN-DATE > 0 AND PRLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VK' TO PRLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VK'
                   IF PRLM-OPEN-DATE > 0 AND PRLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WB' TO PRLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WB'
                   IF PRLM-OPEN-DATE > 0 AND PRLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XL' TO PRLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3590-COUNT-CYCLE.
           IF PRLM-CYCLE-CNT < 9999999
               ADD 1 TO PRLM-CYCLE-CNT
           END-IF
           IF PRLM-CYCLE-CNT NOT < 12
               MOVE 'Y' TO PRLM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3600-COUNT-SEQ.
           IF PRLT-SEQ < 9999999
               ADD 1 TO PRLT-SEQ
           END-IF
           IF PRLT-SEQ NOT < 5
               MOVE 'Y' TO PRLM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON NET YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3610-ACCRUE-NET.
           IF PRLM-NET-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PRLM-NET-YTD * PRLM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO PRLM-FED-TAX-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 60 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3620-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF PRLM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PRLM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO PRLM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DEDUCT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO NET YTD.
      *----------------------------------------------------------------*
       3630-PRORATE-NET.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PRLM-NET-YTD ROUNDED =
                   PRLM-DEDUCT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PRLM-NET-YTD
           END-IF
           ADD PRLM-NET-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO NET YTD.
      *----------------------------------------------------------------*
       3640-PRORATE-NET.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PRLM-NET-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PRLM-NET-YTD
           END-IF
           ADD PRLM-NET-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE NET AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FICA AMT.
      *----------------------------------------------------------------*
       3650-PRORATE-FICA.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PRLT-FICA-AMT ROUNDED =
                   PRLT-NET-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PRLT-FICA-AMT
           END-IF
           ADD PRLT-FICA-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FICA AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3660-FLAG-FICA.
           IF PRLM-FICA-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PRLM-FICA-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3670-DESCRIBE-SOURCE.
           EVALUATE PRLM-SOURCE-CD
               WHEN 'NY'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'PL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'VK'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'WB'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'XL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3680-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 1000.00
               MOVE 1000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3690-DESCRIBE-SOURCE.
           EVALUATE PRLM-SOURCE-CD
               WHEN 'NY'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'PL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'VK'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'WB'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'XL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE DEDUCT FEE ON THE NET AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3700-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN PRLM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO PRLT-DEDUCT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PRLT-REVERSAL-FLAG = 'Y'
                   COMPUTE PRLT-DEDUCT-AMT ROUNDED = PRLM-NET-AMT *
                       PRLM-FEE-PCT / 200
               WHEN PRLT-CHANNEL-CD = 'BRN'
                   MOVE 10.00 TO PRLT-DEDUCT-AMT
               WHEN PRLT-CHANNEL-CD = 'BAT'
                   MOVE 5.00 TO PRLT-DEDUCT-AMT
               WHEN OTHER
                   COMPUTE PRLT-DEDUCT-AMT ROUNDED = PRLM-NET-AMT *
                       PRLM-FEE-PCT / 100
           END-EVALUATE
           IF PRLT-DEDUCT-AMT > 0 AND PRLT-DEDUCT-AMT < 2.00
               MOVE 2.00 TO PRLT-DEDUCT-AMT
           END-IF
           IF PRLT-DEDUCT-AMT > 150.00
               MOVE 150.00 TO PRLT-DEDUCT-AMT
           END-IF
           ADD PRLT-DEDUCT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DEDUCT YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3710-FLAG-DEDUCT.
           IF PRLM-DEDUCT-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PRLM-DEDUCT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FICA AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3720-ACCRUE-FICA.
           IF PRLM-FICA-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PRLM-FICA-AMT * PRLM-RATE / 100 / 365
               ADD WS-WORK-AMT TO PRLM-FED-TAX-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATE TAX AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3730-FLAG-STATE-TAX.
           IF PRLT-STATE-TAX-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PRLT-STATE-TAX-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3740-COUNT-ITEM.
           IF PRLM-ITEM-CNT < 9999999
               ADD 1 TO PRLM-ITEM-CNT
           END-IF
           IF PRLM-ITEM-CNT NOT < 6
               MOVE 'Y' TO PRLM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON GROSS YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3750-ACCRUE-GROSS.
           IF PRLM-GROSS-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PRLM-GROSS-YTD * PRLM-RATE / 100 / 360
               ADD WS-WORK-AMT TO PRLM-GROSS-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NET AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3760-FLAG-NET.
           IF PRLT-NET-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PRLT-NET-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DEDUCT GRID TIER FOR THE FICA AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3770-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DEDUCT-GRID-RATE(1) TO PRLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF PRLM-FICA-AMT NOT < WS-DEDUCT-GRID-LOW(WS-TX)
                   MOVE WS-DEDUCT-GRID-RATE(WS-TX) TO PRLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PRLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PRLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3780-COUNT-NOTICE.
           IF PRLM-NOTICE-CNT < 9999999
               ADD 1 TO PRLM-NOTICE-CNT
           END-IF
           IF PRLM-NOTICE-CNT NOT < 10
               MOVE 'Y' TO PRLM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FICA YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3790-FLAG-FICA.
           IF PRLM-FICA-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PRLM-FICA-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * B -> C WHEN NEXT DUE DATE IS PAST
      * C -> G WHEN NEXT DUE DATE IS PAST
      * G -> H WHEN NEXT DUE DATE IS PAST
      * H -> L WHEN NEXT DUE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3800-SET-REASON.
           EVALUATE PRLM-REASON-CD
               WHEN 'B'
                   IF PRLM-NEXT-DUE-DATE > 0 AND PRLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'C' TO PRLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'C'
                   IF PRLM-NEXT-DUE-DATE > 0 AND PRLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'G' TO PRLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'G'
                   IF PRLM-NEXT-DUE-DATE > 0 AND PRLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO PRLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF PRLM-NEXT-DUE-DATE > 0 AND PRLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO PRLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3810-DESCRIBE-REASON.
           EVALUATE PRLM-REASON-CD
               WHEN 'B'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'C'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'G'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'H'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'L'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'T'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'X'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3820-COUNT-NOTICE.
           IF PRLM-NOTICE-CNT < 9999999
               ADD 1 TO PRLM-NOTICE-CNT
           END-IF
           IF PRLM-NOTICE-CNT NOT < 5
               MOVE 'Y' TO PRLM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E829 REJECT CD REQUIRED
      * E615 NET AMT NEGATIVE
      * E191 NET AMT OVER 10000
      * E599 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3830-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PRLT-REJECT-CD = SPACES
                   MOVE 'E829' TO WS-REJECT-CODE
               WHEN PRLM-NET-AMT < 0
                   MOVE 'E615' TO WS-REJECT-CODE
               WHEN PRLM-NET-AMT > 10000.00
                   MOVE 'E191' TO WS-REJECT-CODE
               WHEN PRLM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E599' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * GROSS AMT IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3840-LIMIT-GROSS.
           IF PRLM-GROSS-AMT < 10.00
               MOVE 10.00 TO PRLM-GROSS-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PRLM-GROSS-AMT > 500.00
               MOVE 500.00 TO PRLM-GROSS-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE NET AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3850-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PRLT-TRAN-CD
                   ADD PRLT-NET-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PRLT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PRLT-NET-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE STATE TAX AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FED TAX YTD.
      *----------------------------------------------------------------*
       3860-PRORATE-FED-TAX.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PRLM-FED-TAX-YTD ROUNDED =
                   PRLT-STATE-TAX-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PRLM-FED-TAX-YTD
           END-IF
           ADD PRLM-FED-TAX-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E968 CUST ID REQUIRED
      * E939 FICA YTD NEGATIVE
      * E800 FICA YTD OVER 5000
      * E489 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3870-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PRLM-CUST-ID = SPACES
                   MOVE 'E968' TO WS-REJECT-CODE
               WHEN PRLM-FICA-YTD < 0
                   MOVE 'E939' TO WS-REJECT-CODE
               WHEN PRLM-FICA-YTD > 5000.00
                   MOVE 'E800' TO WS-REJECT-CODE
               WHEN PRLM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E489' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TAX STATE CD:
      * CAZ -> LZU WHEN NEXT DUE DATE IS PAST
      * LZU -> TUF WHEN NEXT DUE DATE IS PAST
      * TUF -> WKE WHEN NEXT DUE DATE IS PAST
      * WKE -> CAZ WHEN NEXT DUE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3880-SET-TAX-STATE.
           EVALUATE PRLM-TAX-STATE-CD
               WHEN 'CAZ'
                   IF PRLM-NEXT-DUE-DATE > 0 AND PRLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LZU' TO PRLM-TAX-STATE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LZU'
                   IF PRLM-NEXT-DUE-DATE > 0 AND PRLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TUF' TO PRLM-TAX-STATE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TUF'
                   IF PRLM-NEXT-DUE-DATE > 0 AND PRLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WKE' TO PRLM-TAX-STATE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WKE'
                   IF PRLM-NEXT-DUE-DATE > 0 AND PRLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CAZ' TO PRLM-TAX-STATE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3890-DESCRIBE-REGION.
           EVALUATE PRLM-REGION-CD
               WHEN 'DP'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'TU'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'WD'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3900-FLAG-ITEM.
           IF WS-WORK-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E055 ADDR REQUIRED
      * E392 STATE TAX YTD NEGATIVE
      * E371 STATE TAX YTD OVER 250000
      * E755 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3910-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PRLM-ADDR = SPACES
                   MOVE 'E055' TO WS-REJECT-CODE
               WHEN PRLM-STATE-TAX-YTD < 0
                   MOVE 'E392' TO WS-REJECT-CODE
               WHEN PRLM-STATE-TAX-YTD > 250000.00
                   MOVE 'E371' TO WS-REJECT-CODE
               WHEN PRLT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E755' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3920-COUNT-NOTICE.
           IF PRLM-NOTICE-CNT < 9999999
               ADD 1 TO PRLM-NOTICE-CNT
           END-IF
           IF PRLM-NOTICE-CNT NOT < 10
               MOVE 'Y' TO PRLM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE NET AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3930-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PRLM-SOURCE-CD
                   ADD PRLM-NET-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PRLM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PRLM-NET-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATE TAX AMT IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3940-LIMIT-STATE-TAX.
           IF PRLM-STATE-TAX-AMT < 50.00
               MOVE 50.00 TO PRLM-STATE-TAX-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PRLM-STATE-TAX-AMT > 500.00
               MOVE 500.00 TO PRLM-STATE-TAX-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3950-DESCRIBE-CHANNEL.
           EVALUATE PRLT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * GROSS AMT IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3960-LIMIT-GROSS.
           IF PRLM-GROSS-AMT < 25.00
               MOVE 25.00 TO PRLM-GROSS-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PRLM-GROSS-AMT > 2500.00
               MOVE 2500.00 TO PRLM-GROSS-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DEDUCT SCHED TIER FOR THE FED TAX AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3970-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DEDUCT-SCHED-RATE(1) TO PRLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF PRLM-FED-TAX-AMT NOT < WS-DEDUCT-SCHED-LOW(WS-TX)
                   MOVE WS-DEDUCT-SCHED-RATE(WS-TX) TO PRLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PRLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PRLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3980-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF PRLM-LAST-ACT-DATE > 0 AND PRLM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PRLM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PRLT-FICA-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD PRLT-FICA-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD PRLT-FICA-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PRLT-FICA-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PRLT-FICA-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE DEDUCT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3990-PRORATE-DEDUCT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   PRLM-DEDUCT-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4000-DESCRIBE-REGION.
           EVALUATE PRLM-REGION-CD
               WHEN 'DP'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'TU'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'WD'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E498 REJECT CD REQUIRED
      * E911 STATE TAX YTD NEGATIVE
      * E947 STATE TAX YTD OVER 10000
      * E022 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       4010-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PRLT-REJECT-CD = SPACES
                   MOVE 'E498' TO WS-REJECT-CODE
               WHEN PRLM-STATE-TAX-YTD < 0
                   MOVE 'E911' TO WS-REJECT-CODE
               WHEN PRLM-STATE-TAX-YTD > 10000.00
                   MOVE 'E947' TO WS-REJECT-CODE
               WHEN PRLM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E022' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATE TAX YTD IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4020-LIMIT-STATE-TAX.
           IF PRLM-STATE-TAX-YTD < 10.00
               MOVE 10.00 TO PRLM-STATE-TAX-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PRLM-STATE-TAX-YTD > 500.00
               MOVE 500.00 TO PRLM-STATE-TAX-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FICA AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4030-FLAG-FICA.
           IF PRLT-FICA-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PRLT-FICA-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATE TAX AMT IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4040-LIMIT-STATE-TAX.
           IF PRLT-STATE-TAX-AMT < 50.00
               MOVE 50.00 TO PRLT-STATE-TAX-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PRLT-STATE-TAX-AMT > 2500.00
               MOVE 2500.00 TO PRLT-STATE-TAX-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FICA YTD IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4050-LIMIT-FICA.
           IF PRLM-FICA-YTD < 10.00
               MOVE 10.00 TO PRLM-FICA-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PRLM-FICA-YTD > 50000.00
               MOVE 50000.00 TO PRLM-FICA-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E763 NAME REQUIRED
      * E045 FED TAX YTD NEGATIVE
      * E021 FED TAX YTD OVER 25000
      * E986 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       4060-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PRLM-NAME = SPACES
                   MOVE 'E763' TO WS-REJECT-CODE
               WHEN PRLM-FED-TAX-YTD < 0
                   MOVE 'E045' TO WS-REJECT-CODE
               WHEN PRLM-FED-TAX-YTD > 25000.00
                   MOVE 'E021' TO WS-REJECT-CODE
               WHEN PRLT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E986' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT GROSS AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4070-SPLIT-GROSS.
           COMPUTE WS-WORK-AMT = PRLM-GROSS-AMT / 2
           COMPUTE WS-WORK-AMT-2 = PRLM-GROSS-AMT - (WS-WORK-AMT * 2)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4080-DESCRIBE-REASON.
           EVALUATE PRLM-REASON-CD
               WHEN 'B'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'C'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'G'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'H'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'L'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'T'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'X'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E369 OPERATOR REQUIRED
      * E394 FED TAX YTD NEGATIVE
      * E522 FED TAX YTD OVER 10000
      * E757 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       4090-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PRLT-OPERATOR = SPACES
                   MOVE 'E369' TO WS-REJECT-CODE
               WHEN PRLM-FED-TAX-YTD < 0
                   MOVE 'E394' TO WS-REJECT-CODE
               WHEN PRLM-FED-TAX-YTD > 10000.00
                   MOVE 'E522' TO WS-REJECT-CODE
               WHEN PRLM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E757' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE PRL FIELDS TO PYPEN AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7040-CALL-PYPEN.
           INITIALIZE PYPEN-PARMS
           MOVE PRLM-NET-AMT
             TO PYPEN-NET-AMT
           MOVE PRLM-FED-TAX-YTD
             TO PYPEN-FED-TAX-YTD
           MOVE PRLM-GROSS-AMT
             TO PYPEN-GROSS-AMT
           MOVE PRLM-DEDUCT-YTD
             TO PYPEN-DEDUCT-YTD
           MOVE PRLM-FICA-YTD
             TO PYPEN-FICA-YTD
           MOVE PRLM-STATE-TAX-YTD
             TO PYPEN-STATE-TAX-YTD
           MOVE PRLM-EFFECTIVE-DATE
             TO PYPEN-EFFECTIVE-DATE
           MOVE PRLM-CLOSE-DATE
             TO PYPEN-CLOSE-DATE
           MOVE PRLM-NEXT-DUE-DATE
             TO PYPEN-NEXT-DUE-DATE
           MOVE PRLM-EXPIRY-DATE
             TO PYPEN-EXPIRY-DATE
           MOVE PRLM-SOURCE-CD
             TO PYPEN-SOURCE-CD
           MOVE PRLM-REASON-CD
             TO PYPEN-REASON-CD
           MOVE PRLM-PAY-FREQ-CD
             TO PYPEN-PAY-FREQ-CD
           MOVE PRLM-TAX-STATE-CD
             TO PYPEN-TAX-STATE-CD
           MOVE PRLM-PREFERRED-FLAG
             TO PYPEN-PREFERRED-FLAG
           MOVE PRLM-SUSPEND-FLAG
             TO PYPEN-SUSPEND-FLAG
           MOVE PRLM-STOP-FLAG
             TO PYPEN-STOP-FLAG
           MOVE PRLM-PAYMENT-CNT
             TO PYPEN-PAYMENT-CNT
           MOVE PRLM-CYCLE-CNT
             TO PYPEN-CYCLE-CNT
           MOVE PRLM-MARGIN
             TO PYPEN-MARGIN
           MOVE PRLM-RATE
             TO PYPEN-RATE
           MOVE PRLM-CUST-ID
             TO PYPEN-CUST-ID
           MOVE PRLM-REF-NO
             TO PYPEN-REF-NO
           CALL 'PYPEN' USING PYPEN-PARMS
           EVALUATE PYPEN-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE PYPEN-NET-AMT
                     TO PRLM-NET-AMT
                   MOVE PYPEN-GROSS-AMT
                     TO PRLM-GROSS-AMT
                   CONTINUE
               WHEN 08
                   MOVE 'RPEN' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7040-CALL-PYPEN' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE PRL FIELDS TO PYRISK AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7030-CALL-PYRISK.
           INITIALIZE PYRISK-PARMS
           MOVE PRLM-GROSS-YTD
             TO PYRISK-GROSS-YTD
           MOVE PRLM-DEDUCT-AMT
             TO PYRISK-DEDUCT-AMT
           MOVE PRLM-FICA-AMT
             TO PYRISK-FICA-AMT
           MOVE PRLM-STATE-TAX-YTD
             TO PYRISK-STATE-TAX-YTD
           MOVE PRLM-DEDUCT-YTD
             TO PYRISK-DEDUCT-YTD
           MOVE PRLM-GROSS-AMT
             TO PYRISK-GROSS-AMT
           MOVE PRLM-NEXT-DUE-DATE
             TO PYRISK-NEXT-DUE-DATE
           MOVE PRLM-EFFECTIVE-DATE
             TO PYRISK-EFFECTIVE-DATE
           MOVE PRLM-OPEN-DATE
             TO PYRISK-OPEN-DATE
           MOVE PRLM-NEXT-REVIEW-DATE
             TO PYRISK-NEXT-REVIEW-DATE
           MOVE PRLM-REGION-CD
             TO PYRISK-REGION-CD
           MOVE PRLM-TAX-STATE-CD
             TO PYRISK-TAX-STATE-CD
           MOVE PRLM-STATUS-CD
             TO PYRISK-STATUS-CD
           MOVE PRLM-SOURCE-CD
             TO PYRISK-SOURCE-CD
           MOVE PRLM-STOP-FLAG
             TO PYRISK-STOP-FLAG
           MOVE PRLM-NOTICE-SENT-FLAG
             TO PYRISK-NOTICE-SENT-FLAG
           MOVE PRLM-AUTO-RENEW-FLAG
             TO PYRISK-AUTO-RENEW-FLAG
           MOVE PRLM-CYCLE-CNT
             TO PYRISK-CYCLE-CNT
           MOVE PRLM-NOTICE-CNT
             TO PYRISK-NOTICE-CNT
           MOVE PRLM-RATE
             TO PYRISK-RATE
           MOVE PRLM-MARGIN
             TO PYRISK-MARGIN
           MOVE PRLM-NAME
             TO PYRISK-NAME
           MOVE PRLM-REF-NO
             TO PYRISK-REF-NO
           CALL 'PYRISK' USING PYRISK-PARMS
           EVALUATE PYRISK-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE PYRISK-GROSS-YTD
                     TO PRLM-GROSS-YTD
                   MOVE PYRISK-DEDUCT-AMT
                     TO PRLM-DEDUCT-AMT
                   MOVE PYRISK-STATE-TAX-YTD
                     TO PRLM-STATE-TAX-YTD
                   MOVE PYRISK-STOP-FLAG
                     TO PRLM-STOP-FLAG
                   MOVE PYRISK-NOTICE-SENT-FLAG
                     TO PRLM-NOTICE-SENT-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RISK' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7030-CALL-PYRISK' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE PRL FIELDS TO PYPRC AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7000-CALL-PYPRC.
           INITIALIZE PYPRC-PARMS
           MOVE PRLM-STATE-TAX-YTD
             TO PYPRC-STATE-TAX-YTD
           MOVE PRLM-FED-TAX-AMT
             TO PYPRC-FED-TAX-AMT
           MOVE PRLM-NET-AMT
             TO PYPRC-NET-AMT
           MOVE PRLM-STATE-TAX-AMT
             TO PYPRC-STATE-TAX-AMT
           MOVE PRLM-FICA-YTD
             TO PYPRC-FICA-YTD
           MOVE PRLM-FED-TAX-YTD
             TO PYPRC-FED-TAX-YTD
           MOVE PRLM-NEXT-DUE-DATE
             TO PYPRC-NEXT-DUE-DATE
           MOVE PRLM-LAST-PAY-DATE
             TO PYPRC-LAST-PAY-DATE
           MOVE PRLM-OPEN-DATE
             TO PYPRC-OPEN-DATE
           MOVE PRLM-EXPIRY-DATE
             TO PYPRC-EXPIRY-DATE
           MOVE PRLM-TAX-STATE-CD
             TO PYPRC-TAX-STATE-CD
           MOVE PRLM-STATUS-CD
             TO PYPRC-STATUS-CD
           MOVE PRLM-SOURCE-CD
             TO PYPRC-SOURCE-CD
           MOVE PRLM-REGION-CD
             TO PYPRC-REGION-CD
           MOVE PRLM-STOP-FLAG
             TO PYPRC-STOP-FLAG
           MOVE PRLM-AUTO-RENEW-FLAG
             TO PYPRC-AUTO-RENEW-FLAG
           MOVE PRLM-PREFERRED-FLAG
             TO PYPRC-PREFERRED-FLAG
           MOVE PRLM-NOTICE-CNT
             TO PYPRC-NOTICE-CNT
           MOVE PRLM-ITEM-CNT
             TO PYPRC-ITEM-CNT
           MOVE PRLM-FEE-PCT
             TO PYPRC-FEE-PCT
           MOVE PRLM-MARGIN
             TO PYPRC-MARGIN
           MOVE PRLM-CUST-ID
             TO PYPRC-CUST-ID
           MOVE PRLM-CONTACT
             TO PYPRC-CONTACT
           CALL 'PYPRC' USING PYPRC-PARMS
           EVALUATE PYPRC-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE PYPRC-STATE-TAX-YTD
                     TO PRLM-STATE-TAX-YTD
                   MOVE PYPRC-FED-TAX-AMT
                     TO PRLM-FED-TAX-AMT
                   MOVE PYPRC-STOP-FLAG
                     TO PRLM-STOP-FLAG
                   MOVE PYPRC-AUTO-RENEW-FLAG
                     TO PRLM-AUTO-RENEW-FLAG
                   MOVE PYPRC-PREFERRED-FLAG
                     TO PRLM-PREFERRED-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RPRC' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-PYPRC' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE PRLT-KEY TO RD-KEY
           MOVE PRLT-TRAN-CD TO RD-CODE
           MOVE PRLT-DEDUCT-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE PRLT-KEY TO RD-KEY
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
           DISPLAY 'PYLEXTR READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'PYLEXTR' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
