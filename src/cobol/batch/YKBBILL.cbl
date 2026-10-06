      *================================================================*
      * PROGRAM  : YKBBILL                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : DATE WINDOW BRIDGE - CYCLE BILLING. READS THE YDB   *
      *            TRANSACTIONS, UPDATES THE YDB MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. TWO-DIGIT YEAR WINDOWING BRIDGE *
      *            FOR CENTURY DATE CONVERSION                         *
      * INPUT    : YDBMST (KSDS), YDBTRN, BANKCTL                      *
      * OUTPUT   : YDBOUT, YKBBIRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, YKPRC, YKLIM, YKWAIV    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-09-14 EWS  ORIGINAL                                       *
      * 1999-01-16 RJT  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2001-02-02 TWH  AUDIT FINDING - AMOUNT-2 CONTROL TOTALS CHG6117*
      * 2002-07-22 SKN  CONTROL FEE SCHEDULE UPDATE PRB4180            *
      * 2004-06-01 CRP  AMOUNT-2 STATUS CODES ADDED CHG1297            *
      * 2006-02-07 BHS  CONTROL STATUS CODES ADDED CHG5333             *
      * 2006-02-19 TWH  CONTROL LIMIT REVISED CHG5488                  *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YKBBILL.
       AUTHOR. T W HOLT.

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
           SELECT REPORT-FILE ASSIGN TO "YKBBIRPT"
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
                                           VALUE 13.000000.
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
      * WS-AMOUNT-2-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-AMOUNT-2-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000709000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000614000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020000000000448500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020050000000552000'.
       01  WS-AMOUNT-2-GRID-TBL REDEFINES WS-AMOUNT-2-GRID-VALUES.
           05  WS-AMOUNT-2-GRID-ROW OCCURS 4 TIMES.
               10  WS-AMOUNT-2-GRID-LOW    PIC 9(09)V99.
               10  WS-AMOUNT-2-GRID-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-AMOUNT-2-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-AMOUNT-2-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000092500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000512000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001250000000851000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006250000000345500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006500000000767500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00009000000000788500'.
       01  WS-AMOUNT-2-TIER-TBL REDEFINES WS-AMOUNT-2-TIER-VALUES.
           05  WS-AMOUNT-2-TIER-ROW OCCURS 6 TIMES.
               10  WS-AMOUNT-2-TIER-LOW    PIC 9(09)V99.
               10  WS-AMOUNT-2-TIER-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-AMOUNT-1-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-AMOUNT-1-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000168500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000059000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000402000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005100000000210500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007600000000748500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017600000000533000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017650000000811000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017700000000226500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00027700000000487500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00027950000000760500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00028200000000128000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00028300000000207000'.
       01  WS-AMOUNT-1-BAND-TBL REDEFINES WS-AMOUNT-1-BAND-VALUES.
           05  WS-AMOUNT-1-BAND-ROW OCCURS 12 TIMES.
               10  WS-AMOUNT-1-BAND-LOW    PIC 9(09)V99.
               10  WS-AMOUNT-1-BAND-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-CONTROL-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CONTROL-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000551000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000227000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000750000000638000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005750000000537000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015750000000801000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016250000000153000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021250000000435000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021500000000221500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021600000000237500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00031600000000245500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00031700000000037000'.
       01  WS-CONTROL-SCHED-TBL REDEFINES WS-CONTROL-SCHED-VALUES.
           05  WS-CONTROL-SCHED-ROW OCCURS 11 TIMES.
               10  WS-CONTROL-SCHED-LOW    PIC 9(09)V99.
               10  WS-CONTROL-SCHED-RATE   PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  YKBBILL  DATE WINDOW BRIDGE'.
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
       COPY YKPRCP.
       COPY YKLIMP.
       COPY YKWAIVP.

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
               PERFORM 3000-SPLIT-CONTROL
               PERFORM 3010-SPLIT-CONTROL
               PERFORM 3020-FLAG-CONTROL
               PERFORM 3030-COUNT-SEQ
               PERFORM 3040-CHECK-LAST-MAINT
               PERFORM 3050-COUNT-RETRY
               PERFORM 3060-PRORATE-CONTROL
               PERFORM 3070-FLAG-AMOUNT-2
               PERFORM 3080-SET-TRAN
               PERFORM 3090-PRORATE-CONTROL
               PERFORM 3100-EDIT-DESC
               PERFORM 3110-ACCUM-CHANNEL
               PERFORM 3120-ACCUM-SOURCE
               PERFORM 3130-SET-SOURCE
               PERFORM 3140-ACCUM-SOURCE
               PERFORM 3150-FLAG-AMOUNT-2
               PERFORM 3160-ACCRUE-AMOUNT-2
               PERFORM 3170-COUNT-SEQ
               PERFORM 3180-PRORATE-CONTROL
               PERFORM 3190-SELECT-PENALTY-PCT
               PERFORM 3200-CHECK-NEXT-DUE
               PERFORM 3210-EDIT-REJECT
               PERFORM 3220-ACCUM-CHANNEL
               PERFORM 3230-ASSESS-OVERRIDE
               PERFORM 3240-COUNT-ITEM
               PERFORM 3250-COUNT-RETRY
               PERFORM 3260-SET-CHANNEL
               PERFORM 3270-SET-SOURCE
               PERFORM 3280-COUNT-CYCLE
               PERFORM 3290-ASSESS-OVERRIDE
               PERFORM 3300-PRORATE-AMOUNT-1
               PERFORM 3310-AGE-LAST-ACT
               PERFORM 3320-DESCRIBE-REASON
               PERFORM 3330-COUNT-RETRY
               PERFORM 3340-LIMIT-CONTROL
               PERFORM 3350-ACCUM-CYCLE
               PERFORM 3360-DESCRIBE-PIVOT
               PERFORM 3370-ASSESS-OVERRIDE
               PERFORM 3380-DESCRIBE-WINDOW-RULE
               PERFORM 3390-ACCRUE-AMOUNT-2
               PERFORM 3400-AGE-LAST-STMT
               PERFORM 3410-ACCRUE-CONTROL
               PERFORM 3420-CHECK-POST
               PERFORM 3430-AGE-POST
               PERFORM 3440-DESCRIBE-CYCLE
               PERFORM 3450-ASSESS-PREFERRED
               PERFORM 3460-DESCRIBE-TRAN
               PERFORM 3470-COUNT-PAYMENT
               PERFORM 3480-ACCUM-PIVOT
               PERFORM 3490-EDIT-MEMO
               PERFORM 3500-SET-WINDOW-RULE
               PERFORM 3510-COUNT-SEQ
               PERFORM 3520-DESCRIBE-WINDOW-RULE
               PERFORM 3530-EDIT-DESC
               PERFORM 3540-SPLIT-CONTROL
               PERFORM 3550-ACCUM-WINDOW-RULE
               PERFORM 3560-ACCUM-CHANNEL
               PERFORM 3570-LIMIT-AMOUNT-2
               PERFORM 3580-EDIT-CONTACT
               PERFORM 3590-SPLIT-AMOUNT-2
               PERFORM 3600-COUNT-PAYMENT
               PERFORM 3610-DESCRIBE-WINDOW-RULE
               PERFORM 3620-ACCRUE-CONTROL
               PERFORM 3630-CHECK-EXPIRY
               PERFORM 3640-DESCRIBE-TRAN
               PERFORM 3650-LIMIT-AMOUNT-1
               PERFORM 3660-EDIT-CUST-ID
               PERFORM 3670-DESCRIBE-PIVOT
               PERFORM 3680-ACCUM-TRAN
               PERFORM 3690-SPLIT-AMOUNT-1
               PERFORM 3700-FLAG-CONTROL
               PERFORM 3710-ACCUM-PIVOT
               PERFORM 3720-CHECK-EFF
               PERFORM 3730-FLAG-AMOUNT-1
               PERFORM 3740-ACCRUE-CONTROL
               PERFORM 3750-AGE-LAST-ACT
               PERFORM 3760-PRORATE-AMOUNT-1
               PERFORM 3770-DESCRIBE-REASON
               PERFORM 3780-ASSESS-REVERSAL
               PERFORM 3790-ASSESS-REVIEW
               PERFORM 3800-SELECT-MARGIN
               PERFORM 3810-LIMIT-AMOUNT-2
               PERFORM 3820-FLAG-AMOUNT-1
               PERFORM 3830-SET-REGION
               PERFORM 3840-SELECT-DISCOUNT
               PERFORM 3850-SPLIT-CONTROL
               PERFORM 3860-FLAG-AMOUNT-2
               PERFORM 3870-COUNT-CYCLE
               PERFORM 3880-ACCUM-CYCLE
               PERFORM 3890-COUNT-SEQ
               PERFORM 3900-COUNT-ITEM
               PERFORM 3910-PRORATE-AMOUNT-1
               PERFORM 3920-DESCRIBE-PIVOT
               PERFORM 3930-LIMIT-CONTROL
               PERFORM 3940-SPLIT-CONTROL
               PERFORM 3950-SELECT-PENALTY-PCT
               PERFORM 3960-COUNT-RETRY
               PERFORM 3970-LIMIT-CONTROL
               PERFORM 3980-FLAG-AMOUNT-2
               PERFORM 3990-COUNT-ITEM
               PERFORM 4000-PRORATE-AMOUNT-1
               PERFORM 4010-AGE-OPEN
               PERFORM 4020-FLAG-AMOUNT-2
               PERFORM 4030-SPLIT-AMOUNT-1
               PERFORM 4040-SELECT-MARGIN
               PERFORM 4050-ACCUM-WINDOW-RULE
               PERFORM 4060-COUNT-PAYMENT
               PERFORM 4070-CHECK-LAST-MAINT
               PERFORM 4080-ASSESS-EXEMPT
               PERFORM 4090-CHECK-EXPIRY
               PERFORM 4100-LIMIT-AMOUNT-1
               PERFORM 4110-SET-CYCLE
               PERFORM 4120-ASSESS-EXEMPT
               PERFORM 4130-ASSESS-REVERSAL
               PERFORM 4140-SET-CYCLE
               PERFORM 4150-EDIT-MEMO
               PERFORM 4160-SPLIT-CONTROL
               PERFORM 4170-EDIT-REJECT
               PERFORM 4180-SELECT-MARGIN
               PERFORM 4190-LIMIT-AMOUNT-2
               PERFORM 4200-EDIT-CUST-ID
               PERFORM 4210-SPLIT-AMOUNT-2
               PERFORM 4220-DESCRIBE-PIVOT
               PERFORM 4230-DESCRIBE-TRAN
               PERFORM 4240-EDIT-NAME
               PERFORM 7000-CALL-YKPRC
               PERFORM 7040-CALL-YKLIM
               PERFORM 7030-CALL-YKWAIV
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
      * SPLIT CONTROL YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3000-SPLIT-CONTROL.
           COMPUTE WS-WORK-AMT = YDBM-CONTROL-YTD / 12
           COMPUTE WS-WORK-AMT-2 = YDBM-CONTROL-YTD - (WS-WORK-AMT * 12)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CONTROL YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3010-SPLIT-CONTROL.
           COMPUTE WS-WORK-AMT = YDBM-CONTROL-YTD / 2
           COMPUTE WS-WORK-AMT-2 = YDBM-CONTROL-YTD - (WS-WORK-AMT * 2)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CONTROL YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3020-FLAG-CONTROL.
           IF YDBM-CONTROL-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE YDBM-CONTROL-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3030-COUNT-SEQ.
           IF YDBT-SEQ < 9999999
               ADD 1 TO YDBT-SEQ
           END-IF
           IF YDBT-SEQ NOT < 5
               MOVE 'Y' TO YDBM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 30 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3040-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF YDBM-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YDBM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO YDBM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3050-COUNT-RETRY.
           IF YDBM-RETRY-CNT < 9999999
               ADD 1 TO YDBM-RETRY-CNT
           END-IF
           IF YDBM-RETRY-CNT NOT < 6
               MOVE 'Y' TO YDBM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CONTROL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CONTROL YTD.
      *----------------------------------------------------------------*
       3060-PRORATE-CONTROL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE YDBM-CONTROL-YTD ROUNDED =
                   YDBT-CONTROL-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO YDBM-CONTROL-YTD
           END-IF
           ADD YDBM-CONTROL-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AMOUNT 2 AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3070-FLAG-AMOUNT-2.
           IF YDBM-AMOUNT-2-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE YDBM-AMOUNT-2-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * CV -> RV WHEN LAST PAY DATE IS PAST
      * RV -> TS WHEN LAST PAY DATE IS PAST
      * TS -> CV WHEN LAST PAY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3080-SET-TRAN.
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
      * PRORATE CONTROL YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3090-PRORATE-CONTROL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   YDBM-CONTROL-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E132 DESC REQUIRED
      * E156 CONTROL AMT NEGATIVE
      * E610 CONTROL AMT OVER 5000
      * E500 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3100-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YDBM-DESC = SPACES
                   MOVE 'E132' TO WS-REJECT-CODE
               WHEN YDBM-CONTROL-AMT < 0
                   MOVE 'E156' TO WS-REJECT-CODE
               WHEN YDBM-CONTROL-AMT > 5000.00
                   MOVE 'E610' TO WS-REJECT-CODE
               WHEN YDBM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E500' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE AMOUNT 1 YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3110-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YDBT-CHANNEL-CD
                   ADD YDBM-AMOUNT-1-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YDBT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE YDBM-AMOUNT-1-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CONTROL AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3120-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YDBM-SOURCE-CD
                   ADD YDBT-CONTROL-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YDBM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE YDBT-CONTROL-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * BNX -> FTB WHEN NEXT DUE DATE IS PAST
      * FTB -> GAP WHEN NEXT DUE DATE IS PAST
      * GAP -> PAP WHEN NEXT DUE DATE IS PAST
      * PAP -> VGC WHEN NEXT DUE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3130-SET-SOURCE.
           EVALUATE YDBM-SOURCE-CD
               WHEN 'BNX'
                   IF YDBM-NEXT-DUE-DATE > 0 AND YDBM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FTB' TO YDBM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FTB'
                   IF YDBM-NEXT-DUE-DATE > 0 AND YDBM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GAP' TO YDBM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GAP'
                   IF YDBM-NEXT-DUE-DATE > 0 AND YDBM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PAP' TO YDBM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PAP'
                   IF YDBM-NEXT-DUE-DATE > 0 AND YDBM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VGC' TO YDBM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE CONTROL AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3140-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YDBM-SOURCE-CD
                   ADD YDBM-CONTROL-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YDBM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE YDBM-CONTROL-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT 2 AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3150-FLAG-AMOUNT-2.
           IF YDBM-AMOUNT-2-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE YDBM-AMOUNT-2-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON AMOUNT 2 AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3160-ACCRUE-AMOUNT-2.
           IF YDBT-AMOUNT-2-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   YDBT-AMOUNT-2-AMT * YDBM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO YDBM-AMOUNT-2-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3170-COUNT-SEQ.
           IF YDBT-SEQ < 9999999
               ADD 1 TO YDBT-SEQ
           END-IF
           IF YDBT-SEQ NOT < 10
               MOVE 'Y' TO YDBT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CONTROL AMT.
      *----------------------------------------------------------------*
       3180-PRORATE-CONTROL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE YDBT-CONTROL-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO YDBT-CONTROL-AMT
           END-IF
           ADD YDBT-CONTROL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE AMOUNT 2 TIER TIER FOR THE AMOUNT 2 AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3190-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-AMOUNT-2-TIER-RATE(1) TO YDBM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF YDBT-AMOUNT-2-AMT NOT < WS-AMOUNT-2-TIER-LOW(WS-TX)
                   MOVE WS-AMOUNT-2-TIER-RATE(WS-TX) TO YDBM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF YDBM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO YDBM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3200-CHECK-NEXT-DUE.
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
               MOVE 'Y' TO YDBT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E972 REJECT CD REQUIRED
      * E654 CONTROL YTD NEGATIVE
      * E365 CONTROL YTD OVER 50000
      * E938 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3210-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YDBT-REJECT-CD = SPACES
                   MOVE 'E972' TO WS-REJECT-CODE
               WHEN YDBM-CONTROL-YTD < 0
                   MOVE 'E654' TO WS-REJECT-CODE
               WHEN YDBM-CONTROL-YTD > 50000.00
                   MOVE 'E365' TO WS-REJECT-CODE
               WHEN YDBM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E938' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CONTROL AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3220-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YDBT-CHANNEL-CD
                   ADD YDBM-CONTROL-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YDBT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE YDBM-CONTROL-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE AMOUNT 2 FEE ON THE AMOUNT 2 YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3230-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN YDBM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO YDBM-AMOUNT-2-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YDBM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE YDBM-AMOUNT-2-AMT ROUNDED = YDBM-AMOUNT-2-YTD
                       * YDBM-DISCOUNT / 200
               WHEN YDBM-REASON-CD = 'AX'
                   MOVE 3.00 TO YDBM-AMOUNT-2-AMT
               WHEN YDBM-REASON-CD = 'LK'
                   MOVE 7.00 TO YDBM-AMOUNT-2-AMT
               WHEN OTHER
                   COMPUTE YDBM-AMOUNT-2-AMT ROUNDED = YDBM-AMOUNT-2-YTD
                       * YDBM-DISCOUNT / 100
           END-EVALUATE
           IF YDBM-AMOUNT-2-AMT > 0 AND YDBM-AMOUNT-2-AMT < 1.00
               MOVE 1.00 TO YDBM-AMOUNT-2-AMT
           END-IF
           IF YDBM-AMOUNT-2-AMT > 50.00
               MOVE 50.00 TO YDBM-AMOUNT-2-AMT
           END-IF
           ADD YDBM-AMOUNT-2-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3240-COUNT-ITEM.
           IF YDBM-ITEM-CNT < 9999999
               ADD 1 TO YDBM-ITEM-CNT
           END-IF
           IF YDBM-ITEM-CNT NOT < 10
               MOVE 'Y' TO YDBM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3250-COUNT-RETRY.
           IF YDBM-RETRY-CNT < 9999999
               ADD 1 TO YDBM-RETRY-CNT
           END-IF
           IF YDBM-RETRY-CNT NOT < 12
               MOVE 'Y' TO YDBM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN LAST PAY DATE IS PAST
      * BAT -> MAL WHEN LAST PAY DATE IS PAST
      * MAL -> TEL WHEN LAST PAY DATE IS PAST
      * TEL -> ONL WHEN LAST PAY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3260-SET-CHANNEL.
           EVALUATE YDBT-CHANNEL-CD
               WHEN 'BRN'
                   IF YDBM-LAST-PAY-DATE > 0 AND YDBM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO YDBT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF YDBM-LAST-PAY-DATE > 0 AND YDBM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO YDBT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF YDBM-LAST-PAY-DATE > 0 AND YDBM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO YDBT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF YDBM-LAST-PAY-DATE > 0 AND YDBM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO YDBT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * BNX -> FTB WHEN EXPIRY DATE IS PAST
      * FTB -> GAP WHEN EXPIRY DATE IS PAST
      * GAP -> PAP WHEN EXPIRY DATE IS PAST
      * PAP -> VGC WHEN EXPIRY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3270-SET-SOURCE.
           EVALUATE YDBM-SOURCE-CD
               WHEN 'BNX'
                   IF YDBM-EXPIRY-DATE > 0 AND YDBM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FTB' TO YDBM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FTB'
                   IF YDBM-EXPIRY-DATE > 0 AND YDBM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GAP' TO YDBM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GAP'
                   IF YDBM-EXPIRY-DATE > 0 AND YDBM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PAP' TO YDBM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PAP'
                   IF YDBM-EXPIRY-DATE > 0 AND YDBM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VGC' TO YDBM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3280-COUNT-CYCLE.
           IF YDBM-CYCLE-CNT < 9999999
               ADD 1 TO YDBM-CYCLE-CNT
           END-IF
           IF YDBM-CYCLE-CNT NOT < 5
               MOVE 'Y' TO YDBM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CONTROL YTD FEE ON THE AMOUNT 2 YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3290-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN YDBM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO YDBM-CONTROL-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YDBT-REVERSAL-FLAG = 'Y'
                   COMPUTE YDBM-CONTROL-YTD ROUNDED = YDBM-AMOUNT-2-YTD
                       * YDBM-MARGIN / 200
               WHEN YDBM-SOURCE-CD = 'BNX'
                   MOVE 3.00 TO YDBM-CONTROL-YTD
               WHEN YDBM-SOURCE-CD = 'FTB'
                   MOVE 12.00 TO YDBM-CONTROL-YTD
               WHEN OTHER
                   COMPUTE YDBM-CONTROL-YTD ROUNDED = YDBM-AMOUNT-2-YTD
                       * YDBM-MARGIN / 100
           END-EVALUATE
           IF YDBM-CONTROL-YTD > 0 AND YDBM-CONTROL-YTD < 1.00
               MOVE 1.00 TO YDBM-CONTROL-YTD
           END-IF
           IF YDBM-CONTROL-YTD > 75.00
               MOVE 75.00 TO YDBM-CONTROL-YTD
           END-IF
           ADD YDBM-CONTROL-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE CONTROL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO AMOUNT 1 YTD.
      *----------------------------------------------------------------*
       3300-PRORATE-AMOUNT-1.
           IF WS-PERIOD-DAYS > 0
               COMPUTE YDBM-AMOUNT-1-YTD ROUNDED =
                   YDBT-CONTROL-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO YDBM-AMOUNT-1-YTD
           END-IF
           ADD YDBM-AMOUNT-1-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3310-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF YDBM-LAST-ACT-DATE > 0 AND YDBM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YDBM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD YDBM-CONTROL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD YDBM-CONTROL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD YDBM-CONTROL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD YDBM-CONTROL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD YDBM-CONTROL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3320-DESCRIBE-REASON.
           EVALUATE YDBM-REASON-CD
               WHEN 'AX'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'LK'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'NT'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'UW'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3330-COUNT-RETRY.
           IF YDBM-RETRY-CNT < 9999999
               ADD 1 TO YDBM-RETRY-CNT
           END-IF
           IF YDBM-RETRY-CNT NOT < 10
               MOVE 'Y' TO YDBM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CONTROL AMT IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3340-LIMIT-CONTROL.
           IF YDBT-CONTROL-AMT < 50.00
               MOVE 50.00 TO YDBT-CONTROL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YDBT-CONTROL-AMT > 10000.00
               MOVE 10000.00 TO YDBT-CONTROL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE AMOUNT 1 AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3350-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YDBM-CYCLE-CD
                   ADD YDBT-AMOUNT-1-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YDBM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE YDBT-AMOUNT-1-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PIVOT CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3360-DESCRIBE-PIVOT.
           EVALUATE YDBM-PIVOT-CD
               WHEN 'KGT'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'NNG'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'PTC'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'WZW'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE AMOUNT 1 AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3370-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN YDBM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YDBM-PREFERRED-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = YDBM-AMOUNT-1-AMT *
                       YDBM-DISCOUNT / 200
               WHEN YDBM-SOURCE-CD = 'BNX'
                   MOVE 20.00 TO WS-WORK-AMT
               WHEN YDBM-SOURCE-CD = 'FTB'
                   MOVE 15.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = YDBM-AMOUNT-1-AMT *
                       YDBM-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 1.00
               MOVE 1.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 50.00
               MOVE 50.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE WINDOW RULE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3380-DESCRIBE-WINDOW-RULE.
           EVALUATE YDBM-WINDOW-RULE-CD
               WHEN 'A'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'D'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'S'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'T'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON AMOUNT 2 AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3390-ACCRUE-AMOUNT-2.
           IF YDBT-AMOUNT-2-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   YDBT-AMOUNT-2-AMT * YDBM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO YDBM-AMOUNT-2-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3400-AGE-LAST-STMT.
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
                   ADD YDBM-CONTROL-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD YDBM-CONTROL-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD YDBM-CONTROL-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD YDBM-CONTROL-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD YDBM-CONTROL-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CONTROL AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3410-ACCRUE-CONTROL.
           IF YDBM-CONTROL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   YDBM-CONTROL-AMT * YDBM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO YDBM-AMOUNT-2-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 60 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3420-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF YDBT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YDBT-POST-DATE TO DTU-DATE-1
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
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3430-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF YDBT-POST-DATE > 0 AND YDBT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YDBT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3440-DESCRIBE-CYCLE.
           EVALUATE YDBM-CYCLE-CD
               WHEN 'FJ'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'LA'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'SP'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'UC'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE AMOUNT 2 YTD FEE ON THE WORK AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3450-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN YDBM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO YDBM-AMOUNT-2-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YDBM-EXEMPT-FLAG = 'Y'
                   COMPUTE YDBM-AMOUNT-2-YTD ROUNDED = WS-WORK-AMT *
                       YDBM-MARGIN / 200
               WHEN YDBM-SOURCE-CD = 'BNX'
                   MOVE 3.00 TO YDBM-AMOUNT-2-YTD
               WHEN YDBM-SOURCE-CD = 'FTB'
                   MOVE 20.00 TO YDBM-AMOUNT-2-YTD
               WHEN OTHER
                   COMPUTE YDBM-AMOUNT-2-YTD ROUNDED = WS-WORK-AMT *
                       YDBM-MARGIN / 100
           END-EVALUATE
           IF YDBM-AMOUNT-2-YTD > 0 AND YDBM-AMOUNT-2-YTD < 1.00
               MOVE 1.00 TO YDBM-AMOUNT-2-YTD
           END-IF
           IF YDBM-AMOUNT-2-YTD > 100.00
               MOVE 100.00 TO YDBM-AMOUNT-2-YTD
           END-IF
           ADD YDBM-AMOUNT-2-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3460-DESCRIBE-TRAN.
           EVALUATE YDBT-TRAN-CD
               WHEN 'CV'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'RV'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'TS'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3470-COUNT-PAYMENT.
           IF YDBM-PAYMENT-CNT < 9999999
               ADD 1 TO YDBM-PAYMENT-CNT
           END-IF
           IF YDBM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO YDBT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE AMOUNT 1 YTD BY PIVOT CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3480-ACCUM-PIVOT.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YDBM-PIVOT-CD
                   ADD YDBM-AMOUNT-1-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YDBM-PIVOT-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE YDBM-AMOUNT-1-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E782 MEMO REQUIRED
      * E797 WORK AMT 2 NEGATIVE
      * E393 WORK AMT 2 OVER 10000
      * E119 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3490-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YDBT-MEMO = SPACES
                   MOVE 'E782' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E797' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 10000.00
                   MOVE 'E393' TO WS-REJECT-CODE
               WHEN YDBM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E119' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR WINDOW RULE CD:
      * A -> D WHEN POST DATE IS PAST
      * D -> S WHEN POST DATE IS PAST
      * S -> T WHEN POST DATE IS PAST
      * T -> A WHEN POST DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3500-SET-WINDOW-RULE.
           EVALUATE YDBM-WINDOW-RULE-CD
               WHEN 'A'
                   IF YDBT-POST-DATE > 0 AND YDBT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO YDBM-WINDOW-RULE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF YDBT-POST-DATE > 0 AND YDBT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO YDBM-WINDOW-RULE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF YDBT-POST-DATE > 0 AND YDBT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO YDBM-WINDOW-RULE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF YDBT-POST-DATE > 0 AND YDBT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'A' TO YDBM-WINDOW-RULE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3510-COUNT-SEQ.
           IF YDBT-SEQ < 9999999
               ADD 1 TO YDBT-SEQ
           END-IF
           IF YDBT-SEQ NOT < 10
               MOVE 'Y' TO YDBM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE WINDOW RULE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3520-DESCRIBE-WINDOW-RULE.
           EVALUATE YDBM-WINDOW-RULE-CD
               WHEN 'A'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'D'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'S'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'T'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E391 DESC REQUIRED
      * E109 WORK AMT 2 NEGATIVE
      * E387 WORK AMT 2 OVER 5000
      * E768 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3530-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YDBM-DESC = SPACES
                   MOVE 'E391' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E109' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 5000.00
                   MOVE 'E387' TO WS-REJECT-CODE
               WHEN YDBM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E768' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CONTROL AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3540-SPLIT-CONTROL.
           COMPUTE WS-WORK-AMT = YDBT-CONTROL-AMT / 2
           COMPUTE WS-WORK-AMT-2 = YDBT-CONTROL-AMT - (WS-WORK-AMT * 2)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE AMOUNT 1 AMT BY WINDOW RULE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3550-ACCUM-WINDOW-RULE.
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
      * ACCUMULATE WORK AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3560-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YDBT-CHANNEL-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YDBT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT 2 AMT IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3570-LIMIT-AMOUNT-2.
           IF YDBM-AMOUNT-2-AMT < 100.00
               MOVE 100.00 TO YDBM-AMOUNT-2-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YDBM-AMOUNT-2-AMT > 2500.00
               MOVE 2500.00 TO YDBM-AMOUNT-2-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E245 CONTACT REQUIRED
      * E773 CONTROL AMT NEGATIVE
      * E068 CONTROL AMT OVER 25000
      * E653 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3580-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YDBM-CONTACT = SPACES
                   MOVE 'E245' TO WS-REJECT-CODE
               WHEN YDBT-CONTROL-AMT < 0
                   MOVE 'E773' TO WS-REJECT-CODE
               WHEN YDBT-CONTROL-AMT > 25000.00
                   MOVE 'E068' TO WS-REJECT-CODE
               WHEN YDBM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E653' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT AMOUNT 2 YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3590-SPLIT-AMOUNT-2.
           COMPUTE WS-WORK-AMT = YDBM-AMOUNT-2-YTD / 3
           COMPUTE WS-WORK-AMT-2 = YDBM-AMOUNT-2-YTD - (WS-WORK-AMT * 3)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3600-COUNT-PAYMENT.
           IF YDBM-PAYMENT-CNT < 9999999
               ADD 1 TO YDBM-PAYMENT-CNT
           END-IF
           IF YDBM-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO YDBM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE WINDOW RULE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3610-DESCRIBE-WINDOW-RULE.
           EVALUATE YDBM-WINDOW-RULE-CD
               WHEN 'A'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'D'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'S'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'T'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CONTROL YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3620-ACCRUE-CONTROL.
           IF YDBM-CONTROL-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   YDBM-CONTROL-YTD * YDBM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO YDBM-AMOUNT-1-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 45 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3630-CHECK-EXPIRY.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO YDBT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3640-DESCRIBE-TRAN.
           EVALUATE YDBT-TRAN-CD
               WHEN 'CV'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'RV'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'TS'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AMOUNT 1 YTD IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3650-LIMIT-AMOUNT-1.
           IF YDBM-AMOUNT-1-YTD < 0.00
               MOVE 0.00 TO YDBM-AMOUNT-1-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YDBM-AMOUNT-1-YTD > 50000.00
               MOVE 50000.00 TO YDBM-AMOUNT-1-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E969 CUST ID REQUIRED
      * E739 WORK AMT NEGATIVE
      * E802 WORK AMT OVER 50000
      * E063 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3660-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YDBM-CUST-ID = SPACES
                   MOVE 'E969' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E739' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 50000.00
                   MOVE 'E802' TO WS-REJECT-CODE
               WHEN YDBT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E063' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PIVOT CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3670-DESCRIBE-PIVOT.
           EVALUATE YDBM-PIVOT-CD
               WHEN 'KGT'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'NNG'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'PTC'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'WZW'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE AMOUNT 2 AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3680-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YDBT-TRAN-CD
                   ADD YDBM-AMOUNT-2-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YDBT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE YDBM-AMOUNT-2-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT AMOUNT 1 AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3690-SPLIT-AMOUNT-1.
           COMPUTE WS-WORK-AMT = YDBM-AMOUNT-1-AMT / 3
           COMPUTE WS-WORK-AMT-2 = YDBM-AMOUNT-1-AMT - (WS-WORK-AMT * 3)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CONTROL YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3700-FLAG-CONTROL.
           IF YDBM-CONTROL-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE YDBM-CONTROL-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE AMOUNT 2 AMT BY PIVOT CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3710-ACCUM-PIVOT.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YDBM-PIVOT-CD
                   ADD YDBT-AMOUNT-2-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YDBM-PIVOT-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE YDBT-AMOUNT-2-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 60 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3720-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF YDBT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YDBT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO YDBM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT 1 YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3730-FLAG-AMOUNT-1.
           IF YDBM-AMOUNT-1-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE YDBM-AMOUNT-1-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CONTROL YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3740-ACCRUE-CONTROL.
           IF YDBM-CONTROL-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   YDBM-CONTROL-YTD * YDBM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO YDBT-CONTROL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3750-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF YDBM-LAST-ACT-DATE > 0 AND YDBM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YDBM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD YDBM-AMOUNT-1-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD YDBM-AMOUNT-1-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD YDBM-AMOUNT-1-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD YDBM-AMOUNT-1-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD YDBM-AMOUNT-1-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO AMOUNT 1 AMT.
      *----------------------------------------------------------------*
       3760-PRORATE-AMOUNT-1.
           IF WS-PERIOD-DAYS > 0
               COMPUTE YDBT-AMOUNT-1-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO YDBT-AMOUNT-1-AMT
           END-IF
           ADD YDBT-AMOUNT-1-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3770-DESCRIBE-REASON.
           EVALUATE YDBM-REASON-CD
               WHEN 'AX'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'LK'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'NT'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'UW'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE CONTROL FEE ON THE CONTROL YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3780-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN YDBT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO YDBM-CONTROL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YDBM-REVIEW-FLAG = 'Y'
                   COMPUTE YDBM-CONTROL-AMT ROUNDED = YDBM-CONTROL-YTD *
                       YDBM-DISCOUNT / 200
               WHEN YDBM-REGION-CD = 'C'
                   MOVE 12.00 TO YDBM-CONTROL-AMT
               WHEN YDBM-REGION-CD = 'H'
                   MOVE 15.00 TO YDBM-CONTROL-AMT
               WHEN OTHER
                   COMPUTE YDBM-CONTROL-AMT ROUNDED = YDBM-CONTROL-YTD *
                       YDBM-DISCOUNT / 100
           END-EVALUATE
           IF YDBM-CONTROL-AMT > 0 AND YDBM-CONTROL-AMT < 5.00
               MOVE 5.00 TO YDBM-CONTROL-AMT
           END-IF
           IF YDBM-CONTROL-AMT > 50.00
               MOVE 50.00 TO YDBM-CONTROL-AMT
           END-IF
           ADD YDBM-CONTROL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE AMOUNT 1 FEE ON THE AMOUNT 2 YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3790-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN YDBM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO YDBM-AMOUNT-1-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YDBT-REVERSAL-FLAG = 'Y'
                   COMPUTE YDBM-AMOUNT-1-AMT ROUNDED = YDBM-AMOUNT-2-YTD
                       * YDBM-MARGIN / 200
               WHEN YDBM-PIVOT-CD = 'KGT'
                   MOVE 20.00 TO YDBM-AMOUNT-1-AMT
               WHEN YDBM-PIVOT-CD = 'NNG'
                   MOVE 7.00 TO YDBM-AMOUNT-1-AMT
               WHEN OTHER
                   COMPUTE YDBM-AMOUNT-1-AMT ROUNDED = YDBM-AMOUNT-2-YTD
                       * YDBM-MARGIN / 100
           END-EVALUATE
           IF YDBM-AMOUNT-1-AMT > 0 AND YDBM-AMOUNT-1-AMT < 5.00
               MOVE 5.00 TO YDBM-AMOUNT-1-AMT
           END-IF
           IF YDBM-AMOUNT-1-AMT > 50.00
               MOVE 50.00 TO YDBM-AMOUNT-1-AMT
           END-IF
           ADD YDBM-AMOUNT-1-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE AMOUNT 1 BAND TIER FOR THE AMOUNT 1 AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3800-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-AMOUNT-1-BAND-RATE(1) TO YDBM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF YDBT-AMOUNT-1-AMT NOT < WS-AMOUNT-1-BAND-LOW(WS-TX)
                   MOVE WS-AMOUNT-1-BAND-RATE(WS-TX) TO YDBM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF YDBM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO YDBM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT 2 AMT IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3810-LIMIT-AMOUNT-2.
           IF YDBM-AMOUNT-2-AMT < 100.00
               MOVE 100.00 TO YDBM-AMOUNT-2-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YDBM-AMOUNT-2-AMT > 5000.00
               MOVE 5000.00 TO YDBM-AMOUNT-2-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT 1 AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3820-FLAG-AMOUNT-1.
           IF YDBT-AMOUNT-1-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE YDBT-AMOUNT-1-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * C -> H WHEN LAST PAY DATE IS PAST
      * H -> K WHEN LAST PAY DATE IS PAST
      * K -> S WHEN LAST PAY DATE IS PAST
      * S -> V WHEN LAST PAY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3830-SET-REGION.
           EVALUATE YDBM-REGION-CD
               WHEN 'C'
                   IF YDBM-LAST-PAY-DATE > 0 AND YDBM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO YDBM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF YDBM-LAST-PAY-DATE > 0 AND YDBM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'K' TO YDBM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF YDBM-LAST-PAY-DATE > 0 AND YDBM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO YDBM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF YDBM-LAST-PAY-DATE > 0 AND YDBM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'V' TO YDBM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE AMOUNT 2 GRID TIER FOR THE AMOUNT 2 AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3840-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-AMOUNT-2-GRID-RATE(1) TO YDBM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF YDBT-AMOUNT-2-AMT NOT < WS-AMOUNT-2-GRID-LOW(WS-TX)
                   MOVE WS-AMOUNT-2-GRID-RATE(WS-TX) TO YDBM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF YDBM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO YDBM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3850-SPLIT-CONTROL.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 3)
           COMPUTE YDBT-CONTROL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT 2 YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3860-FLAG-AMOUNT-2.
           IF YDBM-AMOUNT-2-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE YDBM-AMOUNT-2-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3870-COUNT-CYCLE.
           IF YDBM-CYCLE-CNT < 9999999
               ADD 1 TO YDBM-CYCLE-CNT
           END-IF
           IF YDBM-CYCLE-CNT NOT < 3
               MOVE 'Y' TO YDBM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE AMOUNT 1 AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3880-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YDBM-CYCLE-CD
                   ADD YDBT-AMOUNT-1-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YDBM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE YDBT-AMOUNT-1-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3890-COUNT-SEQ.
           IF YDBT-SEQ < 9999999
               ADD 1 TO YDBT-SEQ
           END-IF
           IF YDBT-SEQ NOT < 6
               MOVE 'Y' TO YDBM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3900-COUNT-ITEM.
           IF YDBM-ITEM-CNT < 9999999
               ADD 1 TO YDBM-ITEM-CNT
           END-IF
           IF YDBM-ITEM-CNT NOT < 6
               MOVE 'Y' TO YDBM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE AMOUNT 2 AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO AMOUNT 1 AMT.
      *----------------------------------------------------------------*
       3910-PRORATE-AMOUNT-1.
           IF WS-PERIOD-DAYS > 0
               COMPUTE YDBT-AMOUNT-1-AMT ROUNDED =
                   YDBT-AMOUNT-2-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO YDBT-AMOUNT-1-AMT
           END-IF
           ADD YDBT-AMOUNT-1-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE PIVOT CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3920-DESCRIBE-PIVOT.
           EVALUATE YDBM-PIVOT-CD
               WHEN 'KGT'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'NNG'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'PTC'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'WZW'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CONTROL AMT IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3930-LIMIT-CONTROL.
           IF YDBT-CONTROL-AMT < 25.00
               MOVE 25.00 TO YDBT-CONTROL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YDBT-CONTROL-AMT > 2500.00
               MOVE 2500.00 TO YDBT-CONTROL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3940-SPLIT-CONTROL.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 3)
           COMPUTE YDBT-CONTROL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE AMOUNT 1 BAND TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3950-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-AMOUNT-1-BAND-RATE(1) TO YDBM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF WS-WORK-AMT-2 NOT < WS-AMOUNT-1-BAND-LOW(WS-TX)
                   MOVE WS-AMOUNT-1-BAND-RATE(WS-TX) TO YDBM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF YDBM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO YDBM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3960-COUNT-RETRY.
           IF YDBM-RETRY-CNT < 9999999
               ADD 1 TO YDBM-RETRY-CNT
           END-IF
           IF YDBM-RETRY-CNT NOT < 3
               MOVE 'Y' TO YDBM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CONTROL YTD IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3970-LIMIT-CONTROL.
           IF YDBM-CONTROL-YTD < 10.00
               MOVE 10.00 TO YDBM-CONTROL-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YDBM-CONTROL-YTD > 5000.00
               MOVE 5000.00 TO YDBM-CONTROL-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT 2 YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3980-FLAG-AMOUNT-2.
           IF YDBM-AMOUNT-2-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE YDBM-AMOUNT-2-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3990-COUNT-ITEM.
           IF YDBM-ITEM-CNT < 9999999
               ADD 1 TO YDBM-ITEM-CNT
           END-IF
           IF YDBM-ITEM-CNT NOT < 5
               MOVE 'Y' TO YDBM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CONTROL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO AMOUNT 1 AMT.
      *----------------------------------------------------------------*
       4000-PRORATE-AMOUNT-1.
           IF WS-PERIOD-DAYS > 0
               COMPUTE YDBM-AMOUNT-1-AMT ROUNDED =
                   YDBT-CONTROL-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO YDBM-AMOUNT-1-AMT
           END-IF
           ADD YDBM-AMOUNT-1-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4010-AGE-OPEN.
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
                   ADD YDBT-CONTROL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD YDBT-CONTROL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD YDBT-CONTROL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD YDBT-CONTROL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD YDBT-CONTROL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AMOUNT 2 AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4020-FLAG-AMOUNT-2.
           IF YDBM-AMOUNT-2-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE YDBM-AMOUNT-2-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT AMOUNT 1 AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4030-SPLIT-AMOUNT-1.
           COMPUTE WS-WORK-AMT = YDBM-AMOUNT-1-AMT / 12
           COMPUTE WS-WORK-AMT-2 = YDBM-AMOUNT-1-AMT - (WS-WORK-AMT *
               12)
           COMPUTE YDBM-AMOUNT-2-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CONTROL SCHED TIER FOR THE AMOUNT 2 AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4040-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-CONTROL-SCHED-RATE(1) TO YDBM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF YDBT-AMOUNT-2-AMT NOT < WS-CONTROL-SCHED-LOW(WS-TX)
                   MOVE WS-CONTROL-SCHED-RATE(WS-TX) TO YDBM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF YDBM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO YDBM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE AMOUNT 1 AMT BY WINDOW RULE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       4050-ACCUM-WINDOW-RULE.
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
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4060-COUNT-PAYMENT.
           IF YDBM-PAYMENT-CNT < 9999999
               ADD 1 TO YDBM-PAYMENT-CNT
           END-IF
           IF YDBM-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO YDBM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 180 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4070-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF YDBM-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YDBM-LAST-MAINT-DATE TO DTU-DATE-1
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
      * ASSESS THE WORK 2 FEE ON THE CONTROL AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       4080-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN YDBM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YDBM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = YDBM-CONTROL-AMT *
                       YDBM-PENALTY-PCT / 200
               WHEN YDBM-WINDOW-RULE-CD = 'A'
                   MOVE 15.00 TO WS-WORK-AMT-2
               WHEN YDBM-WINDOW-RULE-CD = 'D'
                   MOVE 15.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = YDBM-CONTROL-AMT *
                       YDBM-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 5.00
               MOVE 5.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 250.00
               MOVE 250.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 90 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4090-CHECK-EXPIRY.
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
           IF WS-DAYS > 90
               MOVE 'Y' TO YDBM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT 1 AMT IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4100-LIMIT-AMOUNT-1.
           IF YDBT-AMOUNT-1-AMT < 0.00
               MOVE 0.00 TO YDBT-AMOUNT-1-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YDBT-AMOUNT-1-AMT > 1000.00
               MOVE 1000.00 TO YDBT-AMOUNT-1-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * FJ -> LA WHEN LAST MAINT DATE IS PAST
      * LA -> SP WHEN LAST MAINT DATE IS PAST
      * SP -> UC WHEN LAST MAINT DATE IS PAST
      * UC -> FJ WHEN LAST MAINT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4110-SET-CYCLE.
           EVALUATE YDBM-CYCLE-CD
               WHEN 'FJ'
                   IF YDBM-LAST-MAINT-DATE > 0 AND YDBM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'LA' TO YDBM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LA'
                   IF YDBM-LAST-MAINT-DATE > 0 AND YDBM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'SP' TO YDBM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SP'
                   IF YDBM-LAST-MAINT-DATE > 0 AND YDBM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'UC' TO YDBM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UC'
                   IF YDBM-LAST-MAINT-DATE > 0 AND YDBM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'FJ' TO YDBM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE AMOUNT 2 FEE ON THE WORK AMT 2.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4120-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN YDBM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO YDBT-AMOUNT-2-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YDBM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE YDBT-AMOUNT-2-AMT ROUNDED = WS-WORK-AMT-2 *
                       YDBM-MARGIN / 200
               WHEN YDBT-TRAN-CD = 'CV'
                   MOVE 20.00 TO YDBT-AMOUNT-2-AMT
               WHEN YDBT-TRAN-CD = 'RV'
                   MOVE 5.00 TO YDBT-AMOUNT-2-AMT
               WHEN OTHER
                   COMPUTE YDBT-AMOUNT-2-AMT ROUNDED = WS-WORK-AMT-2 *
                       YDBM-MARGIN / 100
           END-EVALUATE
           IF YDBT-AMOUNT-2-AMT > 0 AND YDBT-AMOUNT-2-AMT < 2.00
               MOVE 2.00 TO YDBT-AMOUNT-2-AMT
           END-IF
           IF YDBT-AMOUNT-2-AMT > 25.00
               MOVE 25.00 TO YDBT-AMOUNT-2-AMT
           END-IF
           ADD YDBT-AMOUNT-2-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE AMOUNT 2 FEE ON THE AMOUNT 1 AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4130-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN YDBT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO YDBT-AMOUNT-2-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YDBM-PREFERRED-FLAG = 'Y'
                   COMPUTE YDBT-AMOUNT-2-AMT ROUNDED = YDBM-AMOUNT-1-AMT
                       * YDBM-PENALTY-PCT / 200
               WHEN YDBM-CYCLE-CD = 'FJ'
                   MOVE 15.00 TO YDBT-AMOUNT-2-AMT
               WHEN YDBM-CYCLE-CD = 'LA'
                   MOVE 20.00 TO YDBT-AMOUNT-2-AMT
               WHEN OTHER
                   COMPUTE YDBT-AMOUNT-2-AMT ROUNDED = YDBM-AMOUNT-1-AMT
                       * YDBM-PENALTY-PCT / 100
           END-EVALUATE
           IF YDBT-AMOUNT-2-AMT > 0 AND YDBT-AMOUNT-2-AMT < 1.00
               MOVE 1.00 TO YDBT-AMOUNT-2-AMT
           END-IF
           IF YDBT-AMOUNT-2-AMT > 75.00
               MOVE 75.00 TO YDBT-AMOUNT-2-AMT
           END-IF
           ADD YDBT-AMOUNT-2-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * FJ -> LA WHEN NOTICE DATE IS PAST
      * LA -> SP WHEN NOTICE DATE IS PAST
      * SP -> UC WHEN NOTICE DATE IS PAST
      * UC -> FJ WHEN NOTICE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4140-SET-CYCLE.
           EVALUATE YDBM-CYCLE-CD
               WHEN 'FJ'
                   IF YDBM-NOTICE-DATE > 0 AND YDBM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LA' TO YDBM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LA'
                   IF YDBM-NOTICE-DATE > 0 AND YDBM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SP' TO YDBM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SP'
                   IF YDBM-NOTICE-DATE > 0 AND YDBM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UC' TO YDBM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UC'
                   IF YDBM-NOTICE-DATE > 0 AND YDBM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FJ' TO YDBM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E872 MEMO REQUIRED
      * E670 CONTROL AMT NEGATIVE
      * E924 CONTROL AMT OVER 5000
      * E929 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       4150-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YDBT-MEMO = SPACES
                   MOVE 'E872' TO WS-REJECT-CODE
               WHEN YDBT-CONTROL-AMT < 0
                   MOVE 'E670' TO WS-REJECT-CODE
               WHEN YDBT-CONTROL-AMT > 5000.00
                   MOVE 'E924' TO WS-REJECT-CODE
               WHEN YDBT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E929' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CONTROL YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4160-SPLIT-CONTROL.
           COMPUTE WS-WORK-AMT = YDBM-CONTROL-YTD / 12
           COMPUTE WS-WORK-AMT-2 = YDBM-CONTROL-YTD - (WS-WORK-AMT * 12)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E026 REJECT CD REQUIRED
      * E662 WORK AMT NEGATIVE
      * E103 WORK AMT OVER 25000
      * E758 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       4170-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YDBT-REJECT-CD = SPACES
                   MOVE 'E026' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E662' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 25000.00
                   MOVE 'E103' TO WS-REJECT-CODE
               WHEN YDBM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E758' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CONTROL SCHED TIER FOR THE AMOUNT 1 YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4180-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-CONTROL-SCHED-RATE(1) TO YDBM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF YDBM-AMOUNT-1-YTD NOT < WS-CONTROL-SCHED-LOW(WS-TX)
                   MOVE WS-CONTROL-SCHED-RATE(WS-TX) TO YDBM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF YDBM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO YDBM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT 2 AMT IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4190-LIMIT-AMOUNT-2.
           IF YDBM-AMOUNT-2-AMT < 0.00
               MOVE 0.00 TO YDBM-AMOUNT-2-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YDBM-AMOUNT-2-AMT > 5000.00
               MOVE 5000.00 TO YDBM-AMOUNT-2-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E986 CUST ID REQUIRED
      * E086 AMOUNT 2 YTD NEGATIVE
      * E875 AMOUNT 2 YTD OVER 250000
      * E983 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       4200-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YDBM-CUST-ID = SPACES
                   MOVE 'E986' TO WS-REJECT-CODE
               WHEN YDBM-AMOUNT-2-YTD < 0
                   MOVE 'E086' TO WS-REJECT-CODE
               WHEN YDBM-AMOUNT-2-YTD > 250000.00
                   MOVE 'E875' TO WS-REJECT-CODE
               WHEN YDBM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E983' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT AMOUNT 2 AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4210-SPLIT-AMOUNT-2.
           COMPUTE WS-WORK-AMT = YDBT-AMOUNT-2-AMT / 4
           COMPUTE WS-WORK-AMT-2 = YDBT-AMOUNT-2-AMT - (WS-WORK-AMT * 4)
           COMPUTE YDBM-AMOUNT-2-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PIVOT CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4220-DESCRIBE-PIVOT.
           EVALUATE YDBM-PIVOT-CD
               WHEN 'KGT'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'NNG'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'PTC'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'WZW'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4230-DESCRIBE-TRAN.
           EVALUATE YDBT-TRAN-CD
               WHEN 'CV'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'RV'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'TS'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E933 NAME REQUIRED
      * E643 AMOUNT 2 AMT NEGATIVE
      * E233 AMOUNT 2 AMT OVER 10000
      * E299 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       4240-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YDBM-NAME = SPACES
                   MOVE 'E933' TO WS-REJECT-CODE
               WHEN YDBT-AMOUNT-2-AMT < 0
                   MOVE 'E643' TO WS-REJECT-CODE
               WHEN YDBT-AMOUNT-2-AMT > 10000.00
                   MOVE 'E233' TO WS-REJECT-CODE
               WHEN YDBM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E299' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

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
                   MOVE YKPRC-AMOUNT-1-AMT
                     TO YDBM-AMOUNT-1-AMT
                   MOVE YKPRC-AMOUNT-1-YTD
                     TO YDBM-AMOUNT-1-YTD
                   MOVE YKPRC-AMOUNT-2-AMT
                     TO YDBM-AMOUNT-2-AMT
                   MOVE YKPRC-CONTROL-AMT
                     TO YDBM-CONTROL-AMT
                   MOVE YKPRC-AUTO-RENEW-FLAG
                     TO YDBM-AUTO-RENEW-FLAG
                   MOVE YKPRC-PREFERRED-FLAG
                     TO YDBM-PREFERRED-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RPRC' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-YKPRC' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
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
                   MOVE YKLIM-CONTROL-AMT
                     TO YDBM-CONTROL-AMT
                   MOVE YKLIM-AMOUNT-2-YTD
                     TO YDBM-AMOUNT-2-YTD
                   MOVE YKLIM-CONTROL-YTD
                     TO YDBM-CONTROL-YTD
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
           DISPLAY 'YKBBILL READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'YKBBILL' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
