      *================================================================*
      * PROGRAM  : YKBCLSE                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : DATE WINDOW BRIDGE - CLOSURE PROCESSING. READS THE  *
      *            YDB TRANSACTIONS, UPDATES THE YDB MASTER AND        *
      *            PRODUCES THE CONTROL REPORT. TWO-DIGIT YEAR         *
      *            WINDOWING BRIDGE FOR CENTURY DATE CONVERSION        *
      * INPUT    : YDBMST (KSDS), YDBTRN, BANKCTL                      *
      * OUTPUT   : YDBOUT, YKBCLRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, YKTIER                  *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-04-18 TWH  ORIGINAL                                       *
      * 1992-07-13 GAF  CONTROL CUTOFF MOVED REQ3113                   *
      * 1995-07-27 GAF  CONTROL EXCEPTIONS TO OPS QUEUE CHG5811        *
      * 1997-08-05 CRP  PERFORMANCE - AMOUNT-1 READ AHEAD INC2963      *
      * 1998-01-01 GAF  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1998-08-11 EWS  AMOUNT-1 EXCEPTIONS TO OPS QUEUE INC0616       *
      * 2003-08-20 JLM  CONTROL RERUN PROTECTION INC7559               *
      * 2006-03-09 TWH  AMOUNT-1 LIMIT REVISED REQ2286                 *
      * 2007-10-01 PQR  AMOUNT-1 STATUS CODES ADDED INC7865            *
      * 2007-02-28 MKL  CONTROL LIMIT REVISED CHG3047                  *
      * 2008-07-07 CRP  PERFORMANCE - AMOUNT-2 READ AHEAD REQ1035      *
      * 2008-02-24 DPS  CONTROL CUTOFF MOVED CHG1054                   *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YKBCLSE.
       AUTHOR. R J TRAVERS.

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
           SELECT REPORT-FILE ASSIGN TO "YKBCLRPT"
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
      * WS-CONTROL-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CONTROL-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000150000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000201000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012500000000182000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017500000000218000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020000000000285500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022500000000897000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023000000000700000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00024000000000154000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00029000000000630000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00029100000000667000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00031600000000739000'.
       01  WS-CONTROL-SCHED-TBL REDEFINES WS-CONTROL-SCHED-VALUES.
           05  WS-CONTROL-SCHED-ROW OCCURS 11 TIMES.
               10  WS-CONTROL-SCHED-LOW    PIC 9(09)V99.
               10  WS-CONTROL-SCHED-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-AMOUNT-1-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-AMOUNT-1-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000623000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000694500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010250000000659500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020250000000209500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020350000000053500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022850000000356000'.
       01  WS-AMOUNT-1-BAND-TBL REDEFINES WS-AMOUNT-1-BAND-VALUES.
           05  WS-AMOUNT-1-BAND-ROW OCCURS 6 TIMES.
               10  WS-AMOUNT-1-BAND-LOW    PIC 9(09)V99.
               10  WS-AMOUNT-1-BAND-RATE   PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  YKBCLSE  DATE WINDOW BRIDGE'.
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
       COPY YKTIERP.

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
               PERFORM 3000-PRORATE-AMOUNT-2
               PERFORM 3010-AGE-POST
               PERFORM 3020-PRORATE-AMOUNT-1
               PERFORM 3030-EDIT-CONTACT
               PERFORM 3040-AGE-LAST-PAY
               PERFORM 3050-ACCRUE-AMOUNT-1
               PERFORM 3060-CHECK-NEXT-DUE
               PERFORM 3070-PRORATE-AMOUNT-1
               PERFORM 3080-FLAG-AMOUNT-2
               PERFORM 3090-COUNT-ITEM
               PERFORM 3100-COUNT-SEQ
               PERFORM 3110-DESCRIBE-CYCLE
               PERFORM 3120-ACCRUE-CONTROL
               PERFORM 3130-ACCRUE-AMOUNT-2
               PERFORM 3140-SELECT-DISCOUNT
               PERFORM 3150-ACCUM-REGION
               PERFORM 3160-ACCUM-REASON
               PERFORM 3170-SELECT-MARGIN
               PERFORM 3180-SET-PIVOT
               PERFORM 3190-COUNT-PAYMENT
               PERFORM 3200-DESCRIBE-WINDOW-RULE
               PERFORM 3210-LIMIT-CONTROL
               PERFORM 3220-SPLIT-AMOUNT-2
               PERFORM 3230-AGE-OPEN
               PERFORM 3240-SET-SOURCE
               PERFORM 3250-FLAG-AMOUNT-2
               PERFORM 3260-ACCUM-TRAN
               PERFORM 3270-ASSESS-REVERSAL
               PERFORM 3280-SET-CHANNEL
               PERFORM 3290-AGE-EFF
               PERFORM 3300-COUNT-RETRY
               PERFORM 3310-DESCRIBE-SOURCE
               PERFORM 3320-SET-SOURCE
               PERFORM 3330-FLAG-AMOUNT-2
               PERFORM 3340-FLAG-CONTROL
               PERFORM 3350-CHECK-MATURITY
               PERFORM 3360-COUNT-RETRY
               PERFORM 3370-FLAG-AMOUNT-2
               PERFORM 3380-ASSESS-REVIEW
               PERFORM 3390-AGE-NEXT-DUE
               PERFORM 3400-ASSESS-PREFERRED
               PERFORM 3410-FLAG-AMOUNT-2
               PERFORM 3420-LIMIT-AMOUNT-2
               PERFORM 3430-ACCRUE-AMOUNT-2
               PERFORM 3440-SPLIT-CONTROL
               PERFORM 3450-SELECT-DISCOUNT
               PERFORM 3460-DESCRIBE-TRAN
               PERFORM 3470-AGE-LAST-ACT
               PERFORM 3480-EDIT-OPERATOR
               PERFORM 3490-ACCRUE-MARGIN
               PERFORM 3500-SET-CYCLE
               PERFORM 3510-SELECT-DISCOUNT
               PERFORM 3520-PRORATE-AMOUNT-1
               PERFORM 3530-EDIT-NAME
               PERFORM 3540-SPLIT-AMOUNT-2
               PERFORM 3550-LIMIT-CONTROL
               PERFORM 3560-ACCUM-WINDOW-RULE
               PERFORM 3570-COUNT-CYCLE
               PERFORM 3580-COUNT-CYCLE
               PERFORM 3590-AGE-NOTICE
               PERFORM 3600-ACCUM-CYCLE
               PERFORM 3610-ACCUM-REGION
               PERFORM 3620-EDIT-ADDR
               PERFORM 3630-EDIT-CUST-ID
               PERFORM 3640-PRORATE-AMOUNT-1
               PERFORM 3650-EDIT-REJECT
               PERFORM 3660-SPLIT-AMOUNT-1
               PERFORM 3670-CHECK-NOTICE
               PERFORM 3680-PRORATE-AMOUNT-2
               PERFORM 3690-EDIT-ADDR
               PERFORM 3700-PRORATE-AMOUNT-1
               PERFORM 3710-CHECK-LAST-PAY
               PERFORM 3720-FLAG-CONTROL
               PERFORM 3730-SPLIT-AMOUNT-1
               PERFORM 3740-SELECT-MARGIN
               PERFORM 3750-DESCRIBE-REGION
               PERFORM 3760-LIMIT-AMOUNT-1
               PERFORM 3770-AGE-EFF
               PERFORM 3780-CHECK-LAST-PAY
               PERFORM 3790-ASSESS-AUTO-RENEW
               PERFORM 3800-EDIT-CONTACT
               PERFORM 3810-SELECT-DISCOUNT
               PERFORM 3820-AGE-POST
               PERFORM 3830-CHECK-OPEN
               PERFORM 3840-LIMIT-CONTROL
               PERFORM 3850-ACCUM-CHANNEL
               PERFORM 3860-PRORATE-CONTROL
               PERFORM 3870-DESCRIBE-CHANNEL
               PERFORM 3880-COUNT-RETRY
               PERFORM 3890-LIMIT-AMOUNT-2
               PERFORM 3900-LIMIT-ITEM
               PERFORM 3910-LIMIT-AMOUNT-2
               PERFORM 3920-SELECT-PENALTY-PCT
               PERFORM 3930-ACCUM-REGION
               PERFORM 3940-SPLIT-AMOUNT-2
               PERFORM 3950-SET-CYCLE
               PERFORM 3960-EDIT-ADDR
               PERFORM 3970-ASSESS-REVIEW
               PERFORM 3980-CHECK-EXPIRY
               PERFORM 3990-EDIT-MEMO
               PERFORM 4000-SPLIT-AMOUNT-1
               PERFORM 4010-SET-REASON
               PERFORM 4020-ACCRUE-AMOUNT-2
               PERFORM 4030-LIMIT-AMOUNT-2
               PERFORM 4040-SPLIT-CONTROL
               PERFORM 4050-EDIT-CONTACT
               PERFORM 4060-ACCRUE-AMOUNT-2
               PERFORM 4070-SELECT-PENALTY-PCT
               PERFORM 4080-ACCRUE-AMOUNT-1
               PERFORM 4090-CHECK-NOTICE
               PERFORM 4100-CHECK-EFF
               PERFORM 7010-CALL-YKTIER
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
      * PRORATE AMOUNT 1 AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO AMOUNT 2 AMT.
      *----------------------------------------------------------------*
       3000-PRORATE-AMOUNT-2.
           IF WS-PERIOD-DAYS > 0
               COMPUTE YDBT-AMOUNT-2-AMT ROUNDED =
                   YDBM-AMOUNT-1-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO YDBT-AMOUNT-2-AMT
           END-IF
           ADD YDBT-AMOUNT-2-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3010-AGE-POST.
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
                   ADD YDBM-CONTROL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD YDBM-CONTROL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD YDBM-CONTROL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD YDBM-CONTROL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD YDBM-CONTROL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE AMOUNT 1 YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3020-PRORATE-AMOUNT-1.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   YDBM-AMOUNT-1-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E602 CONTACT REQUIRED
      * E223 CONTROL AMT NEGATIVE
      * E954 CONTROL AMT OVER 99999
      * E813 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3030-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YDBM-CONTACT = SPACES
                   MOVE 'E602' TO WS-REJECT-CODE
               WHEN YDBM-CONTROL-AMT < 0
                   MOVE 'E223' TO WS-REJECT-CODE
               WHEN YDBM-CONTROL-AMT > 99999.00
                   MOVE 'E954' TO WS-REJECT-CODE
               WHEN YDBM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E813' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3040-AGE-LAST-PAY.
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
                   ADD YDBT-AMOUNT-2-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD YDBT-AMOUNT-2-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD YDBT-AMOUNT-2-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD YDBT-AMOUNT-2-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD YDBT-AMOUNT-2-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON AMOUNT 1 YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3050-ACCRUE-AMOUNT-1.
           IF YDBM-AMOUNT-1-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   YDBM-AMOUNT-1-YTD * YDBM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO YDBM-AMOUNT-2-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3060-CHECK-NEXT-DUE.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO YDBM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CONTROL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO AMOUNT 1 YTD.
      *----------------------------------------------------------------*
       3070-PRORATE-AMOUNT-1.
           IF WS-PERIOD-DAYS > 0
               COMPUTE YDBM-AMOUNT-1-YTD ROUNDED =
                   YDBT-CONTROL-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO YDBM-AMOUNT-1-YTD
           END-IF
           ADD YDBM-AMOUNT-1-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AMOUNT 2 AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3080-FLAG-AMOUNT-2.
           IF YDBT-AMOUNT-2-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE YDBT-AMOUNT-2-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3090-COUNT-ITEM.
           IF YDBM-ITEM-CNT < 9999999
               ADD 1 TO YDBM-ITEM-CNT
           END-IF
           IF YDBM-ITEM-CNT NOT < 5
               MOVE 'Y' TO YDBM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3100-COUNT-SEQ.
           IF YDBT-SEQ < 9999999
               ADD 1 TO YDBT-SEQ
           END-IF
           IF YDBT-SEQ NOT < 10
               MOVE 'Y' TO YDBT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3110-DESCRIBE-CYCLE.
           EVALUATE YDBM-CYCLE-CD
               WHEN 'FJ'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'LA'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'SP'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'UC'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CONTROL AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3120-ACCRUE-CONTROL.
           IF YDBT-CONTROL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   YDBT-CONTROL-AMT * YDBM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO YDBM-AMOUNT-2-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON AMOUNT 2 AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3130-ACCRUE-AMOUNT-2.
           IF YDBT-AMOUNT-2-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   YDBT-AMOUNT-2-AMT * YDBM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO YDBM-AMOUNT-1-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CONTROL SCHED TIER FOR THE AMOUNT 1 YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3140-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CONTROL-SCHED-RATE(1) TO YDBM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF YDBM-AMOUNT-1-YTD NOT < WS-CONTROL-SCHED-LOW(WS-TX)
                   MOVE WS-CONTROL-SCHED-RATE(WS-TX) TO YDBM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF YDBM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO YDBM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3150-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YDBM-REGION-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YDBM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3160-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YDBM-REASON-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YDBM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE AMOUNT 1 BAND TIER FOR THE AMOUNT 2 AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3170-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-AMOUNT-1-BAND-RATE(1) TO YDBM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF YDBM-AMOUNT-2-AMT NOT < WS-AMOUNT-1-BAND-LOW(WS-TX)
                   MOVE WS-AMOUNT-1-BAND-RATE(WS-TX) TO YDBM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF YDBM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO YDBM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PIVOT CD:
      * KGT -> NNG WHEN EFF DATE IS PAST
      * NNG -> PTC WHEN EFF DATE IS PAST
      * PTC -> WZW WHEN EFF DATE IS PAST
      * WZW -> KGT WHEN EFF DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3180-SET-PIVOT.
           EVALUATE YDBM-PIVOT-CD
               WHEN 'KGT'
                   IF YDBT-EFF-DATE > 0 AND YDBT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NNG' TO YDBM-PIVOT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NNG'
                   IF YDBT-EFF-DATE > 0 AND YDBT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PTC' TO YDBM-PIVOT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PTC'
                   IF YDBT-EFF-DATE > 0 AND YDBT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WZW' TO YDBM-PIVOT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WZW'
                   IF YDBT-EFF-DATE > 0 AND YDBT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KGT' TO YDBM-PIVOT-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3190-COUNT-PAYMENT.
           IF YDBM-PAYMENT-CNT < 9999999
               ADD 1 TO YDBM-PAYMENT-CNT
           END-IF
           IF YDBM-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO YDBM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE WINDOW RULE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3200-DESCRIBE-WINDOW-RULE.
           EVALUATE YDBM-WINDOW-RULE-CD
               WHEN 'A'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'D'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'S'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'T'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CONTROL AMT IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3210-LIMIT-CONTROL.
           IF YDBT-CONTROL-AMT < 0.00
               MOVE 0.00 TO YDBT-CONTROL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YDBT-CONTROL-AMT > 500.00
               MOVE 500.00 TO YDBT-CONTROL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT AMOUNT 2 YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3220-SPLIT-AMOUNT-2.
           COMPUTE WS-WORK-AMT = YDBM-AMOUNT-2-YTD / 12
           COMPUTE WS-WORK-AMT-2 = YDBM-AMOUNT-2-YTD - (WS-WORK-AMT *
               12)
           COMPUTE YDBM-AMOUNT-1-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-45 AND OVER 45 DAYS.
      *----------------------------------------------------------------*
       3230-AGE-OPEN.
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
                   ADD YDBM-AMOUNT-2-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD YDBM-AMOUNT-2-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD YDBM-AMOUNT-2-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 45
                   ADD YDBM-AMOUNT-2-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD YDBM-AMOUNT-2-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * BNX -> FTB WHEN LAST MAINT DATE IS PAST
      * FTB -> GAP WHEN LAST MAINT DATE IS PAST
      * GAP -> PAP WHEN LAST MAINT DATE IS PAST
      * PAP -> VGC WHEN LAST MAINT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3240-SET-SOURCE.
           EVALUATE YDBM-SOURCE-CD
               WHEN 'BNX'
                   IF YDBM-LAST-MAINT-DATE > 0 AND YDBM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'FTB' TO YDBM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FTB'
                   IF YDBM-LAST-MAINT-DATE > 0 AND YDBM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'GAP' TO YDBM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GAP'
                   IF YDBM-LAST-MAINT-DATE > 0 AND YDBM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'PAP' TO YDBM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PAP'
                   IF YDBM-LAST-MAINT-DATE > 0 AND YDBM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'VGC' TO YDBM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AMOUNT 2 AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3250-FLAG-AMOUNT-2.
           IF YDBM-AMOUNT-2-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE YDBM-AMOUNT-2-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE AMOUNT 2 YTD BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3260-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YDBT-TRAN-CD
                   ADD YDBM-AMOUNT-2-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YDBT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE YDBM-AMOUNT-2-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CONTROL FEE ON THE WORK AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3270-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN YDBT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO YDBM-CONTROL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YDBM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE YDBM-CONTROL-AMT ROUNDED = WS-WORK-AMT *
                       YDBM-MARGIN / 200
               WHEN YDBM-SOURCE-CD = 'BNX'
                   MOVE 3.00 TO YDBM-CONTROL-AMT
               WHEN YDBM-SOURCE-CD = 'FTB'
                   MOVE 15.00 TO YDBM-CONTROL-AMT
               WHEN OTHER
                   COMPUTE YDBM-CONTROL-AMT ROUNDED = WS-WORK-AMT *
                       YDBM-MARGIN / 100
           END-EVALUATE
           IF YDBM-CONTROL-AMT > 0 AND YDBM-CONTROL-AMT < 5.00
               MOVE 5.00 TO YDBM-CONTROL-AMT
           END-IF
           IF YDBM-CONTROL-AMT > 250.00
               MOVE 250.00 TO YDBM-CONTROL-AMT
           END-IF
           ADD YDBM-CONTROL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN NOTICE DATE IS PAST
      * BAT -> MAL WHEN NOTICE DATE IS PAST
      * MAL -> TEL WHEN NOTICE DATE IS PAST
      * TEL -> ONL WHEN NOTICE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3280-SET-CHANNEL.
           EVALUATE YDBT-CHANNEL-CD
               WHEN 'BRN'
                   IF YDBM-NOTICE-DATE > 0 AND YDBM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO YDBT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF YDBM-NOTICE-DATE > 0 AND YDBM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO YDBT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF YDBM-NOTICE-DATE > 0 AND YDBM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO YDBT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF YDBM-NOTICE-DATE > 0 AND YDBM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO YDBT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3290-AGE-EFF.
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
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3300-COUNT-RETRY.
           IF YDBM-RETRY-CNT < 9999999
               ADD 1 TO YDBM-RETRY-CNT
           END-IF
           IF YDBM-RETRY-CNT NOT < 10
               MOVE 'Y' TO YDBM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3310-DESCRIBE-SOURCE.
           EVALUATE YDBM-SOURCE-CD
               WHEN 'BNX'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'FTB'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'GAP'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'PAP'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'VGC'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'VJV'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * BNX -> FTB WHEN LAST PAY DATE IS PAST
      * FTB -> GAP WHEN LAST PAY DATE IS PAST
      * GAP -> PAP WHEN LAST PAY DATE IS PAST
      * PAP -> VGC WHEN LAST PAY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3320-SET-SOURCE.
           EVALUATE YDBM-SOURCE-CD
               WHEN 'BNX'
                   IF YDBM-LAST-PAY-DATE > 0 AND YDBM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FTB' TO YDBM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FTB'
                   IF YDBM-LAST-PAY-DATE > 0 AND YDBM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GAP' TO YDBM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GAP'
                   IF YDBM-LAST-PAY-DATE > 0 AND YDBM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PAP' TO YDBM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PAP'
                   IF YDBM-LAST-PAY-DATE > 0 AND YDBM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VGC' TO YDBM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AMOUNT 2 AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3330-FLAG-AMOUNT-2.
           IF YDBM-AMOUNT-2-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE YDBM-AMOUNT-2-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * CONTROL AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3340-FLAG-CONTROL.
           IF YDBM-CONTROL-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE YDBM-CONTROL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 15 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3350-CHECK-MATURITY.
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
           IF WS-DAYS > 15
               MOVE 'Y' TO YDBM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3360-COUNT-RETRY.
           IF YDBM-RETRY-CNT < 9999999
               ADD 1 TO YDBM-RETRY-CNT
           END-IF
           IF YDBM-RETRY-CNT NOT < 12
               MOVE 'Y' TO YDBT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT 2 YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3370-FLAG-AMOUNT-2.
           IF YDBM-AMOUNT-2-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE YDBM-AMOUNT-2-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CONTROL FEE ON THE AMOUNT 2 YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3380-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN YDBM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO YDBT-CONTROL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YDBM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE YDBT-CONTROL-AMT ROUNDED = YDBM-AMOUNT-2-YTD
                       * YDBM-MARGIN / 200
               WHEN YDBM-WINDOW-RULE-CD = 'A'
                   MOVE 3.00 TO YDBT-CONTROL-AMT
               WHEN YDBM-WINDOW-RULE-CD = 'D'
                   MOVE 20.00 TO YDBT-CONTROL-AMT
               WHEN OTHER
                   COMPUTE YDBT-CONTROL-AMT ROUNDED = YDBM-AMOUNT-2-YTD
                       * YDBM-MARGIN / 100
           END-EVALUATE
           IF YDBT-CONTROL-AMT > 0 AND YDBT-CONTROL-AMT < 5.00
               MOVE 5.00 TO YDBT-CONTROL-AMT
           END-IF
           IF YDBT-CONTROL-AMT > 75.00
               MOVE 75.00 TO YDBT-CONTROL-AMT
           END-IF
           ADD YDBT-CONTROL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3390-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF YDBM-NEXT-DUE-DATE > 0 AND YDBM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YDBM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD YDBM-AMOUNT-1-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD YDBM-AMOUNT-1-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD YDBM-AMOUNT-1-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD YDBM-AMOUNT-1-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD YDBM-AMOUNT-1-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE CONTROL FEE ON THE AMOUNT 1 AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3400-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN YDBM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO YDBT-CONTROL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YDBM-REVIEW-FLAG = 'Y'
                   COMPUTE YDBT-CONTROL-AMT ROUNDED = YDBM-AMOUNT-1-AMT
                       * YDBM-DISCOUNT / 200
               WHEN YDBT-CHANNEL-CD = 'BRN'
                   MOVE 7.00 TO YDBT-CONTROL-AMT
               WHEN YDBT-CHANNEL-CD = 'BAT'
                   MOVE 12.00 TO YDBT-CONTROL-AMT
               WHEN OTHER
                   COMPUTE YDBT-CONTROL-AMT ROUNDED = YDBM-AMOUNT-1-AMT
                       * YDBM-DISCOUNT / 100
           END-EVALUATE
           IF YDBT-CONTROL-AMT > 0 AND YDBT-CONTROL-AMT < 1.00
               MOVE 1.00 TO YDBT-CONTROL-AMT
           END-IF
           IF YDBT-CONTROL-AMT > 25.00
               MOVE 25.00 TO YDBT-CONTROL-AMT
           END-IF
           ADD YDBT-CONTROL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AMOUNT 2 YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3410-FLAG-AMOUNT-2.
           IF YDBM-AMOUNT-2-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE YDBM-AMOUNT-2-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT 2 AMT IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3420-LIMIT-AMOUNT-2.
           IF YDBT-AMOUNT-2-AMT < 10.00
               MOVE 10.00 TO YDBT-AMOUNT-2-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YDBT-AMOUNT-2-AMT > 50000.00
               MOVE 50000.00 TO YDBT-AMOUNT-2-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON AMOUNT 2 YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3430-ACCRUE-AMOUNT-2.
           IF YDBM-AMOUNT-2-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   YDBM-AMOUNT-2-YTD * YDBM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO YDBT-AMOUNT-1-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CONTROL AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3440-SPLIT-CONTROL.
           COMPUTE WS-WORK-AMT = YDBM-CONTROL-AMT / 4
           COMPUTE WS-WORK-AMT-2 = YDBM-CONTROL-AMT - (WS-WORK-AMT * 4)
           COMPUTE YDBT-AMOUNT-2-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE AMOUNT 1 BAND TIER FOR THE AMOUNT 2 AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3450-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-AMOUNT-1-BAND-RATE(1) TO YDBM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF YDBT-AMOUNT-2-AMT NOT < WS-AMOUNT-1-BAND-LOW(WS-TX)
                   MOVE WS-AMOUNT-1-BAND-RATE(WS-TX) TO YDBM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF YDBM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO YDBM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3460-DESCRIBE-TRAN.
           EVALUATE YDBT-TRAN-CD
               WHEN 'CV'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'RV'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'TS'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3470-AGE-LAST-ACT.
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
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E761 OPERATOR REQUIRED
      * E744 WORK AMT NEGATIVE
      * E752 WORK AMT OVER 99999
      * E145 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3480-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YDBT-OPERATOR = SPACES
                   MOVE 'E761' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E744' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 99999.00
                   MOVE 'E752' TO WS-REJECT-CODE
               WHEN YDBM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E145' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3490-ACCRUE-MARGIN.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * YDBM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO YDBT-CONTROL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * FJ -> LA WHEN LAST PAY DATE IS PAST
      * LA -> SP WHEN LAST PAY DATE IS PAST
      * SP -> UC WHEN LAST PAY DATE IS PAST
      * UC -> FJ WHEN LAST PAY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3500-SET-CYCLE.
           EVALUATE YDBM-CYCLE-CD
               WHEN 'FJ'
                   IF YDBM-LAST-PAY-DATE > 0 AND YDBM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LA' TO YDBM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LA'
                   IF YDBM-LAST-PAY-DATE > 0 AND YDBM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SP' TO YDBM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SP'
                   IF YDBM-LAST-PAY-DATE > 0 AND YDBM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UC' TO YDBM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UC'
                   IF YDBM-LAST-PAY-DATE > 0 AND YDBM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FJ' TO YDBM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE AMOUNT 1 BAND TIER FOR THE AMOUNT 1 AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3510-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-AMOUNT-1-BAND-RATE(1) TO YDBM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF YDBM-AMOUNT-1-AMT NOT < WS-AMOUNT-1-BAND-LOW(WS-TX)
                   MOVE WS-AMOUNT-1-BAND-RATE(WS-TX) TO YDBM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF YDBM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO YDBM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE AMOUNT 1 AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO AMOUNT 1 YTD.
      *----------------------------------------------------------------*
       3520-PRORATE-AMOUNT-1.
           IF WS-PERIOD-DAYS > 0
               COMPUTE YDBM-AMOUNT-1-YTD ROUNDED =
                   YDBM-AMOUNT-1-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO YDBM-AMOUNT-1-YTD
           END-IF
           ADD YDBM-AMOUNT-1-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E220 NAME REQUIRED
      * E514 AMOUNT 1 YTD NEGATIVE
      * E756 AMOUNT 1 YTD OVER 99999
      * E516 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3530-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YDBM-NAME = SPACES
                   MOVE 'E220' TO WS-REJECT-CODE
               WHEN YDBM-AMOUNT-1-YTD < 0
                   MOVE 'E514' TO WS-REJECT-CODE
               WHEN YDBM-AMOUNT-1-YTD > 99999.00
                   MOVE 'E756' TO WS-REJECT-CODE
               WHEN YDBM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E516' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT AMOUNT 2 YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3540-SPLIT-AMOUNT-2.
           COMPUTE WS-WORK-AMT = YDBM-AMOUNT-2-YTD / 12
           COMPUTE WS-WORK-AMT-2 = YDBM-AMOUNT-2-YTD - (WS-WORK-AMT *
               12)
           COMPUTE YDBM-CONTROL-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CONTROL AMT IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3550-LIMIT-CONTROL.
           IF YDBT-CONTROL-AMT < 10.00
               MOVE 10.00 TO YDBT-CONTROL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YDBT-CONTROL-AMT > 1000.00
               MOVE 1000.00 TO YDBT-CONTROL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CONTROL AMT BY WINDOW RULE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3560-ACCUM-WINDOW-RULE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YDBM-WINDOW-RULE-CD
                   ADD YDBT-CONTROL-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YDBM-WINDOW-RULE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE YDBT-CONTROL-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3570-COUNT-CYCLE.
           IF YDBM-CYCLE-CNT < 9999999
               ADD 1 TO YDBM-CYCLE-CNT
           END-IF
           IF YDBM-CYCLE-CNT NOT < 12
               MOVE 'Y' TO YDBM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3580-COUNT-CYCLE.
           IF YDBM-CYCLE-CNT < 9999999
               ADD 1 TO YDBM-CYCLE-CNT
           END-IF
           IF YDBM-CYCLE-CNT NOT < 6
               MOVE 'Y' TO YDBM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3590-AGE-NOTICE.
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
                   ADD YDBM-AMOUNT-1-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD YDBM-AMOUNT-1-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD YDBM-AMOUNT-1-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD YDBM-AMOUNT-1-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD YDBM-AMOUNT-1-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3600-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YDBM-CYCLE-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YDBM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE AMOUNT 1 AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3610-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YDBM-REGION-CD
                   ADD YDBT-AMOUNT-1-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YDBM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE YDBT-AMOUNT-1-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E507 ADDR REQUIRED
      * E992 AMOUNT 2 AMT NEGATIVE
      * E094 AMOUNT 2 AMT OVER 50000
      * E627 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3620-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YDBM-ADDR = SPACES
                   MOVE 'E507' TO WS-REJECT-CODE
               WHEN YDBM-AMOUNT-2-AMT < 0
                   MOVE 'E992' TO WS-REJECT-CODE
               WHEN YDBM-AMOUNT-2-AMT > 50000.00
                   MOVE 'E094' TO WS-REJECT-CODE
               WHEN YDBT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E627' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E488 CUST ID REQUIRED
      * E646 AMOUNT 2 YTD NEGATIVE
      * E857 AMOUNT 2 YTD OVER 10000
      * E711 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3630-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YDBM-CUST-ID = SPACES
                   MOVE 'E488' TO WS-REJECT-CODE
               WHEN YDBM-AMOUNT-2-YTD < 0
                   MOVE 'E646' TO WS-REJECT-CODE
               WHEN YDBM-AMOUNT-2-YTD > 10000.00
                   MOVE 'E857' TO WS-REJECT-CODE
               WHEN YDBT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E711' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE AMOUNT 2 AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO AMOUNT 1 YTD.
      *----------------------------------------------------------------*
       3640-PRORATE-AMOUNT-1.
           IF WS-PERIOD-DAYS > 0
               COMPUTE YDBM-AMOUNT-1-YTD ROUNDED =
                   YDBM-AMOUNT-2-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO YDBM-AMOUNT-1-YTD
           END-IF
           ADD YDBM-AMOUNT-1-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E069 REJECT CD REQUIRED
      * E111 AMOUNT 2 AMT NEGATIVE
      * E784 AMOUNT 2 AMT OVER 10000
      * E250 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3650-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YDBT-REJECT-CD = SPACES
                   MOVE 'E069' TO WS-REJECT-CODE
               WHEN YDBM-AMOUNT-2-AMT < 0
                   MOVE 'E111' TO WS-REJECT-CODE
               WHEN YDBM-AMOUNT-2-AMT > 10000.00
                   MOVE 'E784' TO WS-REJECT-CODE
               WHEN YDBM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E250' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3660-SPLIT-AMOUNT-1.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 12)
           COMPUTE YDBM-AMOUNT-1-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3670-CHECK-NOTICE.
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
      * PRORATE AMOUNT 2 AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO AMOUNT 2 AMT.
      *----------------------------------------------------------------*
       3680-PRORATE-AMOUNT-2.
           IF WS-PERIOD-DAYS > 0
               COMPUTE YDBM-AMOUNT-2-AMT ROUNDED =
                   YDBT-AMOUNT-2-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO YDBM-AMOUNT-2-AMT
           END-IF
           ADD YDBM-AMOUNT-2-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E461 ADDR REQUIRED
      * E401 AMOUNT 2 YTD NEGATIVE
      * E643 AMOUNT 2 YTD OVER 250000
      * E843 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3690-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YDBM-ADDR = SPACES
                   MOVE 'E461' TO WS-REJECT-CODE
               WHEN YDBM-AMOUNT-2-YTD < 0
                   MOVE 'E401' TO WS-REJECT-CODE
               WHEN YDBM-AMOUNT-2-YTD > 250000.00
                   MOVE 'E643' TO WS-REJECT-CODE
               WHEN YDBM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E843' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO AMOUNT 1 AMT.
      *----------------------------------------------------------------*
       3700-PRORATE-AMOUNT-1.
           IF WS-PERIOD-DAYS > 0
               COMPUTE YDBM-AMOUNT-1-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO YDBM-AMOUNT-1-AMT
           END-IF
           ADD YDBM-AMOUNT-1-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 365 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3710-CHECK-LAST-PAY.
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
           IF WS-DAYS > 365
               MOVE 'Y' TO YDBM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CONTROL AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3720-FLAG-CONTROL.
           IF YDBM-CONTROL-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE YDBM-CONTROL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT AMOUNT 1 YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3730-SPLIT-AMOUNT-1.
           COMPUTE WS-WORK-AMT = YDBM-AMOUNT-1-YTD / 4
           COMPUTE WS-WORK-AMT-2 = YDBM-AMOUNT-1-YTD - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE AMOUNT 1 BAND TIER FOR THE CONTROL YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3740-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-AMOUNT-1-BAND-RATE(1) TO YDBM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF YDBM-CONTROL-YTD NOT < WS-AMOUNT-1-BAND-LOW(WS-TX)
                   MOVE WS-AMOUNT-1-BAND-RATE(WS-TX) TO YDBM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF YDBM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO YDBM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3750-DESCRIBE-REGION.
           EVALUATE YDBM-REGION-CD
               WHEN 'C'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'H'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'K'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'S'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'V'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'W'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'X'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AMOUNT 1 AMT IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3760-LIMIT-AMOUNT-1.
           IF YDBT-AMOUNT-1-AMT < 50.00
               MOVE 50.00 TO YDBT-AMOUNT-1-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YDBT-AMOUNT-1-AMT > 2500.00
               MOVE 2500.00 TO YDBT-AMOUNT-1-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-15, 16-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3770-AGE-EFF.
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
                   ADD YDBM-CONTROL-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD YDBM-CONTROL-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD YDBM-CONTROL-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD YDBM-CONTROL-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD YDBM-CONTROL-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3780-CHECK-LAST-PAY.
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
           IF WS-DAYS > 30
               MOVE 'Y' TO YDBM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CONTROL YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3790-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN YDBM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO YDBM-CONTROL-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YDBM-REVIEW-FLAG = 'Y'
                   COMPUTE YDBM-CONTROL-YTD ROUNDED = WS-WORK-AMT-2 *
                       YDBM-PENALTY-PCT / 200
               WHEN YDBM-SOURCE-CD = 'BNX'
                   MOVE 5.00 TO YDBM-CONTROL-YTD
               WHEN YDBM-SOURCE-CD = 'FTB'
                   MOVE 7.00 TO YDBM-CONTROL-YTD
               WHEN OTHER
                   COMPUTE YDBM-CONTROL-YTD ROUNDED = WS-WORK-AMT-2 *
                       YDBM-PENALTY-PCT / 100
           END-EVALUATE
           IF YDBM-CONTROL-YTD > 0 AND YDBM-CONTROL-YTD < 5.00
               MOVE 5.00 TO YDBM-CONTROL-YTD
           END-IF
           IF YDBM-CONTROL-YTD > 150.00
               MOVE 150.00 TO YDBM-CONTROL-YTD
           END-IF
           ADD YDBM-CONTROL-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E280 CONTACT REQUIRED
      * E080 AMOUNT 2 AMT NEGATIVE
      * E922 AMOUNT 2 AMT OVER 25000
      * E813 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3800-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YDBM-CONTACT = SPACES
                   MOVE 'E280' TO WS-REJECT-CODE
               WHEN YDBM-AMOUNT-2-AMT < 0
                   MOVE 'E080' TO WS-REJECT-CODE
               WHEN YDBM-AMOUNT-2-AMT > 25000.00
                   MOVE 'E922' TO WS-REJECT-CODE
               WHEN YDBM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E813' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CONTROL SCHED TIER FOR THE CONTROL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3810-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CONTROL-SCHED-RATE(1) TO YDBM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF YDBT-CONTROL-AMT NOT < WS-CONTROL-SCHED-LOW(WS-TX)
                   MOVE WS-CONTROL-SCHED-RATE(WS-TX) TO YDBM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF YDBM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO YDBM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3820-AGE-POST.
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
      * DAYS SINCE OPEN DATE. MORE THAN 60 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3830-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF YDBM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE YDBM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO YDBM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CONTROL AMT IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3840-LIMIT-CONTROL.
           IF YDBM-CONTROL-AMT < 10.00
               MOVE 10.00 TO YDBM-CONTROL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YDBM-CONTROL-AMT > 5000.00
               MOVE 5000.00 TO YDBM-CONTROL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CONTROL YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3850-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = YDBT-CHANNEL-CD
                   ADD YDBM-CONTROL-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE YDBT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE YDBM-CONTROL-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CONTROL AMT.
      *----------------------------------------------------------------*
       3860-PRORATE-CONTROL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE YDBM-CONTROL-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO YDBM-CONTROL-AMT
           END-IF
           ADD YDBM-CONTROL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3870-DESCRIBE-CHANNEL.
           EVALUATE YDBT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3880-COUNT-RETRY.
           IF YDBM-RETRY-CNT < 9999999
               ADD 1 TO YDBM-RETRY-CNT
           END-IF
           IF YDBM-RETRY-CNT NOT < 10
               MOVE 'Y' TO YDBM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT 2 YTD IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3890-LIMIT-AMOUNT-2.
           IF YDBM-AMOUNT-2-YTD < 25.00
               MOVE 25.00 TO YDBM-AMOUNT-2-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YDBM-AMOUNT-2-YTD > 5000.00
               MOVE 5000.00 TO YDBM-AMOUNT-2-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3900-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 10.00
               MOVE 10.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 2500.00
               MOVE 2500.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT 2 YTD IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3910-LIMIT-AMOUNT-2.
           IF YDBM-AMOUNT-2-YTD < 10.00
               MOVE 10.00 TO YDBM-AMOUNT-2-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YDBM-AMOUNT-2-YTD > 1000.00
               MOVE 1000.00 TO YDBM-AMOUNT-2-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE AMOUNT 1 BAND TIER FOR THE CONTROL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3920-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-AMOUNT-1-BAND-RATE(1) TO YDBM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF YDBM-CONTROL-AMT NOT < WS-AMOUNT-1-BAND-LOW(WS-TX)
                   MOVE WS-AMOUNT-1-BAND-RATE(WS-TX) TO YDBM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF YDBM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO YDBM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE AMOUNT 1 YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3930-ACCUM-REGION.
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
      * SPLIT AMOUNT 2 YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3940-SPLIT-AMOUNT-2.
           COMPUTE WS-WORK-AMT = YDBM-AMOUNT-2-YTD / 3
           COMPUTE WS-WORK-AMT-2 = YDBM-AMOUNT-2-YTD - (WS-WORK-AMT * 3)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * FJ -> LA WHEN EFF DATE IS PAST
      * LA -> SP WHEN EFF DATE IS PAST
      * SP -> UC WHEN EFF DATE IS PAST
      * UC -> FJ WHEN EFF DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3950-SET-CYCLE.
           EVALUATE YDBM-CYCLE-CD
               WHEN 'FJ'
                   IF YDBT-EFF-DATE > 0 AND YDBT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LA' TO YDBM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LA'
                   IF YDBT-EFF-DATE > 0 AND YDBT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SP' TO YDBM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SP'
                   IF YDBT-EFF-DATE > 0 AND YDBT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UC' TO YDBM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UC'
                   IF YDBT-EFF-DATE > 0 AND YDBT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FJ' TO YDBM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E670 ADDR REQUIRED
      * E089 AMOUNT 2 AMT NEGATIVE
      * E124 AMOUNT 2 AMT OVER 25000
      * E836 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3960-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YDBM-ADDR = SPACES
                   MOVE 'E670' TO WS-REJECT-CODE
               WHEN YDBM-AMOUNT-2-AMT < 0
                   MOVE 'E089' TO WS-REJECT-CODE
               WHEN YDBM-AMOUNT-2-AMT > 25000.00
                   MOVE 'E124' TO WS-REJECT-CODE
               WHEN YDBM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E836' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE AMOUNT 1 FEE ON THE AMOUNT 2 AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3970-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN YDBM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO YDBT-AMOUNT-1-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN YDBM-PREFERRED-FLAG = 'Y'
                   COMPUTE YDBT-AMOUNT-1-AMT ROUNDED = YDBM-AMOUNT-2-AMT
                       * YDBM-DISCOUNT / 200
               WHEN YDBT-TRAN-CD = 'CV'
                   MOVE 12.00 TO YDBT-AMOUNT-1-AMT
               WHEN YDBT-TRAN-CD = 'RV'
                   MOVE 3.00 TO YDBT-AMOUNT-1-AMT
               WHEN OTHER
                   COMPUTE YDBT-AMOUNT-1-AMT ROUNDED = YDBM-AMOUNT-2-AMT
                       * YDBM-DISCOUNT / 100
           END-EVALUATE
           IF YDBT-AMOUNT-1-AMT > 0 AND YDBT-AMOUNT-1-AMT < 10.00
               MOVE 10.00 TO YDBT-AMOUNT-1-AMT
           END-IF
           IF YDBT-AMOUNT-1-AMT > 150.00
               MOVE 150.00 TO YDBT-AMOUNT-1-AMT
           END-IF
           ADD YDBT-AMOUNT-1-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3980-CHECK-EXPIRY.
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
           IF WS-DAYS > 180
               MOVE 'Y' TO YDBM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E277 MEMO REQUIRED
      * E474 AMOUNT 1 AMT NEGATIVE
      * E800 AMOUNT 1 AMT OVER 25000
      * E019 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3990-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YDBT-MEMO = SPACES
                   MOVE 'E277' TO WS-REJECT-CODE
               WHEN YDBM-AMOUNT-1-AMT < 0
                   MOVE 'E474' TO WS-REJECT-CODE
               WHEN YDBM-AMOUNT-1-AMT > 25000.00
                   MOVE 'E800' TO WS-REJECT-CODE
               WHEN YDBM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E019' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT AMOUNT 1 AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4000-SPLIT-AMOUNT-1.
           COMPUTE WS-WORK-AMT = YDBT-AMOUNT-1-AMT / 2
           COMPUTE WS-WORK-AMT-2 = YDBT-AMOUNT-1-AMT - (WS-WORK-AMT * 2)
           COMPUTE YDBT-AMOUNT-2-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * AX -> LK WHEN NEXT DUE DATE IS PAST
      * LK -> NT WHEN NEXT DUE DATE IS PAST
      * NT -> UW WHEN NEXT DUE DATE IS PAST
      * UW -> AX WHEN NEXT DUE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4010-SET-REASON.
           EVALUATE YDBM-REASON-CD
               WHEN 'AX'
                   IF YDBM-NEXT-DUE-DATE > 0 AND YDBM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LK' TO YDBM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LK'
                   IF YDBM-NEXT-DUE-DATE > 0 AND YDBM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NT' TO YDBM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NT'
                   IF YDBM-NEXT-DUE-DATE > 0 AND YDBM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UW' TO YDBM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UW'
                   IF YDBM-NEXT-DUE-DATE > 0 AND YDBM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AX' TO YDBM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON AMOUNT 2 AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4020-ACCRUE-AMOUNT-2.
           IF YDBT-AMOUNT-2-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   YDBT-AMOUNT-2-AMT * YDBM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO YDBT-CONTROL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT 2 AMT IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4030-LIMIT-AMOUNT-2.
           IF YDBM-AMOUNT-2-AMT < 10.00
               MOVE 10.00 TO YDBM-AMOUNT-2-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF YDBM-AMOUNT-2-AMT > 5000.00
               MOVE 5000.00 TO YDBM-AMOUNT-2-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CONTROL AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4040-SPLIT-CONTROL.
           COMPUTE WS-WORK-AMT = YDBM-CONTROL-AMT / 3
           COMPUTE WS-WORK-AMT-2 = YDBM-CONTROL-AMT - (WS-WORK-AMT * 3)
           COMPUTE YDBM-AMOUNT-1-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E386 CONTACT REQUIRED
      * E601 WORK AMT NEGATIVE
      * E207 WORK AMT OVER 250000
      * E993 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       4050-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN YDBM-CONTACT = SPACES
                   MOVE 'E386' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E601' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 250000.00
                   MOVE 'E207' TO WS-REJECT-CODE
               WHEN YDBM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E993' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON AMOUNT 2 AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4060-ACCRUE-AMOUNT-2.
           IF YDBT-AMOUNT-2-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   YDBT-AMOUNT-2-AMT * YDBM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE AMOUNT 1 BAND TIER FOR THE AMOUNT 2 AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4070-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-AMOUNT-1-BAND-RATE(1) TO YDBM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF YDBT-AMOUNT-2-AMT NOT < WS-AMOUNT-1-BAND-LOW(WS-TX)
                   MOVE WS-AMOUNT-1-BAND-RATE(WS-TX) TO YDBM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF YDBM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO YDBM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON AMOUNT 1 YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4080-ACCRUE-AMOUNT-1.
           IF YDBM-AMOUNT-1-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   YDBM-AMOUNT-1-YTD * YDBM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO YDBM-AMOUNT-1-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4090-CHECK-NOTICE.
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
           IF WS-DAYS > 120
               MOVE 'Y' TO YDBM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 120 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4100-CHECK-EFF.
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
           IF WS-DAYS > 120
               MOVE 'Y' TO YDBM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE YDB FIELDS TO YKTIER AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7010-CALL-YKTIER.
           INITIALIZE YKTIER-PARMS
           MOVE YDBM-AMOUNT-2-YTD
             TO YKTIER-AMOUNT-2-YTD
           MOVE YDBM-CONTROL-YTD
             TO YKTIER-CONTROL-YTD
           MOVE YDBM-AMOUNT-1-YTD
             TO YKTIER-AMOUNT-1-YTD
           MOVE YDBM-AMOUNT-2-AMT
             TO YKTIER-AMOUNT-2-AMT
           MOVE YDBM-AMOUNT-1-AMT
             TO YKTIER-AMOUNT-1-AMT
           MOVE YDBM-CONTROL-AMT
             TO YKTIER-CONTROL-AMT
           MOVE YDBM-LAST-ACT-DATE
             TO YKTIER-LAST-ACT-DATE
           MOVE YDBM-MATURITY-DATE
             TO YKTIER-MATURITY-DATE
           MOVE YDBM-LAST-MAINT-DATE
             TO YKTIER-LAST-MAINT-DATE
           MOVE YDBM-EXPIRY-DATE
             TO YKTIER-EXPIRY-DATE
           MOVE YDBM-CYCLE-CD
             TO YKTIER-CYCLE-CD
           MOVE YDBM-SOURCE-CD
             TO YKTIER-SOURCE-CD
           MOVE YDBM-REGION-CD
             TO YKTIER-REGION-CD
           MOVE YDBM-PIVOT-CD
             TO YKTIER-PIVOT-CD
           MOVE YDBM-PREFERRED-FLAG
             TO YKTIER-PREFERRED-FLAG
           MOVE YDBM-AUTO-RENEW-FLAG
             TO YKTIER-AUTO-RENEW-FLAG
           MOVE YDBM-REVIEW-FLAG
             TO YKTIER-REVIEW-FLAG
           MOVE YDBM-ITEM-CNT
             TO YKTIER-ITEM-CNT
           MOVE YDBM-RETRY-CNT
             TO YKTIER-RETRY-CNT
           MOVE YDBM-DISCOUNT
             TO YKTIER-DISCOUNT
           MOVE YDBM-PENALTY-PCT
             TO YKTIER-PENALTY-PCT
           MOVE YDBM-CUST-ID
             TO YKTIER-CUST-ID
           MOVE YDBM-CONTACT
             TO YKTIER-CONTACT
           CALL 'YKTIER' USING YKTIER-PARMS
           EVALUATE YKTIER-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE YKTIER-AMOUNT-2-YTD
                     TO YDBM-AMOUNT-2-YTD
                   MOVE YKTIER-AMOUNT-1-YTD
                     TO YDBM-AMOUNT-1-YTD
                   MOVE YKTIER-AMOUNT-1-AMT
                     TO YDBM-AMOUNT-1-AMT
                   MOVE YKTIER-CONTROL-AMT
                     TO YDBM-CONTROL-AMT
                   MOVE YKTIER-REVIEW-FLAG
                     TO YDBM-REVIEW-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RIER' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-YKTIER' TO ABND-PARAGRAPH
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
           DISPLAY 'YKBCLSE READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'YKBCLSE' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
