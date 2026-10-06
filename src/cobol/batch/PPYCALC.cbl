      *================================================================*
      * PROGRAM  : PPYCALC                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : POSITIVE PAY - PERIODIC CALCULATION. READS THE PPY  *
      *            TRANSACTIONS, UPDATES THE PPY MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. CORPORATE POSITIVE PAY ISSUE    *
      *            FILE MATCHING AND EXCEPTIONS                        *
      * INPUT    : PPYMST (KSDS), PPYTRN, BANKCTL                      *
      * OUTPUT   : PPYOUT, PPYCARPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, PPELG                   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-01-14 PQR  ORIGINAL                                       *
      * 1997-11-04 CRP  ISSUED CALCULATION CORRECTED PRB2827           *
      * 1997-12-09 BHS  AUDIT FINDING - EXCEPT CONTROL TOTALS INC7543  *
      * 2000-11-08 GAF  AUDIT FINDING - ISSUED CONTROL TOTALS REQ3641  *
      * 2001-07-08 PQR  EXCEPT CUTOFF MOVED PRB3276                    *
      * 2003-02-24 BHS  PERFORMANCE - EXCEPT READ AHEAD PRB6244        *
      * 2003-03-08 CRP  EXCEPT FEE SCHEDULE UPDATE REQ6377             *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PPYCALC.
       AUTHOR. R J TRAVERS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "PPYMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS PPYM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "PPYTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "PPYOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "PPYCARPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY PPYMST.
       FD  TRAN-FILE.
       COPY PPYTRN.
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
                                           VALUE 23.000000.
           05  WS-DAYS                     PIC S9(07) VALUE 0.
           05  WS-PERIOD-DAYS              PIC S9(05) VALUE 30.
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
      * WS-EXCEPT-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-EXCEPT-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000273500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000793500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015000000000846000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017500000000670500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017550000000892000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022550000000829000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022600000000614500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025100000000119000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025350000000258500'.
       01  WS-EXCEPT-GRID-TBL REDEFINES WS-EXCEPT-GRID-VALUES.
           05  WS-EXCEPT-GRID-ROW OCCURS 9 TIMES.
               10  WS-EXCEPT-GRID-LOW      PIC 9(09)V99.
               10  WS-EXCEPT-GRID-RATE     PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-PAID-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PAID-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000048500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000107000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000028500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000600000000778000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005600000000150000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005650000000094000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005900000000183000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006000000000830500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006100000000276000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008600000000819000'.
       01  WS-PAID-GRID-TBL REDEFINES WS-PAID-GRID-VALUES.
           05  WS-PAID-GRID-ROW OCCURS 10 TIMES.
               10  WS-PAID-GRID-LOW        PIC 9(09)V99.
               10  WS-PAID-GRID-RATE       PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-STALE-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-STALE-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000632000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000843000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000070000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005250000000137000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007750000000079500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017750000000653000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020250000000240000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025250000000360000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026250000000081000'.
       01  WS-STALE-TIER-TBL REDEFINES WS-STALE-TIER-VALUES.
           05  WS-STALE-TIER-ROW OCCURS 9 TIMES.
               10  WS-STALE-TIER-LOW       PIC 9(09)V99.
               10  WS-STALE-TIER-RATE      PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  PPYCALC  POSITIVE PAY'.
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
       COPY PPELGP.

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
               MOVE 'PPYMST' TO ABND-FILE-NAME
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
           MOVE PPYT-KEY TO PPYM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-SPLIT-PAID
               PERFORM 3010-ASSESS-NOTICE-SENT
               PERFORM 3020-COUNT-NOTICE
               PERFORM 3030-AGE-LAST-ACT
               PERFORM 3040-DESCRIBE-REASON
               PERFORM 3050-FLAG-STALE
               PERFORM 3060-AGE-LAST-PAY
               PERFORM 3070-ASSESS-NOTICE-SENT
               PERFORM 3080-LIMIT-PAID
               PERFORM 3090-SELECT-RATE
               PERFORM 3100-SET-STATUS
               PERFORM 3110-COUNT-CYCLE
               PERFORM 3120-DESCRIBE-REGION
               PERFORM 3130-ACCUM-TRAN
               PERFORM 3140-EDIT-CONTACT
               PERFORM 3150-FLAG-PAID
               PERFORM 3160-EDIT-REJECT
               PERFORM 3170-SPLIT-ISSUED
               PERFORM 3180-ASSESS-REVERSAL
               PERFORM 3190-CHECK-NEXT-DUE
               PERFORM 3200-LIMIT-VOID
               PERFORM 3210-ASSESS-STOP
               PERFORM 3220-CHECK-NEXT-REVIEW
               PERFORM 3230-ASSESS-EXEMPT
               PERFORM 3240-SPLIT-ISSUED
               PERFORM 3250-ACCRUE-PAID
               PERFORM 3260-ASSESS-REVERSAL
               PERFORM 3270-FLAG-PAID
               PERFORM 3280-ACCUM-REGION
               PERFORM 3290-DESCRIBE-STATUS
               PERFORM 3300-DESCRIBE-MATCH-RULE
               PERFORM 3310-SELECT-DISCOUNT
               PERFORM 3320-LIMIT-EXCEPT
               PERFORM 3330-SELECT-DISCOUNT
               PERFORM 3340-COUNT-EXCEPT
               PERFORM 3350-PRORATE-VOID
               PERFORM 3360-AGE-LAST-PAY
               PERFORM 3370-EDIT-NAME
               PERFORM 3380-ACCUM-REASON
               PERFORM 3390-SELECT-MARGIN
               PERFORM 3400-ASSESS-OVERRIDE
               PERFORM 3410-ACCRUE-STALE
               PERFORM 3420-ACCRUE-VOID
               PERFORM 3430-CHECK-NEXT-DUE
               PERFORM 3440-SPLIT-STALE
               PERFORM 3450-AGE-EFF
               PERFORM 3460-LIMIT-ITEM
               PERFORM 3470-DESCRIBE-REASON
               PERFORM 3480-ASSESS-REVERSAL
               PERFORM 3490-SPLIT-ISSUED
               PERFORM 3500-ACCUM-TRAN
               PERFORM 3510-SET-TRAN
               PERFORM 3520-SET-REGION
               PERFORM 3530-LIMIT-ISSUED
               PERFORM 3540-PRORATE-PAID
               PERFORM 3550-FLAG-ISSUED
               PERFORM 3560-SET-CHANNEL
               PERFORM 3570-EDIT-MEMO
               PERFORM 3580-ACCUM-MATCH-RULE
               PERFORM 3590-CHECK-LAST-ACT
               PERFORM 3600-FLAG-EXCEPT
               PERFORM 3610-ASSESS-EXEMPT
               PERFORM 3620-SET-TRAN
               PERFORM 3630-EDIT-ADDR
               PERFORM 3640-DESCRIBE-REASON
               PERFORM 3650-AGE-LAST-MAINT
               PERFORM 3660-LIMIT-ITEM
               PERFORM 3670-PRORATE-EXCEPT
               PERFORM 3680-ASSESS-EXEMPT
               PERFORM 3690-SPLIT-EXCEPT
               PERFORM 3700-SPLIT-STALE
               PERFORM 3710-FLAG-STALE
               PERFORM 3720-AGE-CLOSE
               PERFORM 3730-FLAG-ITEM
               PERFORM 3740-AGE-LAST-PAY
               PERFORM 3750-SET-TRAN
               PERFORM 3760-SET-CHANNEL
               PERFORM 3770-PRORATE-PAID
               PERFORM 3780-LIMIT-ISSUED
               PERFORM 3790-ASSESS-HOLD
               PERFORM 3800-PRORATE-ISSUED
               PERFORM 3810-SPLIT-ISSUED
               PERFORM 3820-SELECT-DISCOUNT
               PERFORM 3830-COUNT-EXCEPT
               PERFORM 3840-SELECT-MARGIN
               PERFORM 3850-DESCRIBE-CYCLE
               PERFORM 3860-COUNT-PAYMENT
               PERFORM 3870-CHECK-POST
               PERFORM 3880-DESCRIBE-MATCH-RULE
               PERFORM 3890-PRORATE-VOID
               PERFORM 3900-FLAG-EXCEPT
               PERFORM 3910-FLAG-EXCEPT
               PERFORM 3920-ACCRUE-RATE
               PERFORM 3930-DESCRIBE-CYCLE
               PERFORM 3940-PRORATE-ISSUED
               PERFORM 3950-CHECK-CLOSE
               PERFORM 3960-SELECT-RATE
               PERFORM 3970-AGE-EFFECTIVE
               PERFORM 3980-PRORATE-PAID
               PERFORM 3990-DESCRIBE-MATCH-RULE
               PERFORM 4000-SPLIT-VOID
               PERFORM 4010-ASSESS-NOTICE-SENT
               PERFORM 4020-SPLIT-STALE
               PERFORM 4030-EDIT-DESC
               PERFORM 4040-DESCRIBE-MATCH-RULE
               PERFORM 4050-LIMIT-ITEM
               PERFORM 4060-SET-REASON
               PERFORM 4070-SPLIT-VOID
               PERFORM 4080-FLAG-STALE
               PERFORM 4090-DESCRIBE-TRAN
               PERFORM 4100-ASSESS-STOP
               PERFORM 4110-CHECK-OPEN
               PERFORM 4120-ACCRUE-VOID
               PERFORM 4130-PRORATE-PAID
               PERFORM 4140-AGE-OPEN
               PERFORM 4150-DESCRIBE-TRAN
               PERFORM 4160-EDIT-NAME
               PERFORM 4170-ASSESS-NOTICE-SENT
               PERFORM 4180-SET-CHANNEL
               PERFORM 4190-SET-REASON
               PERFORM 4200-COUNT-PAYMENT
               PERFORM 4210-SPLIT-EXCEPT
               PERFORM 4220-DESCRIBE-DECISION
               PERFORM 4230-FLAG-ISSUED
               PERFORM 4240-ACCRUE-ISSUED
               PERFORM 4250-SELECT-DISCOUNT
               PERFORM 4260-ACCRUE-STALE
               PERFORM 7020-CALL-PPELG
               REWRITE PPY-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'PPYMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM PPY-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * SPLIT PAID YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3000-SPLIT-PAID.
           COMPUTE WS-WORK-AMT = PPYM-PAID-YTD / 2
           COMPUTE WS-WORK-AMT-2 = PPYM-PAID-YTD - (WS-WORK-AMT * 2)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ISSUED FEE ON THE WORK AMT 2.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3010-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN PPYM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO PPYM-ISSUED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYM-STOP-FLAG = 'Y'
                   COMPUTE PPYM-ISSUED-AMT ROUNDED = WS-WORK-AMT-2 *
                       PPYM-RATE / 200
               WHEN PPYM-REGION-CD = 'A'
                   MOVE 5.00 TO PPYM-ISSUED-AMT
               WHEN PPYM-REGION-CD = 'B'
                   MOVE 5.00 TO PPYM-ISSUED-AMT
               WHEN OTHER
                   COMPUTE PPYM-ISSUED-AMT ROUNDED = WS-WORK-AMT-2 *
                       PPYM-RATE / 100
           END-EVALUATE
           IF PPYM-ISSUED-AMT > 0 AND PPYM-ISSUED-AMT < 5.00
               MOVE 5.00 TO PPYM-ISSUED-AMT
           END-IF
           IF PPYM-ISSUED-AMT > 50.00
               MOVE 50.00 TO PPYM-ISSUED-AMT
           END-IF
           ADD PPYM-ISSUED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3020-COUNT-NOTICE.
           IF PPYM-NOTICE-CNT < 9999999
               ADD 1 TO PPYM-NOTICE-CNT
           END-IF
           IF PPYM-NOTICE-CNT NOT < 10
               MOVE 'Y' TO PPYM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3030-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF PPYM-LAST-ACT-DATE > 0 AND PPYM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPYM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PPYT-ISSUED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PPYT-ISSUED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD PPYT-ISSUED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PPYT-ISSUED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPYT-ISSUED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3040-DESCRIBE-REASON.
           EVALUATE PPYM-REASON-CD
               WHEN 'B'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'M'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'N'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'U'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'V'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'Z'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STALE YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3050-FLAG-STALE.
           IF PPYM-STALE-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PPYM-STALE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3060-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF PPYM-LAST-PAY-DATE > 0 AND PPYM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPYM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE VOID AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3070-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN PPYM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYM-OVERRIDE-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = PPYT-VOID-AMT *
                       PPYM-MARGIN / 200
               WHEN PPYM-REGION-CD = 'A'
                   MOVE 3.00 TO WS-WORK-AMT
               WHEN PPYM-REGION-CD = 'B'
                   MOVE 3.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = PPYT-VOID-AMT *
                       PPYM-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 1.00
               MOVE 1.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 100.00
               MOVE 100.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PAID YTD IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3080-LIMIT-PAID.
           IF PPYM-PAID-YTD < 50.00
               MOVE 50.00 TO PPYM-PAID-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PPYM-PAID-YTD > 500.00
               MOVE 500.00 TO PPYM-PAID-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STALE TIER TIER FOR THE PAID YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3090-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-STALE-TIER-RATE(1) TO PPYM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF PPYM-PAID-YTD NOT < WS-STALE-TIER-LOW(WS-TX)
                   MOVE WS-STALE-TIER-RATE(WS-TX) TO PPYM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPYM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPYM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * DR -> TK WHEN OPEN DATE IS PAST
      * TK -> WX WHEN OPEN DATE IS PAST
      * WX -> DR WHEN OPEN DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3100-SET-STATUS.
           EVALUATE PPYM-STATUS-CD
               WHEN 'DR'
                   IF PPYM-OPEN-DATE > 0 AND PPYM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TK' TO PPYM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TK'
                   IF PPYM-OPEN-DATE > 0 AND PPYM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WX' TO PPYM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WX'
                   IF PPYM-OPEN-DATE > 0 AND PPYM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DR' TO PPYM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3110-COUNT-CYCLE.
           IF PPYM-CYCLE-CNT < 9999999
               ADD 1 TO PPYM-CYCLE-CNT
           END-IF
           IF PPYM-CYCLE-CNT NOT < 10
               MOVE 'Y' TO PPYM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3120-DESCRIBE-REGION.
           EVALUATE PPYM-REGION-CD
               WHEN 'A'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'B'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'D'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'H'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'N'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'Y'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE VOID AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3130-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPYT-TRAN-CD
                   ADD PPYM-VOID-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPYT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PPYM-VOID-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E391 CONTACT REQUIRED
      * E953 VOID YTD NEGATIVE
      * E014 VOID YTD OVER 1000000
      * E124 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3140-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPYM-CONTACT = SPACES
                   MOVE 'E391' TO WS-REJECT-CODE
               WHEN PPYM-VOID-YTD < 0
                   MOVE 'E953' TO WS-REJECT-CODE
               WHEN PPYM-VOID-YTD > 1000000.00
                   MOVE 'E014' TO WS-REJECT-CODE
               WHEN PPYM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E124' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAID AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3150-FLAG-PAID.
           IF PPYM-PAID-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PPYM-PAID-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E513 REJECT CD REQUIRED
      * E097 EXCEPT YTD NEGATIVE
      * E570 EXCEPT YTD OVER 5000
      * E925 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3160-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPYT-REJECT-CD = SPACES
                   MOVE 'E513' TO WS-REJECT-CODE
               WHEN PPYM-EXCEPT-YTD < 0
                   MOVE 'E097' TO WS-REJECT-CODE
               WHEN PPYM-EXCEPT-YTD > 5000.00
                   MOVE 'E570' TO WS-REJECT-CODE
               WHEN PPYM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E925' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ISSUED YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3170-SPLIT-ISSUED.
           COMPUTE WS-WORK-AMT = PPYM-ISSUED-YTD / 3
           COMPUTE WS-WORK-AMT-2 = PPYM-ISSUED-YTD - (WS-WORK-AMT * 3)
           COMPUTE PPYM-STALE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE STALE FEE ON THE EXCEPT YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3180-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN PPYT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO PPYM-STALE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYM-HOLD-FLAG = 'Y'
                   COMPUTE PPYM-STALE-AMT ROUNDED = PPYM-EXCEPT-YTD *
                       PPYM-MARGIN / 200
               WHEN PPYT-TRAN-CD = 'IS'
                   MOVE 3.00 TO PPYM-STALE-AMT
               WHEN PPYT-TRAN-CD = 'PD'
                   MOVE 15.00 TO PPYM-STALE-AMT
               WHEN OTHER
                   COMPUTE PPYM-STALE-AMT ROUNDED = PPYM-EXCEPT-YTD *
                       PPYM-MARGIN / 100
           END-EVALUATE
           IF PPYM-STALE-AMT > 0 AND PPYM-STALE-AMT < 2.00
               MOVE 2.00 TO PPYM-STALE-AMT
           END-IF
           IF PPYM-STALE-AMT > 75.00
               MOVE 75.00 TO PPYM-STALE-AMT
           END-IF
           ADD PPYM-STALE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3190-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF PPYM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPYM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO PPYM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * VOID AMT IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3200-LIMIT-VOID.
           IF PPYM-VOID-AMT < 100.00
               MOVE 100.00 TO PPYM-VOID-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PPYM-VOID-AMT > 5000.00
               MOVE 5000.00 TO PPYM-VOID-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE STALE FEE ON THE ISSUED AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3210-ASSESS-STOP.
           EVALUATE TRUE
               WHEN PPYM-STOP-FLAG = 'Y'
                   MOVE ZERO TO PPYT-STALE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYM-EXEMPT-FLAG = 'Y'
                   COMPUTE PPYT-STALE-AMT ROUNDED = PPYM-ISSUED-AMT *
                       PPYM-RATE / 200
               WHEN PPYM-CYCLE-CD = 'A'
                   MOVE 7.00 TO PPYT-STALE-AMT
               WHEN PPYM-CYCLE-CD = 'B'
                   MOVE 7.00 TO PPYT-STALE-AMT
               WHEN OTHER
                   COMPUTE PPYT-STALE-AMT ROUNDED = PPYM-ISSUED-AMT *
                       PPYM-RATE / 100
           END-EVALUATE
           IF PPYT-STALE-AMT > 0 AND PPYT-STALE-AMT < 10.00
               MOVE 10.00 TO PPYT-STALE-AMT
           END-IF
           IF PPYT-STALE-AMT > 250.00
               MOVE 250.00 TO PPYT-STALE-AMT
           END-IF
           ADD PPYT-STALE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 60 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3220-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF PPYM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPYM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO PPYT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE VOID FEE ON THE PAID AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3230-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN PPYM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO PPYT-VOID-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYT-REVERSAL-FLAG = 'Y'
                   COMPUTE PPYT-VOID-AMT ROUNDED = PPYM-PAID-AMT *
                       PPYM-MARGIN / 200
               WHEN PPYM-DECISION-CD = 'AND'
                   MOVE 20.00 TO PPYT-VOID-AMT
               WHEN PPYM-DECISION-CD = 'BER'
                   MOVE 15.00 TO PPYT-VOID-AMT
               WHEN OTHER
                   COMPUTE PPYT-VOID-AMT ROUNDED = PPYM-PAID-AMT *
                       PPYM-MARGIN / 100
           END-EVALUATE
           IF PPYT-VOID-AMT > 0 AND PPYT-VOID-AMT < 2.00
               MOVE 2.00 TO PPYT-VOID-AMT
           END-IF
           IF PPYT-VOID-AMT > 50.00
               MOVE 50.00 TO PPYT-VOID-AMT
           END-IF
           ADD PPYT-VOID-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT ISSUED AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3240-SPLIT-ISSUED.
           COMPUTE WS-WORK-AMT = PPYT-ISSUED-AMT / 12
           COMPUTE WS-WORK-AMT-2 = PPYT-ISSUED-AMT - (WS-WORK-AMT * 12)
           COMPUTE PPYT-PAID-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAID AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3250-ACCRUE-PAID.
           IF PPYT-PAID-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPYT-PAID-AMT * PPYM-RATE / 100 / 360
               ADD WS-WORK-AMT TO PPYM-ISSUED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE EXCEPT FEE ON THE WORK AMT 2.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3260-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN PPYT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO PPYM-EXCEPT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE PPYM-EXCEPT-AMT ROUNDED = WS-WORK-AMT-2 *
                       PPYM-RATE / 200
               WHEN PPYM-STATUS-CD = 'DR'
                   MOVE 20.00 TO PPYM-EXCEPT-AMT
               WHEN PPYM-STATUS-CD = 'TK'
                   MOVE 7.00 TO PPYM-EXCEPT-AMT
               WHEN OTHER
                   COMPUTE PPYM-EXCEPT-AMT ROUNDED = WS-WORK-AMT-2 *
                       PPYM-RATE / 100
           END-EVALUATE
           IF PPYM-EXCEPT-AMT > 0 AND PPYM-EXCEPT-AMT < 2.00
               MOVE 2.00 TO PPYM-EXCEPT-AMT
           END-IF
           IF PPYM-EXCEPT-AMT > 50.00
               MOVE 50.00 TO PPYM-EXCEPT-AMT
           END-IF
           ADD PPYM-EXCEPT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PAID AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3270-FLAG-PAID.
           IF PPYM-PAID-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PPYM-PAID-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE EXCEPT AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3280-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPYM-REGION-CD
                   ADD PPYM-EXCEPT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPYM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PPYM-EXCEPT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3290-DESCRIBE-STATUS.
           EVALUATE PPYM-STATUS-CD
               WHEN 'DR'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'TK'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'WX'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE MATCH RULE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3300-DESCRIBE-MATCH-RULE.
           EVALUATE PPYM-MATCH-RULE-CD
               WHEN 'A'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'B'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'F'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'M'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'S'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'V'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE EXCEPT GRID TIER FOR THE VOID AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3310-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-EXCEPT-GRID-RATE(1) TO PPYM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF PPYM-VOID-AMT NOT < WS-EXCEPT-GRID-LOW(WS-TX)
                   MOVE WS-EXCEPT-GRID-RATE(WS-TX) TO PPYM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPYM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPYM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT AMT IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3320-LIMIT-EXCEPT.
           IF PPYM-EXCEPT-AMT < 100.00
               MOVE 100.00 TO PPYM-EXCEPT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PPYM-EXCEPT-AMT > 1000.00
               MOVE 1000.00 TO PPYM-EXCEPT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAID GRID TIER FOR THE VOID AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3330-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-PAID-GRID-RATE(1) TO PPYM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF PPYT-VOID-AMT NOT < WS-PAID-GRID-LOW(WS-TX)
                   MOVE WS-PAID-GRID-RATE(WS-TX) TO PPYM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPYM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPYM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3340-COUNT-EXCEPT.
           IF PPYM-EXCEPT-CNT < 9999999
               ADD 1 TO PPYM-EXCEPT-CNT
           END-IF
           IF PPYM-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO PPYM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE EXCEPT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO VOID YTD.
      *----------------------------------------------------------------*
       3350-PRORATE-VOID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPYM-VOID-YTD ROUNDED =
                   PPYM-EXCEPT-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PPYM-VOID-YTD
           END-IF
           ADD PPYM-VOID-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3360-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF PPYM-LAST-PAY-DATE > 0 AND PPYM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPYM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PPYM-EXCEPT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD PPYM-EXCEPT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD PPYM-EXCEPT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD PPYM-EXCEPT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPYM-EXCEPT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E439 NAME REQUIRED
      * E711 PAID AMT NEGATIVE
      * E766 PAID AMT OVER 99999
      * E822 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3370-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPYM-NAME = SPACES
                   MOVE 'E439' TO WS-REJECT-CODE
               WHEN PPYM-PAID-AMT < 0
                   MOVE 'E711' TO WS-REJECT-CODE
               WHEN PPYM-PAID-AMT > 99999.00
                   MOVE 'E766' TO WS-REJECT-CODE
               WHEN PPYM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E822' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE VOID AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3380-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPYM-REASON-CD
                   ADD PPYT-VOID-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPYM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PPYT-VOID-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE EXCEPT GRID TIER FOR THE PAID AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3390-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-EXCEPT-GRID-RATE(1) TO PPYM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF PPYM-PAID-AMT NOT < WS-EXCEPT-GRID-LOW(WS-TX)
                   MOVE WS-EXCEPT-GRID-RATE(WS-TX) TO PPYM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPYM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPYM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAID YTD FEE ON THE PAID AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3400-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN PPYM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO PPYM-PAID-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYM-HOLD-FLAG = 'Y'
                   COMPUTE PPYM-PAID-YTD ROUNDED = PPYM-PAID-AMT *
                       PPYM-RATE / 200
               WHEN PPYT-TRAN-CD = 'IS'
                   MOVE 12.00 TO PPYM-PAID-YTD
               WHEN PPYT-TRAN-CD = 'PD'
                   MOVE 3.00 TO PPYM-PAID-YTD
               WHEN OTHER
                   COMPUTE PPYM-PAID-YTD ROUNDED = PPYM-PAID-AMT *
                       PPYM-RATE / 100
           END-EVALUATE
           IF PPYM-PAID-YTD > 0 AND PPYM-PAID-YTD < 10.00
               MOVE 10.00 TO PPYM-PAID-YTD
           END-IF
           IF PPYM-PAID-YTD > 100.00
               MOVE 100.00 TO PPYM-PAID-YTD
           END-IF
           ADD PPYM-PAID-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STALE AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3410-ACCRUE-STALE.
           IF PPYM-STALE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPYM-STALE-AMT * PPYM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO PPYM-EXCEPT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VOID YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3420-ACCRUE-VOID.
           IF PPYM-VOID-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPYM-VOID-YTD * PPYM-RATE / 100 / 365
               ADD WS-WORK-AMT TO PPYT-ISSUED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3430-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF PPYM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPYM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO PPYT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT STALE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3440-SPLIT-STALE.
           COMPUTE WS-WORK-AMT = PPYT-STALE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = PPYT-STALE-AMT - (WS-WORK-AMT * 2)
           COMPUTE PPYT-VOID-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3450-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF PPYT-EFF-DATE > 0 AND PPYT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPYT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PPYT-PAID-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PPYT-PAID-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD PPYT-PAID-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PPYT-PAID-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPYT-PAID-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3460-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 0.00
               MOVE 0.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 5000.00
               MOVE 5000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3470-DESCRIBE-REASON.
           EVALUATE PPYM-REASON-CD
               WHEN 'B'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'M'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'N'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'U'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'V'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'Z'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE EXCEPT YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3480-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN PPYT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYM-STOP-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = PPYM-EXCEPT-YTD *
                       PPYM-RATE / 200
               WHEN PPYM-REGION-CD = 'A'
                   MOVE 3.00 TO WS-WORK-AMT
               WHEN PPYM-REGION-CD = 'B'
                   MOVE 10.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = PPYM-EXCEPT-YTD *
                       PPYM-RATE / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 1.00
               MOVE 1.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 100.00
               MOVE 100.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT ISSUED AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3490-SPLIT-ISSUED.
           COMPUTE WS-WORK-AMT = PPYM-ISSUED-AMT / 4
           COMPUTE WS-WORK-AMT-2 = PPYM-ISSUED-AMT - (WS-WORK-AMT * 4)
           COMPUTE PPYM-EXCEPT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE STALE YTD BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3500-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPYT-TRAN-CD
                   ADD PPYM-STALE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPYT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PPYM-STALE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * IS -> PD WHEN LAST MAINT DATE IS PAST
      * PD -> EX WHEN LAST MAINT DATE IS PAST
      * EX -> VD WHEN LAST MAINT DATE IS PAST
      * VD -> PR WHEN LAST MAINT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3510-SET-TRAN.
           EVALUATE PPYT-TRAN-CD
               WHEN 'IS'
                   IF PPYM-LAST-MAINT-DATE > 0 AND PPYM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'PD' TO PPYT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PD'
                   IF PPYM-LAST-MAINT-DATE > 0 AND PPYM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'EX' TO PPYT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'EX'
                   IF PPYM-LAST-MAINT-DATE > 0 AND PPYM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'VD' TO PPYT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VD'
                   IF PPYM-LAST-MAINT-DATE > 0 AND PPYM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'PR' TO PPYT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * A -> B WHEN EFF DATE IS PAST
      * B -> D WHEN EFF DATE IS PAST
      * D -> H WHEN EFF DATE IS PAST
      * H -> N WHEN EFF DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3520-SET-REGION.
           EVALUATE PPYM-REGION-CD
               WHEN 'A'
                   IF PPYT-EFF-DATE > 0 AND PPYT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'B' TO PPYM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'B'
                   IF PPYT-EFF-DATE > 0 AND PPYT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO PPYM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF PPYT-EFF-DATE > 0 AND PPYT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO PPYM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF PPYT-EFF-DATE > 0 AND PPYT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'N' TO PPYM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ISSUED AMT IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3530-LIMIT-ISSUED.
           IF PPYM-ISSUED-AMT < 25.00
               MOVE 25.00 TO PPYM-ISSUED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PPYM-ISSUED-AMT > 50000.00
               MOVE 50000.00 TO PPYM-ISSUED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE STALE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAID AMT.
      *----------------------------------------------------------------*
       3540-PRORATE-PAID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPYT-PAID-AMT ROUNDED =
                   PPYM-STALE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PPYT-PAID-AMT
           END-IF
           ADD PPYT-PAID-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ISSUED AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3550-FLAG-ISSUED.
           IF PPYT-ISSUED-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PPYT-ISSUED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN EFFECTIVE DATE IS PAST
      * BAT -> MAL WHEN EFFECTIVE DATE IS PAST
      * MAL -> TEL WHEN EFFECTIVE DATE IS PAST
      * TEL -> ONL WHEN EFFECTIVE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3560-SET-CHANNEL.
           EVALUATE PPYT-CHANNEL-CD
               WHEN 'BRN'
                   IF PPYM-EFFECTIVE-DATE > 0 AND PPYM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO PPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF PPYM-EFFECTIVE-DATE > 0 AND PPYM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO PPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF PPYM-EFFECTIVE-DATE > 0 AND PPYM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO PPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF PPYM-EFFECTIVE-DATE > 0 AND PPYM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO PPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E861 MEMO REQUIRED
      * E496 ISSUED YTD NEGATIVE
      * E807 ISSUED YTD OVER 50000
      * E013 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3570-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPYT-MEMO = SPACES
                   MOVE 'E861' TO WS-REJECT-CODE
               WHEN PPYM-ISSUED-YTD < 0
                   MOVE 'E496' TO WS-REJECT-CODE
               WHEN PPYM-ISSUED-YTD > 50000.00
                   MOVE 'E807' TO WS-REJECT-CODE
               WHEN PPYM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E013' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE VOID YTD BY MATCH RULE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3580-ACCUM-MATCH-RULE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPYM-MATCH-RULE-CD
                   ADD PPYM-VOID-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPYM-MATCH-RULE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PPYM-VOID-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 365 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3590-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF PPYM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPYM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO PPYM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3600-FLAG-EXCEPT.
           IF PPYM-EXCEPT-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PPYM-EXCEPT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAID FEE ON THE STALE AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3610-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN PPYM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO PPYM-PAID-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYT-REVERSAL-FLAG = 'Y'
                   COMPUTE PPYM-PAID-AMT ROUNDED = PPYM-STALE-AMT *
                       PPYM-MARGIN / 200
               WHEN PPYT-TRAN-CD = 'IS'
                   MOVE 15.00 TO PPYM-PAID-AMT
               WHEN PPYT-TRAN-CD = 'PD'
                   MOVE 3.00 TO PPYM-PAID-AMT
               WHEN OTHER
                   COMPUTE PPYM-PAID-AMT ROUNDED = PPYM-STALE-AMT *
                       PPYM-MARGIN / 100
           END-EVALUATE
           IF PPYM-PAID-AMT > 0 AND PPYM-PAID-AMT < 2.00
               MOVE 2.00 TO PPYM-PAID-AMT
           END-IF
           IF PPYM-PAID-AMT > 100.00
               MOVE 100.00 TO PPYM-PAID-AMT
           END-IF
           ADD PPYM-PAID-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * IS -> PD WHEN EFF DATE IS PAST
      * PD -> EX WHEN EFF DATE IS PAST
      * EX -> VD WHEN EFF DATE IS PAST
      * VD -> PR WHEN EFF DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3620-SET-TRAN.
           EVALUATE PPYT-TRAN-CD
               WHEN 'IS'
                   IF PPYT-EFF-DATE > 0 AND PPYT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PD' TO PPYT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PD'
                   IF PPYT-EFF-DATE > 0 AND PPYT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EX' TO PPYT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'EX'
                   IF PPYT-EFF-DATE > 0 AND PPYT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VD' TO PPYT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VD'
                   IF PPYT-EFF-DATE > 0 AND PPYT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PR' TO PPYT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E491 ADDR REQUIRED
      * E700 PAID AMT NEGATIVE
      * E069 PAID AMT OVER 50000
      * E835 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3630-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPYM-ADDR = SPACES
                   MOVE 'E491' TO WS-REJECT-CODE
               WHEN PPYT-PAID-AMT < 0
                   MOVE 'E700' TO WS-REJECT-CODE
               WHEN PPYT-PAID-AMT > 50000.00
                   MOVE 'E069' TO WS-REJECT-CODE
               WHEN PPYM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E835' TO WS-REJECT-CODE
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
       3640-DESCRIBE-REASON.
           EVALUATE PPYM-REASON-CD
               WHEN 'B'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'M'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'N'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'U'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'V'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'Z'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3650-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF PPYM-LAST-MAINT-DATE > 0 AND PPYM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPYM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PPYM-ISSUED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PPYM-ISSUED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD PPYM-ISSUED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD PPYM-ISSUED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPYM-ISSUED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3660-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 10.00
               MOVE 10.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 10000.00
               MOVE 10000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAID AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO EXCEPT AMT.
      *----------------------------------------------------------------*
       3670-PRORATE-EXCEPT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPYM-EXCEPT-AMT ROUNDED =
                   PPYT-PAID-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PPYM-EXCEPT-AMT
           END-IF
           ADD PPYM-EXCEPT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE PAID FEE ON THE WORK AMT 2.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3680-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN PPYM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO PPYT-PAID-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYM-STOP-FLAG = 'Y'
                   COMPUTE PPYT-PAID-AMT ROUNDED = WS-WORK-AMT-2 *
                       PPYM-DISCOUNT / 200
               WHEN PPYM-REASON-CD = 'B'
                   MOVE 15.00 TO PPYT-PAID-AMT
               WHEN PPYM-REASON-CD = 'M'
                   MOVE 5.00 TO PPYT-PAID-AMT
               WHEN OTHER
                   COMPUTE PPYT-PAID-AMT ROUNDED = WS-WORK-AMT-2 *
                       PPYM-DISCOUNT / 100
           END-EVALUATE
           IF PPYT-PAID-AMT > 0 AND PPYT-PAID-AMT < 2.00
               MOVE 2.00 TO PPYT-PAID-AMT
           END-IF
           IF PPYT-PAID-AMT > 25.00
               MOVE 25.00 TO PPYT-PAID-AMT
           END-IF
           ADD PPYT-PAID-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT EXCEPT AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3690-SPLIT-EXCEPT.
           COMPUTE WS-WORK-AMT = PPYM-EXCEPT-AMT / 3
           COMPUTE WS-WORK-AMT-2 = PPYM-EXCEPT-AMT - (WS-WORK-AMT * 3)
           COMPUTE PPYM-VOID-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT STALE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3700-SPLIT-STALE.
           COMPUTE WS-WORK-AMT = PPYT-STALE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = PPYT-STALE-AMT - (WS-WORK-AMT * 4)
           COMPUTE PPYM-EXCEPT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STALE AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3710-FLAG-STALE.
           IF PPYM-STALE-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PPYM-STALE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3720-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF PPYM-CLOSE-DATE > 0 AND PPYM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPYM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3730-FLAG-ITEM.
           IF WS-WORK-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3740-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF PPYM-LAST-PAY-DATE > 0 AND PPYM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPYM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PPYM-STALE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD PPYM-STALE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD PPYM-STALE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PPYM-STALE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPYM-STALE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * IS -> PD WHEN LAST MAINT DATE IS PAST
      * PD -> EX WHEN LAST MAINT DATE IS PAST
      * EX -> VD WHEN LAST MAINT DATE IS PAST
      * VD -> PR WHEN LAST MAINT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3750-SET-TRAN.
           EVALUATE PPYT-TRAN-CD
               WHEN 'IS'
                   IF PPYM-LAST-MAINT-DATE > 0 AND PPYM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'PD' TO PPYT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PD'
                   IF PPYM-LAST-MAINT-DATE > 0 AND PPYM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'EX' TO PPYT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'EX'
                   IF PPYM-LAST-MAINT-DATE > 0 AND PPYM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'VD' TO PPYT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VD'
                   IF PPYM-LAST-MAINT-DATE > 0 AND PPYM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'PR' TO PPYT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN EFFECTIVE DATE IS PAST
      * BAT -> MAL WHEN EFFECTIVE DATE IS PAST
      * MAL -> TEL WHEN EFFECTIVE DATE IS PAST
      * TEL -> ONL WHEN EFFECTIVE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3760-SET-CHANNEL.
           EVALUATE PPYT-CHANNEL-CD
               WHEN 'BRN'
                   IF PPYM-EFFECTIVE-DATE > 0 AND PPYM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO PPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF PPYM-EFFECTIVE-DATE > 0 AND PPYM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO PPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF PPYM-EFFECTIVE-DATE > 0 AND PPYM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO PPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF PPYM-EFFECTIVE-DATE > 0 AND PPYM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO PPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE ISSUED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAID AMT.
      *----------------------------------------------------------------*
       3770-PRORATE-PAID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPYM-PAID-AMT ROUNDED =
                   PPYM-ISSUED-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PPYM-PAID-AMT
           END-IF
           ADD PPYM-PAID-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ISSUED AMT IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3780-LIMIT-ISSUED.
           IF PPYM-ISSUED-AMT < 25.00
               MOVE 25.00 TO PPYM-ISSUED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PPYM-ISSUED-AMT > 10000.00
               MOVE 10000.00 TO PPYM-ISSUED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE VOID YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3790-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN PPYM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYT-REVERSAL-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = PPYM-VOID-YTD *
                       PPYM-RATE / 200
               WHEN PPYM-REGION-CD = 'A'
                   MOVE 3.00 TO WS-WORK-AMT
               WHEN PPYM-REGION-CD = 'B'
                   MOVE 7.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = PPYM-VOID-YTD *
                       PPYM-RATE / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 1.00
               MOVE 1.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 25.00
               MOVE 25.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE ISSUED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3800-PRORATE-ISSUED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPYM-ISSUED-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT ISSUED YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3810-SPLIT-ISSUED.
           COMPUTE WS-WORK-AMT = PPYM-ISSUED-YTD / 4
           COMPUTE WS-WORK-AMT-2 = PPYM-ISSUED-YTD - (WS-WORK-AMT * 4)
           COMPUTE PPYT-STALE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STALE TIER TIER FOR THE STALE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3820-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-STALE-TIER-RATE(1) TO PPYM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF PPYM-STALE-AMT NOT < WS-STALE-TIER-LOW(WS-TX)
                   MOVE WS-STALE-TIER-RATE(WS-TX) TO PPYM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPYM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPYM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3830-COUNT-EXCEPT.
           IF PPYM-EXCEPT-CNT < 9999999
               ADD 1 TO PPYM-EXCEPT-CNT
           END-IF
           IF PPYM-EXCEPT-CNT NOT < 12
               MOVE 'Y' TO PPYM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE EXCEPT GRID TIER FOR THE VOID AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3840-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-EXCEPT-GRID-RATE(1) TO PPYM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF PPYT-VOID-AMT NOT < WS-EXCEPT-GRID-LOW(WS-TX)
                   MOVE WS-EXCEPT-GRID-RATE(WS-TX) TO PPYM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPYM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPYM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3850-DESCRIBE-CYCLE.
           EVALUATE PPYM-CYCLE-CD
               WHEN 'A'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'B'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'C'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'R'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'T'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'Y'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3860-COUNT-PAYMENT.
           IF PPYM-PAYMENT-CNT < 9999999
               ADD 1 TO PPYM-PAYMENT-CNT
           END-IF
           IF PPYM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO PPYM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 180 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3870-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF PPYT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPYT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO PPYM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE MATCH RULE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3880-DESCRIBE-MATCH-RULE.
           EVALUATE PPYM-MATCH-RULE-CD
               WHEN 'A'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'B'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'F'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'M'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'S'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'V'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE VOID YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO VOID AMT.
      *----------------------------------------------------------------*
       3890-PRORATE-VOID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPYT-VOID-AMT ROUNDED =
                   PPYM-VOID-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PPYT-VOID-AMT
           END-IF
           ADD PPYT-VOID-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * EXCEPT AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3900-FLAG-EXCEPT.
           IF PPYM-EXCEPT-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PPYM-EXCEPT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3910-FLAG-EXCEPT.
           IF PPYM-EXCEPT-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PPYM-EXCEPT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3920-ACCRUE-RATE.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * PPYM-RATE / 100 / 365
               ADD WS-WORK-AMT TO PPYM-STALE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3930-DESCRIBE-CYCLE.
           EVALUATE PPYM-CYCLE-CD
               WHEN 'A'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'B'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'C'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'R'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'T'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'Y'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE PAID AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ISSUED AMT.
      *----------------------------------------------------------------*
       3940-PRORATE-ISSUED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPYM-ISSUED-AMT ROUNDED =
                   PPYM-PAID-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PPYM-ISSUED-AMT
           END-IF
           ADD PPYM-ISSUED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3950-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF PPYM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPYM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO PPYM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAID GRID TIER FOR THE EXCEPT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3960-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PAID-GRID-RATE(1) TO PPYM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF PPYM-EXCEPT-YTD NOT < WS-PAID-GRID-LOW(WS-TX)
                   MOVE WS-PAID-GRID-RATE(WS-TX) TO PPYM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPYM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPYM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3970-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF PPYM-EFFECTIVE-DATE > 0 AND PPYM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPYM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PPYM-STALE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD PPYM-STALE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD PPYM-STALE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PPYM-STALE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPYM-STALE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE STALE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PAID AMT.
      *----------------------------------------------------------------*
       3980-PRORATE-PAID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPYT-PAID-AMT ROUNDED =
                   PPYT-STALE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PPYT-PAID-AMT
           END-IF
           ADD PPYT-PAID-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE MATCH RULE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3990-DESCRIBE-MATCH-RULE.
           EVALUATE PPYM-MATCH-RULE-CD
               WHEN 'A'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'B'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'F'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'M'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'S'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'V'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT VOID AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4000-SPLIT-VOID.
           COMPUTE WS-WORK-AMT = PPYT-VOID-AMT / 4
           COMPUTE WS-WORK-AMT-2 = PPYT-VOID-AMT - (WS-WORK-AMT * 4)
           COMPUTE PPYM-EXCEPT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE VOID YTD FEE ON THE VOID AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4010-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN PPYM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO PPYM-VOID-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE PPYM-VOID-YTD ROUNDED = PPYT-VOID-AMT *
                       PPYM-RATE / 200
               WHEN PPYT-TRAN-CD = 'IS'
                   MOVE 10.00 TO PPYM-VOID-YTD
               WHEN PPYT-TRAN-CD = 'PD'
                   MOVE 7.00 TO PPYM-VOID-YTD
               WHEN OTHER
                   COMPUTE PPYM-VOID-YTD ROUNDED = PPYT-VOID-AMT *
                       PPYM-RATE / 100
           END-EVALUATE
           IF PPYM-VOID-YTD > 0 AND PPYM-VOID-YTD < 2.00
               MOVE 2.00 TO PPYM-VOID-YTD
           END-IF
           IF PPYM-VOID-YTD > 75.00
               MOVE 75.00 TO PPYM-VOID-YTD
           END-IF
           ADD PPYM-VOID-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT STALE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4020-SPLIT-STALE.
           COMPUTE WS-WORK-AMT = PPYT-STALE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = PPYT-STALE-AMT - (WS-WORK-AMT * 4)
           COMPUTE PPYT-VOID-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E507 DESC REQUIRED
      * E249 PAID AMT NEGATIVE
      * E564 PAID AMT OVER 1000000
      * E397 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       4030-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPYM-DESC = SPACES
                   MOVE 'E507' TO WS-REJECT-CODE
               WHEN PPYM-PAID-AMT < 0
                   MOVE 'E249' TO WS-REJECT-CODE
               WHEN PPYM-PAID-AMT > 1000000.00
                   MOVE 'E564' TO WS-REJECT-CODE
               WHEN PPYM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E397' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE MATCH RULE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4040-DESCRIBE-MATCH-RULE.
           EVALUATE PPYM-MATCH-RULE-CD
               WHEN 'A'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'B'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'F'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'M'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'S'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'V'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4050-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 25.00
               MOVE 25.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 10000.00
               MOVE 10000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * B -> M WHEN LAST PAY DATE IS PAST
      * M -> N WHEN LAST PAY DATE IS PAST
      * N -> U WHEN LAST PAY DATE IS PAST
      * U -> V WHEN LAST PAY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4060-SET-REASON.
           EVALUATE PPYM-REASON-CD
               WHEN 'B'
                   IF PPYM-LAST-PAY-DATE > 0 AND PPYM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO PPYM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF PPYM-LAST-PAY-DATE > 0 AND PPYM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'N' TO PPYM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'N'
                   IF PPYM-LAST-PAY-DATE > 0 AND PPYM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO PPYM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF PPYM-LAST-PAY-DATE > 0 AND PPYM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'V' TO PPYM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4070-SPLIT-VOID.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 12)
           COMPUTE PPYT-VOID-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STALE YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4080-FLAG-STALE.
           IF PPYM-STALE-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PPYM-STALE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4090-DESCRIBE-TRAN.
           EVALUATE PPYT-TRAN-CD
               WHEN 'IS'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'PD'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'EX'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'VD'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'PR'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE EXCEPT YTD FEE ON THE ISSUED YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4100-ASSESS-STOP.
           EVALUATE TRUE
               WHEN PPYM-STOP-FLAG = 'Y'
                   MOVE ZERO TO PPYM-EXCEPT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYM-OVERRIDE-FLAG = 'Y'
                   COMPUTE PPYM-EXCEPT-YTD ROUNDED = PPYM-ISSUED-YTD *
                       PPYM-DISCOUNT / 200
               WHEN PPYT-CHANNEL-CD = 'BRN'
                   MOVE 15.00 TO PPYM-EXCEPT-YTD
               WHEN PPYT-CHANNEL-CD = 'BAT'
                   MOVE 15.00 TO PPYM-EXCEPT-YTD
               WHEN OTHER
                   COMPUTE PPYM-EXCEPT-YTD ROUNDED = PPYM-ISSUED-YTD *
                       PPYM-DISCOUNT / 100
           END-EVALUATE
           IF PPYM-EXCEPT-YTD > 0 AND PPYM-EXCEPT-YTD < 5.00
               MOVE 5.00 TO PPYM-EXCEPT-YTD
           END-IF
           IF PPYM-EXCEPT-YTD > 35.00
               MOVE 35.00 TO PPYM-EXCEPT-YTD
           END-IF
           ADD PPYM-EXCEPT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 45 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4110-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF PPYM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPYM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO PPYT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VOID AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4120-ACCRUE-VOID.
           IF PPYM-VOID-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPYM-VOID-AMT * PPYM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE STALE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAID AMT.
      *----------------------------------------------------------------*
       4130-PRORATE-PAID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPYM-PAID-AMT ROUNDED =
                   PPYM-STALE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PPYM-PAID-AMT
           END-IF
           ADD PPYM-PAID-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       4140-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF PPYM-OPEN-DATE > 0 AND PPYM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPYM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PPYM-STALE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PPYM-STALE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD PPYM-STALE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD PPYM-STALE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPYM-STALE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4150-DESCRIBE-TRAN.
           EVALUATE PPYT-TRAN-CD
               WHEN 'IS'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'PD'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'EX'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'VD'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'PR'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E657 NAME REQUIRED
      * E845 STALE YTD NEGATIVE
      * E173 STALE YTD OVER 10000
      * E745 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       4160-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPYM-NAME = SPACES
                   MOVE 'E657' TO WS-REJECT-CODE
               WHEN PPYM-STALE-YTD < 0
                   MOVE 'E845' TO WS-REJECT-CODE
               WHEN PPYM-STALE-YTD > 10000.00
                   MOVE 'E173' TO WS-REJECT-CODE
               WHEN PPYM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E745' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE STALE FEE ON THE WORK AMT 2.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4170-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN PPYM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO PPYM-STALE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYM-HOLD-FLAG = 'Y'
                   COMPUTE PPYM-STALE-AMT ROUNDED = WS-WORK-AMT-2 *
                       PPYM-MARGIN / 200
               WHEN PPYM-REASON-CD = 'B'
                   MOVE 10.00 TO PPYM-STALE-AMT
               WHEN PPYM-REASON-CD = 'M'
                   MOVE 5.00 TO PPYM-STALE-AMT
               WHEN OTHER
                   COMPUTE PPYM-STALE-AMT ROUNDED = WS-WORK-AMT-2 *
                       PPYM-MARGIN / 100
           END-EVALUATE
           IF PPYM-STALE-AMT > 0 AND PPYM-STALE-AMT < 1.00
               MOVE 1.00 TO PPYM-STALE-AMT
           END-IF
           IF PPYM-STALE-AMT > 35.00
               MOVE 35.00 TO PPYM-STALE-AMT
           END-IF
           ADD PPYM-STALE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN EFF DATE IS PAST
      * BAT -> MAL WHEN EFF DATE IS PAST
      * MAL -> TEL WHEN EFF DATE IS PAST
      * TEL -> ONL WHEN EFF DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4180-SET-CHANNEL.
           EVALUATE PPYT-CHANNEL-CD
               WHEN 'BRN'
                   IF PPYT-EFF-DATE > 0 AND PPYT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO PPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF PPYT-EFF-DATE > 0 AND PPYT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO PPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF PPYT-EFF-DATE > 0 AND PPYT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO PPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF PPYT-EFF-DATE > 0 AND PPYT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO PPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * B -> M WHEN OPEN DATE IS PAST
      * M -> N WHEN OPEN DATE IS PAST
      * N -> U WHEN OPEN DATE IS PAST
      * U -> V WHEN OPEN DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4190-SET-REASON.
           EVALUATE PPYM-REASON-CD
               WHEN 'B'
                   IF PPYM-OPEN-DATE > 0 AND PPYM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO PPYM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF PPYM-OPEN-DATE > 0 AND PPYM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'N' TO PPYM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'N'
                   IF PPYM-OPEN-DATE > 0 AND PPYM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO PPYM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF PPYM-OPEN-DATE > 0 AND PPYM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'V' TO PPYM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       4200-COUNT-PAYMENT.
           IF PPYM-PAYMENT-CNT < 9999999
               ADD 1 TO PPYM-PAYMENT-CNT
           END-IF
           IF PPYM-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO PPYM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT EXCEPT AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4210-SPLIT-EXCEPT.
           COMPUTE WS-WORK-AMT = PPYM-EXCEPT-AMT / 3
           COMPUTE WS-WORK-AMT-2 = PPYM-EXCEPT-AMT - (WS-WORK-AMT * 3)
           COMPUTE PPYT-STALE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE DECISION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4220-DESCRIBE-DECISION.
           EVALUATE PPYM-DECISION-CD
               WHEN 'AND'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'BER'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'CZW'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'SBV'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ZNG'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'ZPS'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ISSUED AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4230-FLAG-ISSUED.
           IF PPYT-ISSUED-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PPYT-ISSUED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ISSUED AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4240-ACCRUE-ISSUED.
           IF PPYM-ISSUED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPYM-ISSUED-AMT * PPYM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO PPYM-VOID-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STALE TIER TIER FOR THE VOID AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4250-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-STALE-TIER-RATE(1) TO PPYM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF PPYT-VOID-AMT NOT < WS-STALE-TIER-LOW(WS-TX)
                   MOVE WS-STALE-TIER-RATE(WS-TX) TO PPYM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPYM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPYM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STALE AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4260-ACCRUE-STALE.
           IF PPYM-STALE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPYM-STALE-AMT * PPYM-RATE / 100 / 365
               ADD WS-WORK-AMT TO PPYM-EXCEPT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE PPY FIELDS TO PPELG AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7020-CALL-PPELG.
           INITIALIZE PPELG-PARMS
           MOVE PPYM-PAID-AMT
             TO PPELG-PAID-AMT
           MOVE PPYM-PAID-YTD
             TO PPELG-PAID-YTD
           MOVE PPYM-VOID-AMT
             TO PPELG-VOID-AMT
           MOVE PPYM-ISSUED-YTD
             TO PPELG-ISSUED-YTD
           MOVE PPYM-EXCEPT-AMT
             TO PPELG-EXCEPT-AMT
           MOVE PPYM-STALE-AMT
             TO PPELG-STALE-AMT
           MOVE PPYM-NEXT-DUE-DATE
             TO PPELG-NEXT-DUE-DATE
           MOVE PPYM-EFFECTIVE-DATE
             TO PPELG-EFFECTIVE-DATE
           MOVE PPYM-EXPIRY-DATE
             TO PPELG-EXPIRY-DATE
           MOVE PPYM-CLOSE-DATE
             TO PPELG-CLOSE-DATE
           MOVE PPYM-STATUS-CD
             TO PPELG-STATUS-CD
           MOVE PPYM-REASON-CD
             TO PPELG-REASON-CD
           MOVE PPYM-CYCLE-CD
             TO PPELG-CYCLE-CD
           MOVE PPYM-REGION-CD
             TO PPELG-REGION-CD
           MOVE PPYM-HOLD-FLAG
             TO PPELG-HOLD-FLAG
           MOVE PPYM-STOP-FLAG
             TO PPELG-STOP-FLAG
           MOVE PPYM-NOTICE-SENT-FLAG
             TO PPELG-NOTICE-SENT-FLAG
           MOVE PPYM-EXCEPT-CNT
             TO PPELG-EXCEPT-CNT
           MOVE PPYM-CYCLE-CNT
             TO PPELG-CYCLE-CNT
           MOVE PPYM-DISCOUNT
             TO PPELG-DISCOUNT
           MOVE PPYM-MARGIN
             TO PPELG-MARGIN
           MOVE PPYM-REF-NO
             TO PPELG-REF-NO
           MOVE PPYM-CONTACT
             TO PPELG-CONTACT
           CALL 'PPELG' USING PPELG-PARMS
           EVALUATE PPELG-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE PPELG-PAID-YTD
                     TO PPYM-PAID-YTD
                   MOVE PPELG-VOID-AMT
                     TO PPYM-VOID-AMT
                   MOVE PPELG-EXCEPT-AMT
                     TO PPYM-EXCEPT-AMT
                   MOVE PPELG-HOLD-FLAG
                     TO PPYM-HOLD-FLAG
                   MOVE PPELG-NOTICE-SENT-FLAG
                     TO PPYM-NOTICE-SENT-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RELG' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-PPELG' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE PPYT-KEY TO RD-KEY
           MOVE PPYT-TRAN-CD TO RD-CODE
           MOVE PPYT-VOID-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE PPYT-KEY TO RD-KEY
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
           DISPLAY 'PPYCALC READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'PPYCALC' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
