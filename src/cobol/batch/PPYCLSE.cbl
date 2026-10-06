      *================================================================*
      * PROGRAM  : PPYCLSE                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : POSITIVE PAY - CLOSURE PROCESSING. READS THE PPY    *
      *            TRANSACTIONS, UPDATES THE PPY MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. CORPORATE POSITIVE PAY ISSUE    *
      *            FILE MATCHING AND EXCEPTIONS                        *
      * INPUT    : PPYMST (KSDS), PPYTRN, BANKCTL                      *
      * OUTPUT   : PPYOUT, PPYCLRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, PPSTAT                  *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-01-23 BHS  ORIGINAL                                       *
      * 1992-03-22 TWH  VOID STATUS CODES ADDED CHG4457                *
      * 1997-07-13 EWS  STALE ROUNDING - HALF UP INC7538               *
      * 1998-02-08 MKL  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-02-26 MKL  AUDIT FINDING - VOID CONTROL TOTALS INC1023    *
      * 2001-05-14 CRP  STALE EXCEPTIONS TO OPS QUEUE INC4911          *
      * 2001-09-13 SKN  PAID CALCULATION CORRECTED CHG1791             *
      * 2003-04-16 LMB  AUDIT FINDING - VOID CONTROL TOTALS INC7653    *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PPYCLSE.
       AUTHOR. J L MARSH.

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
           SELECT REPORT-FILE ASSIGN TO "PPYCLRPT"
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
                                           VALUE 9.000000.
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
      * WS-STALE-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-STALE-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000551000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000298000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005250000000850500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005750000000039500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005850000000710500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006850000000372000'.
       01  WS-STALE-TIER-TBL REDEFINES WS-STALE-TIER-VALUES.
           05  WS-STALE-TIER-ROW OCCURS 6 TIMES.
               10  WS-STALE-TIER-LOW       PIC 9(09)V99.
               10  WS-STALE-TIER-RATE      PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-PAID-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PAID-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000578000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000772500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015000000000689500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025000000000237000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00027500000000866500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00027550000000810000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00027600000000470500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00030100000000053000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00035100000000615000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00037600000000790000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00042600000000321000'.
       01  WS-PAID-SCHED-TBL REDEFINES WS-PAID-SCHED-VALUES.
           05  WS-PAID-SCHED-ROW OCCURS 11 TIMES.
               10  WS-PAID-SCHED-LOW       PIC 9(09)V99.
               10  WS-PAID-SCHED-RATE      PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-PAID-SCHEDX - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PAID-SCHEDX-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000194500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000823500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010100000000343000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010150000000648000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010650000000344000'.
       01  WS-PAID-SCHEDX-TBL REDEFINES WS-PAID-SCHEDX-VALUES.
           05  WS-PAID-SCHEDX-ROW OCCURS 5 TIMES.
               10  WS-PAID-SCHEDX-LOW      PIC 9(09)V99.
               10  WS-PAID-SCHEDX-RATE     PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-VOID-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-VOID-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000043000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000295000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000300000000169000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000350000000552000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000450000000426500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000950000000201000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000827000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003500000000828500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008500000000710000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00009500000000053500'.
       01  WS-VOID-BAND-TBL REDEFINES WS-VOID-BAND-VALUES.
           05  WS-VOID-BAND-ROW OCCURS 10 TIMES.
               10  WS-VOID-BAND-LOW        PIC 9(09)V99.
               10  WS-VOID-BAND-RATE       PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  PPYCLSE  POSITIVE PAY'.
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
       COPY PPSTATP.

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
               PERFORM 3000-EDIT-OPERATOR
               PERFORM 3010-DESCRIBE-STATUS
               PERFORM 3020-SET-CYCLE
               PERFORM 3030-LIMIT-STALE
               PERFORM 3040-DESCRIBE-REGION
               PERFORM 3050-ASSESS-AUTO-RENEW
               PERFORM 3060-FLAG-PAID
               PERFORM 3070-COUNT-CYCLE
               PERFORM 3080-COUNT-SEQ
               PERFORM 3090-SELECT-DISCOUNT
               PERFORM 3100-COUNT-NOTICE
               PERFORM 3110-DESCRIBE-STATUS
               PERFORM 3120-COUNT-EXCEPT
               PERFORM 3130-ASSESS-AUTO-RENEW
               PERFORM 3140-DESCRIBE-REGION
               PERFORM 3150-DESCRIBE-CHANNEL
               PERFORM 3160-SELECT-MARGIN
               PERFORM 3170-SET-STATUS
               PERFORM 3180-PRORATE-STALE
               PERFORM 3190-LIMIT-STALE
               PERFORM 3200-SPLIT-ISSUED
               PERFORM 3210-SET-REASON
               PERFORM 3220-SET-DECISION
               PERFORM 3230-FLAG-PAID
               PERFORM 3240-CHECK-EFFECTIVE
               PERFORM 3250-DESCRIBE-CHANNEL
               PERFORM 3260-ASSESS-EXEMPT
               PERFORM 3270-SELECT-DISCOUNT
               PERFORM 3280-PRORATE-PAID
               PERFORM 3290-COUNT-SEQ
               PERFORM 3300-SET-CYCLE
               PERFORM 3310-SET-CHANNEL
               PERFORM 3320-EDIT-CONTACT
               PERFORM 3330-ACCRUE-VOID
               PERFORM 3340-CHECK-POST
               PERFORM 3350-ACCUM-STATUS
               PERFORM 3360-PRORATE-ISSUED
               PERFORM 3370-FLAG-ITEM
               PERFORM 3380-ACCUM-CHANNEL
               PERFORM 3390-COUNT-PAYMENT
               PERFORM 3400-AGE-EFF
               PERFORM 3410-FLAG-STALE
               PERFORM 3420-DESCRIBE-MATCH-RULE
               PERFORM 3430-COUNT-SEQ
               PERFORM 3440-CHECK-EFFECTIVE
               PERFORM 3450-COUNT-EXCEPT
               PERFORM 3460-ACCRUE-PAID
               PERFORM 3470-ACCRUE-STALE
               PERFORM 3480-ASSESS-REVERSAL
               PERFORM 3490-SPLIT-STALE
               PERFORM 3500-ACCRUE-STALE
               PERFORM 3510-PRORATE-EXCEPT
               PERFORM 3520-LIMIT-PAID
               PERFORM 3530-ASSESS-AUTO-RENEW
               PERFORM 3540-DESCRIBE-REASON
               PERFORM 3550-DESCRIBE-STATUS
               PERFORM 3560-SELECT-RATE
               PERFORM 3570-SPLIT-PAID
               PERFORM 3580-LIMIT-VOID
               PERFORM 3590-CHECK-NEXT-REVIEW
               PERFORM 3600-PRORATE-STALE
               PERFORM 3610-ASSESS-STOP
               PERFORM 3620-FLAG-PAID
               PERFORM 3630-FLAG-VOID
               PERFORM 3640-LIMIT-ISSUED
               PERFORM 3650-PRORATE-VOID
               PERFORM 3660-SPLIT-STALE
               PERFORM 3670-COUNT-CYCLE
               PERFORM 3680-FLAG-VOID
               PERFORM 3690-EDIT-NAME
               PERFORM 3700-DESCRIBE-CYCLE
               PERFORM 3710-ASSESS-HOLD
               PERFORM 3720-DESCRIBE-DECISION
               PERFORM 3730-ACCUM-MATCH-RULE
               PERFORM 3740-PRORATE-ISSUED
               PERFORM 3750-AGE-POST
               PERFORM 3760-SPLIT-STALE
               PERFORM 3770-DESCRIBE-TRAN
               PERFORM 3780-ACCUM-MATCH-RULE
               PERFORM 3790-CHECK-CLOSE
               PERFORM 3800-COUNT-EXCEPT
               PERFORM 3810-LIMIT-STALE
               PERFORM 3820-COUNT-EXCEPT
               PERFORM 3830-AGE-EFFECTIVE
               PERFORM 3840-EDIT-MEMO
               PERFORM 3850-ACCUM-CHANNEL
               PERFORM 3860-EDIT-MEMO
               PERFORM 3870-CHECK-EFFECTIVE
               PERFORM 3880-SET-DECISION
               PERFORM 3890-DESCRIBE-CHANNEL
               PERFORM 3900-CHECK-EFFECTIVE
               PERFORM 3910-FLAG-EXCEPT
               PERFORM 3920-COUNT-NOTICE
               PERFORM 3930-DESCRIBE-CHANNEL
               PERFORM 3940-ASSESS-STOP
               PERFORM 3950-SET-CYCLE
               PERFORM 3960-SPLIT-STALE
               PERFORM 3970-SET-REGION
               PERFORM 3980-ASSESS-EXEMPT
               PERFORM 3990-CHECK-EFF
               PERFORM 4000-ASSESS-NOTICE-SENT
               PERFORM 4010-ACCUM-REASON
               PERFORM 4020-AGE-EFF
               PERFORM 4030-SPLIT-ISSUED
               PERFORM 4040-EDIT-MEMO
               PERFORM 4050-EDIT-CUST-ID
               PERFORM 4060-PRORATE-STALE
               PERFORM 4070-AGE-LAST-ACT
               PERFORM 7030-CALL-PPSTAT
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
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E164 OPERATOR REQUIRED
      * E162 ISSUED AMT NEGATIVE
      * E274 ISSUED AMT OVER 1000000
      * E678 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3000-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPYT-OPERATOR = SPACES
                   MOVE 'E164' TO WS-REJECT-CODE
               WHEN PPYT-ISSUED-AMT < 0
                   MOVE 'E162' TO WS-REJECT-CODE
               WHEN PPYT-ISSUED-AMT > 1000000.00
                   MOVE 'E274' TO WS-REJECT-CODE
               WHEN PPYM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E678' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3010-DESCRIBE-STATUS.
           EVALUATE PPYM-STATUS-CD
               WHEN 'DR'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'TK'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'WX'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * A -> B WHEN NEXT DUE DATE IS PAST
      * B -> C WHEN NEXT DUE DATE IS PAST
      * C -> R WHEN NEXT DUE DATE IS PAST
      * R -> T WHEN NEXT DUE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3020-SET-CYCLE.
           EVALUATE PPYM-CYCLE-CD
               WHEN 'A'
                   IF PPYM-NEXT-DUE-DATE > 0 AND PPYM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'B' TO PPYM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'B'
                   IF PPYM-NEXT-DUE-DATE > 0 AND PPYM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'C' TO PPYM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'C'
                   IF PPYM-NEXT-DUE-DATE > 0 AND PPYM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'R' TO PPYM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF PPYM-NEXT-DUE-DATE > 0 AND PPYM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO PPYM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STALE YTD IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3030-LIMIT-STALE.
           IF PPYM-STALE-YTD < 10.00
               MOVE 10.00 TO PPYM-STALE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PPYM-STALE-YTD > 2500.00
               MOVE 2500.00 TO PPYM-STALE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3040-DESCRIBE-REGION.
           EVALUATE PPYM-REGION-CD
               WHEN 'A'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'B'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'D'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'H'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'N'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'Y'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE VOID FEE ON THE EXCEPT YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3050-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN PPYM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO PPYM-VOID-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE PPYM-VOID-AMT ROUNDED = PPYM-EXCEPT-YTD *
                       PPYM-MARGIN / 200
               WHEN PPYM-MATCH-RULE-CD = 'A'
                   MOVE 10.00 TO PPYM-VOID-AMT
               WHEN PPYM-MATCH-RULE-CD = 'B'
                   MOVE 10.00 TO PPYM-VOID-AMT
               WHEN OTHER
                   COMPUTE PPYM-VOID-AMT ROUNDED = PPYM-EXCEPT-YTD *
                       PPYM-MARGIN / 100
           END-EVALUATE
           IF PPYM-VOID-AMT > 0 AND PPYM-VOID-AMT < 5.00
               MOVE 5.00 TO PPYM-VOID-AMT
           END-IF
           IF PPYM-VOID-AMT > 75.00
               MOVE 75.00 TO PPYM-VOID-AMT
           END-IF
           ADD PPYM-VOID-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PAID AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3060-FLAG-PAID.
           IF PPYM-PAID-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PPYM-PAID-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3070-COUNT-CYCLE.
           IF PPYM-CYCLE-CNT < 9999999
               ADD 1 TO PPYM-CYCLE-CNT
           END-IF
           IF PPYM-CYCLE-CNT NOT < 12
               MOVE 'Y' TO PPYM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3080-COUNT-SEQ.
           IF PPYT-SEQ < 9999999
               ADD 1 TO PPYT-SEQ
           END-IF
           IF PPYT-SEQ NOT < 3
               MOVE 'Y' TO PPYT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STALE TIER TIER FOR THE STALE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3090-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-STALE-TIER-RATE(1) TO PPYM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF PPYT-STALE-AMT NOT < WS-STALE-TIER-LOW(WS-TX)
                   MOVE WS-STALE-TIER-RATE(WS-TX) TO PPYM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPYM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPYM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3100-COUNT-NOTICE.
           IF PPYM-NOTICE-CNT < 9999999
               ADD 1 TO PPYM-NOTICE-CNT
           END-IF
           IF PPYM-NOTICE-CNT NOT < 10
               MOVE 'Y' TO PPYM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3110-DESCRIBE-STATUS.
           EVALUATE PPYM-STATUS-CD
               WHEN 'DR'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'TK'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'WX'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3120-COUNT-EXCEPT.
           IF PPYM-EXCEPT-CNT < 9999999
               ADD 1 TO PPYM-EXCEPT-CNT
           END-IF
           IF PPYM-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO PPYM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE VOID AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3130-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN PPYM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = PPYM-VOID-AMT *
                       PPYM-MARGIN / 200
               WHEN PPYM-REGION-CD = 'A'
                   MOVE 12.00 TO WS-WORK-AMT
               WHEN PPYM-REGION-CD = 'B'
                   MOVE 12.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = PPYM-VOID-AMT *
                       PPYM-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 2.00
               MOVE 2.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 75.00
               MOVE 75.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3140-DESCRIBE-REGION.
           EVALUATE PPYM-REGION-CD
               WHEN 'A'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'B'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'D'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'H'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'N'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'Y'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3150-DESCRIBE-CHANNEL.
           EVALUATE PPYT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE PAID SCHED TIER FOR THE STALE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3160-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PAID-SCHED-RATE(1) TO PPYM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF PPYM-STALE-AMT NOT < WS-PAID-SCHED-LOW(WS-TX)
                   MOVE WS-PAID-SCHED-RATE(WS-TX) TO PPYM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPYM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPYM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * DR -> TK WHEN EFFECTIVE DATE IS PAST
      * TK -> WX WHEN EFFECTIVE DATE IS PAST
      * WX -> DR WHEN EFFECTIVE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3170-SET-STATUS.
           EVALUATE PPYM-STATUS-CD
               WHEN 'DR'
                   IF PPYM-EFFECTIVE-DATE > 0 AND PPYM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TK' TO PPYM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TK'
                   IF PPYM-EFFECTIVE-DATE > 0 AND PPYM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WX' TO PPYM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WX'
                   IF PPYM-EFFECTIVE-DATE > 0 AND PPYM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DR' TO PPYM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE STALE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO STALE YTD.
      *----------------------------------------------------------------*
       3180-PRORATE-STALE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPYM-STALE-YTD ROUNDED =
                   PPYM-STALE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PPYM-STALE-YTD
           END-IF
           ADD PPYM-STALE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STALE AMT IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3190-LIMIT-STALE.
           IF PPYM-STALE-AMT < 100.00
               MOVE 100.00 TO PPYM-STALE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PPYM-STALE-AMT > 1000.00
               MOVE 1000.00 TO PPYM-STALE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ISSUED YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3200-SPLIT-ISSUED.
           COMPUTE WS-WORK-AMT = PPYM-ISSUED-YTD / 3
           COMPUTE WS-WORK-AMT-2 = PPYM-ISSUED-YTD - (WS-WORK-AMT * 3)
           COMPUTE PPYT-ISSUED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * B -> M WHEN EFFECTIVE DATE IS PAST
      * M -> N WHEN EFFECTIVE DATE IS PAST
      * N -> U WHEN EFFECTIVE DATE IS PAST
      * U -> V WHEN EFFECTIVE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3210-SET-REASON.
           EVALUATE PPYM-REASON-CD
               WHEN 'B'
                   IF PPYM-EFFECTIVE-DATE > 0 AND PPYM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO PPYM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF PPYM-EFFECTIVE-DATE > 0 AND PPYM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'N' TO PPYM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'N'
                   IF PPYM-EFFECTIVE-DATE > 0 AND PPYM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO PPYM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF PPYM-EFFECTIVE-DATE > 0 AND PPYM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'V' TO PPYM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DECISION CD:
      * AND -> BER WHEN POST DATE IS PAST
      * BER -> CZW WHEN POST DATE IS PAST
      * CZW -> SBV WHEN POST DATE IS PAST
      * SBV -> ZNG WHEN POST DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3220-SET-DECISION.
           EVALUATE PPYM-DECISION-CD
               WHEN 'AND'
                   IF PPYT-POST-DATE > 0 AND PPYT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BER' TO PPYM-DECISION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BER'
                   IF PPYT-POST-DATE > 0 AND PPYT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CZW' TO PPYM-DECISION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CZW'
                   IF PPYT-POST-DATE > 0 AND PPYT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SBV' TO PPYM-DECISION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SBV'
                   IF PPYT-POST-DATE > 0 AND PPYT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZNG' TO PPYM-DECISION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAID YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3230-FLAG-PAID.
           IF PPYM-PAID-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PPYM-PAID-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3240-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF PPYM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPYM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO PPYM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3250-DESCRIBE-CHANNEL.
           EVALUATE PPYT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE STALE FEE ON THE ISSUED AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3260-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN PPYM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO PPYM-STALE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYM-STOP-FLAG = 'Y'
                   COMPUTE PPYM-STALE-AMT ROUNDED = PPYM-ISSUED-AMT *
                       PPYM-RATE / 200
               WHEN PPYM-STATUS-CD = 'DR'
                   MOVE 7.00 TO PPYM-STALE-AMT
               WHEN PPYM-STATUS-CD = 'TK'
                   MOVE 20.00 TO PPYM-STALE-AMT
               WHEN OTHER
                   COMPUTE PPYM-STALE-AMT ROUNDED = PPYM-ISSUED-AMT *
                       PPYM-RATE / 100
           END-EVALUATE
           IF PPYM-STALE-AMT > 0 AND PPYM-STALE-AMT < 10.00
               MOVE 10.00 TO PPYM-STALE-AMT
           END-IF
           IF PPYM-STALE-AMT > 250.00
               MOVE 250.00 TO PPYM-STALE-AMT
           END-IF
           ADD PPYM-STALE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE PAID SCHED TIER FOR THE STALE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3270-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-PAID-SCHED-RATE(1) TO PPYM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF PPYM-STALE-AMT NOT < WS-PAID-SCHED-LOW(WS-TX)
                   MOVE WS-PAID-SCHED-RATE(WS-TX) TO PPYM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPYM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPYM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PAID AMT.
      *----------------------------------------------------------------*
       3280-PRORATE-PAID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPYT-PAID-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PPYT-PAID-AMT
           END-IF
           ADD PPYT-PAID-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3290-COUNT-SEQ.
           IF PPYT-SEQ < 9999999
               ADD 1 TO PPYT-SEQ
           END-IF
           IF PPYT-SEQ NOT < 3
               MOVE 'Y' TO PPYM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * A -> B WHEN EXPIRY DATE IS PAST
      * B -> C WHEN EXPIRY DATE IS PAST
      * C -> R WHEN EXPIRY DATE IS PAST
      * R -> T WHEN EXPIRY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3300-SET-CYCLE.
           EVALUATE PPYM-CYCLE-CD
               WHEN 'A'
                   IF PPYM-EXPIRY-DATE > 0 AND PPYM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'B' TO PPYM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'B'
                   IF PPYM-EXPIRY-DATE > 0 AND PPYM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'C' TO PPYM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'C'
                   IF PPYM-EXPIRY-DATE > 0 AND PPYM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'R' TO PPYM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF PPYM-EXPIRY-DATE > 0 AND PPYM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO PPYM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN EXPIRY DATE IS PAST
      * BAT -> MAL WHEN EXPIRY DATE IS PAST
      * MAL -> TEL WHEN EXPIRY DATE IS PAST
      * TEL -> ONL WHEN EXPIRY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3310-SET-CHANNEL.
           EVALUATE PPYT-CHANNEL-CD
               WHEN 'BRN'
                   IF PPYM-EXPIRY-DATE > 0 AND PPYM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO PPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF PPYM-EXPIRY-DATE > 0 AND PPYM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO PPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF PPYM-EXPIRY-DATE > 0 AND PPYM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO PPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF PPYM-EXPIRY-DATE > 0 AND PPYM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO PPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E401 CONTACT REQUIRED
      * E985 WORK AMT 2 NEGATIVE
      * E437 WORK AMT 2 OVER 5000
      * E186 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3320-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPYM-CONTACT = SPACES
                   MOVE 'E401' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E985' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 5000.00
                   MOVE 'E437' TO WS-REJECT-CODE
               WHEN PPYT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E186' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VOID AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3330-ACCRUE-VOID.
           IF PPYT-VOID-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPYT-VOID-AMT * PPYM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO PPYT-ISSUED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 90 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3340-CHECK-POST.
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
           IF WS-DAYS > 90
               MOVE 'Y' TO PPYM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAID YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3350-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPYM-STATUS-CD
                   ADD PPYM-PAID-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPYM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PPYM-PAID-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE EXCEPT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ISSUED AMT.
      *----------------------------------------------------------------*
       3360-PRORATE-ISSUED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPYT-ISSUED-AMT ROUNDED =
                   PPYM-EXCEPT-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PPYT-ISSUED-AMT
           END-IF
           ADD PPYT-ISSUED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * WORK AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3370-FLAG-ITEM.
           IF WS-WORK-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAID AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3380-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPYT-CHANNEL-CD
                   ADD PPYT-PAID-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPYT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PPYT-PAID-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3390-COUNT-PAYMENT.
           IF PPYM-PAYMENT-CNT < 9999999
               ADD 1 TO PPYM-PAYMENT-CNT
           END-IF
           IF PPYM-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO PPYM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3400-AGE-EFF.
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
                   ADD PPYM-EXCEPT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PPYM-EXCEPT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD PPYM-EXCEPT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD PPYM-EXCEPT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPYM-EXCEPT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STALE YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3410-FLAG-STALE.
           IF PPYM-STALE-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PPYM-STALE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE MATCH RULE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3420-DESCRIBE-MATCH-RULE.
           EVALUATE PPYM-MATCH-RULE-CD
               WHEN 'A'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'B'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'F'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'M'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'S'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'V'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3430-COUNT-SEQ.
           IF PPYT-SEQ < 9999999
               ADD 1 TO PPYT-SEQ
           END-IF
           IF PPYT-SEQ NOT < 12
               MOVE 'Y' TO PPYM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3440-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF PPYM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPYM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO PPYT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3450-COUNT-EXCEPT.
           IF PPYM-EXCEPT-CNT < 9999999
               ADD 1 TO PPYM-EXCEPT-CNT
           END-IF
           IF PPYM-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO PPYM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAID AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3460-ACCRUE-PAID.
           IF PPYT-PAID-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPYT-PAID-AMT * PPYM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO PPYM-EXCEPT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STALE AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3470-ACCRUE-STALE.
           IF PPYT-STALE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPYT-STALE-AMT * PPYM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO PPYM-ISSUED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ISSUED YTD FEE ON THE EXCEPT AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3480-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN PPYT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO PPYM-ISSUED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYM-HOLD-FLAG = 'Y'
                   COMPUTE PPYM-ISSUED-YTD ROUNDED = PPYM-EXCEPT-AMT *
                       PPYM-DISCOUNT / 200
               WHEN PPYM-DECISION-CD = 'AND'
                   MOVE 15.00 TO PPYM-ISSUED-YTD
               WHEN PPYM-DECISION-CD = 'BER'
                   MOVE 3.00 TO PPYM-ISSUED-YTD
               WHEN OTHER
                   COMPUTE PPYM-ISSUED-YTD ROUNDED = PPYM-EXCEPT-AMT *
                       PPYM-DISCOUNT / 100
           END-EVALUATE
           IF PPYM-ISSUED-YTD > 0 AND PPYM-ISSUED-YTD < 1.00
               MOVE 1.00 TO PPYM-ISSUED-YTD
           END-IF
           IF PPYM-ISSUED-YTD > 35.00
               MOVE 35.00 TO PPYM-ISSUED-YTD
           END-IF
           ADD PPYM-ISSUED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT STALE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3490-SPLIT-STALE.
           COMPUTE WS-WORK-AMT = PPYM-STALE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = PPYM-STALE-AMT - (WS-WORK-AMT * 4)
           COMPUTE PPYM-PAID-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STALE AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3500-ACCRUE-STALE.
           IF PPYM-STALE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPYM-STALE-AMT * PPYM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO PPYT-VOID-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE EXCEPT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3510-PRORATE-EXCEPT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPYM-EXCEPT-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PAID AMT IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3520-LIMIT-PAID.
           IF PPYM-PAID-AMT < 50.00
               MOVE 50.00 TO PPYM-PAID-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PPYM-PAID-AMT > 1000.00
               MOVE 1000.00 TO PPYM-PAID-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE EXCEPT YTD FEE ON THE PAID AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3530-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN PPYM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO PPYM-EXCEPT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYM-HOLD-FLAG = 'Y'
                   COMPUTE PPYM-EXCEPT-YTD ROUNDED = PPYT-PAID-AMT *
                       PPYM-DISCOUNT / 200
               WHEN PPYM-REGION-CD = 'A'
                   MOVE 5.00 TO PPYM-EXCEPT-YTD
               WHEN PPYM-REGION-CD = 'B'
                   MOVE 5.00 TO PPYM-EXCEPT-YTD
               WHEN OTHER
                   COMPUTE PPYM-EXCEPT-YTD ROUNDED = PPYT-PAID-AMT *
                       PPYM-DISCOUNT / 100
           END-EVALUATE
           IF PPYM-EXCEPT-YTD > 0 AND PPYM-EXCEPT-YTD < 5.00
               MOVE 5.00 TO PPYM-EXCEPT-YTD
           END-IF
           IF PPYM-EXCEPT-YTD > 25.00
               MOVE 25.00 TO PPYM-EXCEPT-YTD
           END-IF
           ADD PPYM-EXCEPT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3540-DESCRIBE-REASON.
           EVALUATE PPYM-REASON-CD
               WHEN 'B'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'M'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'N'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'U'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'V'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'Z'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3550-DESCRIBE-STATUS.
           EVALUATE PPYM-STATUS-CD
               WHEN 'DR'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'TK'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'WX'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE PAID SCHED TIER FOR THE PAID AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3560-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PAID-SCHED-RATE(1) TO PPYM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF PPYT-PAID-AMT NOT < WS-PAID-SCHED-LOW(WS-TX)
                   MOVE WS-PAID-SCHED-RATE(WS-TX) TO PPYM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPYM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPYM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PAID AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3570-SPLIT-PAID.
           COMPUTE WS-WORK-AMT = PPYM-PAID-AMT / 4
           COMPUTE WS-WORK-AMT-2 = PPYM-PAID-AMT - (WS-WORK-AMT * 4)
           COMPUTE PPYM-STALE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * VOID AMT IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3580-LIMIT-VOID.
           IF PPYM-VOID-AMT < 25.00
               MOVE 25.00 TO PPYM-VOID-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PPYM-VOID-AMT > 500.00
               MOVE 500.00 TO PPYM-VOID-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 30 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3590-CHECK-NEXT-REVIEW.
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
           IF WS-DAYS > 30
               MOVE 'Y' TO PPYM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAID YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO STALE YTD.
      *----------------------------------------------------------------*
       3600-PRORATE-STALE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPYM-STALE-YTD ROUNDED =
                   PPYM-PAID-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PPYM-STALE-YTD
           END-IF
           ADD PPYM-STALE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE STALE FEE ON THE VOID AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3610-ASSESS-STOP.
           EVALUATE TRUE
               WHEN PPYM-STOP-FLAG = 'Y'
                   MOVE ZERO TO PPYT-STALE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYM-HOLD-FLAG = 'Y'
                   COMPUTE PPYT-STALE-AMT ROUNDED = PPYT-VOID-AMT *
                       PPYM-MARGIN / 200
               WHEN PPYM-MATCH-RULE-CD = 'A'
                   MOVE 3.00 TO PPYT-STALE-AMT
               WHEN PPYM-MATCH-RULE-CD = 'B'
                   MOVE 5.00 TO PPYT-STALE-AMT
               WHEN OTHER
                   COMPUTE PPYT-STALE-AMT ROUNDED = PPYT-VOID-AMT *
                       PPYM-MARGIN / 100
           END-EVALUATE
           IF PPYT-STALE-AMT > 0 AND PPYT-STALE-AMT < 1.00
               MOVE 1.00 TO PPYT-STALE-AMT
           END-IF
           IF PPYT-STALE-AMT > 35.00
               MOVE 35.00 TO PPYT-STALE-AMT
           END-IF
           ADD PPYT-STALE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PAID AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3620-FLAG-PAID.
           IF PPYT-PAID-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PPYT-PAID-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * VOID AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3630-FLAG-VOID.
           IF PPYT-VOID-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PPYT-VOID-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ISSUED AMT IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3640-LIMIT-ISSUED.
           IF PPYT-ISSUED-AMT < 0.00
               MOVE 0.00 TO PPYT-ISSUED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PPYT-ISSUED-AMT > 2500.00
               MOVE 2500.00 TO PPYT-ISSUED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ISSUED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO VOID YTD.
      *----------------------------------------------------------------*
       3650-PRORATE-VOID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPYM-VOID-YTD ROUNDED =
                   PPYT-ISSUED-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PPYM-VOID-YTD
           END-IF
           ADD PPYM-VOID-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT STALE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3660-SPLIT-STALE.
           COMPUTE WS-WORK-AMT = PPYT-STALE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = PPYT-STALE-AMT - (WS-WORK-AMT * 4)
           COMPUTE PPYM-ISSUED-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3670-COUNT-CYCLE.
           IF PPYM-CYCLE-CNT < 9999999
               ADD 1 TO PPYM-CYCLE-CNT
           END-IF
           IF PPYM-CYCLE-CNT NOT < 3
               MOVE 'Y' TO PPYT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * VOID YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3680-FLAG-VOID.
           IF PPYM-VOID-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PPYM-VOID-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E406 NAME REQUIRED
      * E348 STALE YTD NEGATIVE
      * E684 STALE YTD OVER 25000
      * E722 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3690-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPYM-NAME = SPACES
                   MOVE 'E406' TO WS-REJECT-CODE
               WHEN PPYM-STALE-YTD < 0
                   MOVE 'E348' TO WS-REJECT-CODE
               WHEN PPYM-STALE-YTD > 25000.00
                   MOVE 'E684' TO WS-REJECT-CODE
               WHEN PPYM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E722' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3700-DESCRIBE-CYCLE.
           EVALUATE PPYM-CYCLE-CD
               WHEN 'A'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'B'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'C'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'R'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'T'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'Y'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE PAID FEE ON THE ISSUED AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3710-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN PPYM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO PPYT-PAID-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYT-REVERSAL-FLAG = 'Y'
                   COMPUTE PPYT-PAID-AMT ROUNDED = PPYT-ISSUED-AMT *
                       PPYM-RATE / 200
               WHEN PPYM-REASON-CD = 'B'
                   MOVE 3.00 TO PPYT-PAID-AMT
               WHEN PPYM-REASON-CD = 'M'
                   MOVE 3.00 TO PPYT-PAID-AMT
               WHEN OTHER
                   COMPUTE PPYT-PAID-AMT ROUNDED = PPYT-ISSUED-AMT *
                       PPYM-RATE / 100
           END-EVALUATE
           IF PPYT-PAID-AMT > 0 AND PPYT-PAID-AMT < 2.00
               MOVE 2.00 TO PPYT-PAID-AMT
           END-IF
           IF PPYT-PAID-AMT > 150.00
               MOVE 150.00 TO PPYT-PAID-AMT
           END-IF
           ADD PPYT-PAID-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE DECISION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3720-DESCRIBE-DECISION.
           EVALUATE PPYM-DECISION-CD
               WHEN 'AND'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'BER'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'CZW'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'SBV'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ZNG'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'ZPS'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE VOID AMT BY MATCH RULE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3730-ACCUM-MATCH-RULE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPYM-MATCH-RULE-CD
                   ADD PPYT-VOID-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPYM-MATCH-RULE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PPYT-VOID-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE VOID AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ISSUED AMT.
      *----------------------------------------------------------------*
       3740-PRORATE-ISSUED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPYM-ISSUED-AMT ROUNDED =
                   PPYT-VOID-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PPYM-ISSUED-AMT
           END-IF
           ADD PPYM-ISSUED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3750-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF PPYT-POST-DATE > 0 AND PPYT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPYT-POST-DATE TO DTU-DATE-1
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
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT STALE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3760-SPLIT-STALE.
           COMPUTE WS-WORK-AMT = PPYM-STALE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = PPYM-STALE-AMT - (WS-WORK-AMT * 12)
           COMPUTE PPYM-ISSUED-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3770-DESCRIBE-TRAN.
           EVALUATE PPYT-TRAN-CD
               WHEN 'IS'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'PD'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'EX'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'VD'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'PR'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY MATCH RULE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3780-ACCUM-MATCH-RULE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPYM-MATCH-RULE-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPYM-MATCH-RULE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3790-CHECK-CLOSE.
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
           IF WS-DAYS > 365
               MOVE 'Y' TO PPYM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3800-COUNT-EXCEPT.
           IF PPYM-EXCEPT-CNT < 9999999
               ADD 1 TO PPYM-EXCEPT-CNT
           END-IF
           IF PPYM-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO PPYM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STALE AMT IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3810-LIMIT-STALE.
           IF PPYM-STALE-AMT < 0.00
               MOVE 0.00 TO PPYM-STALE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PPYM-STALE-AMT > 2500.00
               MOVE 2500.00 TO PPYM-STALE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3820-COUNT-EXCEPT.
           IF PPYM-EXCEPT-CNT < 9999999
               ADD 1 TO PPYM-EXCEPT-CNT
           END-IF
           IF PPYM-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO PPYM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3830-AGE-EFFECTIVE.
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
                   ADD PPYM-EXCEPT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PPYM-EXCEPT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD PPYM-EXCEPT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PPYM-EXCEPT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPYM-EXCEPT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E990 MEMO REQUIRED
      * E734 STALE YTD NEGATIVE
      * E906 STALE YTD OVER 1000000
      * E734 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3840-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPYT-MEMO = SPACES
                   MOVE 'E990' TO WS-REJECT-CODE
               WHEN PPYM-STALE-YTD < 0
                   MOVE 'E734' TO WS-REJECT-CODE
               WHEN PPYM-STALE-YTD > 1000000.00
                   MOVE 'E906' TO WS-REJECT-CODE
               WHEN PPYT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E734' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE STALE AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3850-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPYT-CHANNEL-CD
                   ADD PPYT-STALE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPYT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PPYT-STALE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E842 MEMO REQUIRED
      * E046 STALE AMT NEGATIVE
      * E224 STALE AMT OVER 50000
      * E465 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3860-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPYT-MEMO = SPACES
                   MOVE 'E842' TO WS-REJECT-CODE
               WHEN PPYT-STALE-AMT < 0
                   MOVE 'E046' TO WS-REJECT-CODE
               WHEN PPYT-STALE-AMT > 50000.00
                   MOVE 'E224' TO WS-REJECT-CODE
               WHEN PPYM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E465' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3870-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF PPYM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPYM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO PPYM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DECISION CD:
      * AND -> BER WHEN LAST PAY DATE IS PAST
      * BER -> CZW WHEN LAST PAY DATE IS PAST
      * CZW -> SBV WHEN LAST PAY DATE IS PAST
      * SBV -> ZNG WHEN LAST PAY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3880-SET-DECISION.
           EVALUATE PPYM-DECISION-CD
               WHEN 'AND'
                   IF PPYM-LAST-PAY-DATE > 0 AND PPYM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BER' TO PPYM-DECISION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BER'
                   IF PPYM-LAST-PAY-DATE > 0 AND PPYM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CZW' TO PPYM-DECISION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CZW'
                   IF PPYM-LAST-PAY-DATE > 0 AND PPYM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SBV' TO PPYM-DECISION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SBV'
                   IF PPYM-LAST-PAY-DATE > 0 AND PPYM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZNG' TO PPYM-DECISION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3890-DESCRIBE-CHANNEL.
           EVALUATE PPYT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3900-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF PPYM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPYM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO PPYT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3910-FLAG-EXCEPT.
           IF PPYM-EXCEPT-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PPYM-EXCEPT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3920-COUNT-NOTICE.
           IF PPYM-NOTICE-CNT < 9999999
               ADD 1 TO PPYM-NOTICE-CNT
           END-IF
           IF PPYM-NOTICE-CNT NOT < 3
               MOVE 'Y' TO PPYM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3930-DESCRIBE-CHANNEL.
           EVALUATE PPYT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE VOID FEE ON THE EXCEPT AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3940-ASSESS-STOP.
           EVALUATE TRUE
               WHEN PPYM-STOP-FLAG = 'Y'
                   MOVE ZERO TO PPYT-VOID-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYT-REVERSAL-FLAG = 'Y'
                   COMPUTE PPYT-VOID-AMT ROUNDED = PPYM-EXCEPT-AMT *
                       PPYM-DISCOUNT / 200
               WHEN PPYT-TRAN-CD = 'IS'
                   MOVE 15.00 TO PPYT-VOID-AMT
               WHEN PPYT-TRAN-CD = 'PD'
                   MOVE 7.00 TO PPYT-VOID-AMT
               WHEN OTHER
                   COMPUTE PPYT-VOID-AMT ROUNDED = PPYM-EXCEPT-AMT *
                       PPYM-DISCOUNT / 100
           END-EVALUATE
           IF PPYT-VOID-AMT > 0 AND PPYT-VOID-AMT < 1.00
               MOVE 1.00 TO PPYT-VOID-AMT
           END-IF
           IF PPYT-VOID-AMT > 35.00
               MOVE 35.00 TO PPYT-VOID-AMT
           END-IF
           ADD PPYT-VOID-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * A -> B WHEN EFF DATE IS PAST
      * B -> C WHEN EFF DATE IS PAST
      * C -> R WHEN EFF DATE IS PAST
      * R -> T WHEN EFF DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3950-SET-CYCLE.
           EVALUATE PPYM-CYCLE-CD
               WHEN 'A'
                   IF PPYT-EFF-DATE > 0 AND PPYT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'B' TO PPYM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'B'
                   IF PPYT-EFF-DATE > 0 AND PPYT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'C' TO PPYM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'C'
                   IF PPYT-EFF-DATE > 0 AND PPYT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'R' TO PPYM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF PPYT-EFF-DATE > 0 AND PPYT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO PPYM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3960-SPLIT-STALE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 12)
           COMPUTE PPYM-STALE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * A -> B WHEN LAST MAINT DATE IS PAST
      * B -> D WHEN LAST MAINT DATE IS PAST
      * D -> H WHEN LAST MAINT DATE IS PAST
      * H -> N WHEN LAST MAINT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3970-SET-REGION.
           EVALUATE PPYM-REGION-CD
               WHEN 'A'
                   IF PPYM-LAST-MAINT-DATE > 0 AND PPYM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'B' TO PPYM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'B'
                   IF PPYM-LAST-MAINT-DATE > 0 AND PPYM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'D' TO PPYM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF PPYM-LAST-MAINT-DATE > 0 AND PPYM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'H' TO PPYM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF PPYM-LAST-MAINT-DATE > 0 AND PPYM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'N' TO PPYM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE PAID YTD FEE ON THE VOID YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3980-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN PPYM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO PPYM-PAID-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYT-REVERSAL-FLAG = 'Y'
                   COMPUTE PPYM-PAID-YTD ROUNDED = PPYM-VOID-YTD *
                       PPYM-MARGIN / 200
               WHEN PPYM-REGION-CD = 'A'
                   MOVE 3.00 TO PPYM-PAID-YTD
               WHEN PPYM-REGION-CD = 'B'
                   MOVE 7.00 TO PPYM-PAID-YTD
               WHEN OTHER
                   COMPUTE PPYM-PAID-YTD ROUNDED = PPYM-VOID-YTD *
                       PPYM-MARGIN / 100
           END-EVALUATE
           IF PPYM-PAID-YTD > 0 AND PPYM-PAID-YTD < 10.00
               MOVE 10.00 TO PPYM-PAID-YTD
           END-IF
           IF PPYM-PAID-YTD > 35.00
               MOVE 35.00 TO PPYM-PAID-YTD
           END-IF
           ADD PPYM-PAID-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 30 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3990-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF PPYT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPYT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO PPYM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE ISSUED AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       4000-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN PPYM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYM-EXEMPT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = PPYT-ISSUED-AMT *
                       PPYM-MARGIN / 200
               WHEN PPYM-REGION-CD = 'A'
                   MOVE 15.00 TO WS-WORK-AMT-2
               WHEN PPYM-REGION-CD = 'B'
                   MOVE 10.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = PPYT-ISSUED-AMT *
                       PPYM-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 1.00
               MOVE 1.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 250.00
               MOVE 250.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE VOID YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4010-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPYM-REASON-CD
                   ADD PPYM-VOID-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPYM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PPYM-VOID-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4020-AGE-EFF.
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
                   ADD PPYM-STALE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD PPYM-STALE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD PPYM-STALE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PPYM-STALE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPYM-STALE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT ISSUED AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4030-SPLIT-ISSUED.
           COMPUTE WS-WORK-AMT = PPYM-ISSUED-AMT / 2
           COMPUTE WS-WORK-AMT-2 = PPYM-ISSUED-AMT - (WS-WORK-AMT * 2)
           COMPUTE PPYM-EXCEPT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E845 MEMO REQUIRED
      * E388 ISSUED AMT NEGATIVE
      * E197 ISSUED AMT OVER 25000
      * E835 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       4040-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPYT-MEMO = SPACES
                   MOVE 'E845' TO WS-REJECT-CODE
               WHEN PPYM-ISSUED-AMT < 0
                   MOVE 'E388' TO WS-REJECT-CODE
               WHEN PPYM-ISSUED-AMT > 25000.00
                   MOVE 'E197' TO WS-REJECT-CODE
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
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E925 CUST ID REQUIRED
      * E872 ISSUED YTD NEGATIVE
      * E370 ISSUED YTD OVER 10000
      * E742 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       4050-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPYM-CUST-ID = SPACES
                   MOVE 'E925' TO WS-REJECT-CODE
               WHEN PPYM-ISSUED-YTD < 0
                   MOVE 'E872' TO WS-REJECT-CODE
               WHEN PPYM-ISSUED-YTD > 10000.00
                   MOVE 'E370' TO WS-REJECT-CODE
               WHEN PPYM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E742' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE EXCEPT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO STALE AMT.
      *----------------------------------------------------------------*
       4060-PRORATE-STALE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPYM-STALE-AMT ROUNDED =
                   PPYM-EXCEPT-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PPYM-STALE-AMT
           END-IF
           ADD PPYM-STALE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4070-AGE-LAST-ACT.
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
                   ADD PPYM-VOID-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD PPYM-VOID-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD PPYM-VOID-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PPYM-VOID-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPYM-VOID-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE PPY FIELDS TO PPSTAT AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7030-CALL-PPSTAT.
           INITIALIZE PPSTAT-PARMS
           MOVE PPYM-EXCEPT-YTD
             TO PPSTAT-EXCEPT-YTD
           MOVE PPYM-VOID-AMT
             TO PPSTAT-VOID-AMT
           MOVE PPYM-PAID-YTD
             TO PPSTAT-PAID-YTD
           MOVE PPYM-EXCEPT-AMT
             TO PPSTAT-EXCEPT-AMT
           MOVE PPYM-PAID-AMT
             TO PPSTAT-PAID-AMT
           MOVE PPYM-STALE-YTD
             TO PPSTAT-STALE-YTD
           MOVE PPYM-CLOSE-DATE
             TO PPSTAT-CLOSE-DATE
           MOVE PPYM-EFFECTIVE-DATE
             TO PPSTAT-EFFECTIVE-DATE
           MOVE PPYM-LAST-PAY-DATE
             TO PPSTAT-LAST-PAY-DATE
           MOVE PPYM-EXPIRY-DATE
             TO PPSTAT-EXPIRY-DATE
           MOVE PPYM-REGION-CD
             TO PPSTAT-REGION-CD
           MOVE PPYM-REASON-CD
             TO PPSTAT-REASON-CD
           MOVE PPYM-MATCH-RULE-CD
             TO PPSTAT-MATCH-RULE-CD
           MOVE PPYM-CYCLE-CD
             TO PPSTAT-CYCLE-CD
           MOVE PPYM-OVERRIDE-FLAG
             TO PPSTAT-OVERRIDE-FLAG
           MOVE PPYM-EXEMPT-FLAG
             TO PPSTAT-EXEMPT-FLAG
           MOVE PPYM-HOLD-FLAG
             TO PPSTAT-HOLD-FLAG
           MOVE PPYM-NOTICE-CNT
             TO PPSTAT-NOTICE-CNT
           MOVE PPYM-PAYMENT-CNT
             TO PPSTAT-PAYMENT-CNT
           MOVE PPYM-DISCOUNT
             TO PPSTAT-DISCOUNT
           MOVE PPYM-MARGIN
             TO PPSTAT-MARGIN
           MOVE PPYM-CONTACT
             TO PPSTAT-CONTACT
           MOVE PPYM-REF-NO
             TO PPSTAT-REF-NO
           CALL 'PPSTAT' USING PPSTAT-PARMS
           EVALUATE PPSTAT-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE PPSTAT-EXCEPT-YTD
                     TO PPYM-EXCEPT-YTD
                   MOVE PPSTAT-VOID-AMT
                     TO PPYM-VOID-AMT
                   MOVE PPSTAT-PAID-YTD
                     TO PPYM-PAID-YTD
                   MOVE PPSTAT-EXCEPT-AMT
                     TO PPYM-EXCEPT-AMT
                   CONTINUE
               WHEN 08
                   MOVE 'RTAT' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7030-CALL-PPSTAT' TO ABND-PARAGRAPH
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
           DISPLAY 'PPYCLSE READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'PPYCLSE' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
