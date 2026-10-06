      *================================================================*
      * PROGRAM  : PPYRNEW                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : POSITIVE PAY - RENEWAL. READS THE PPY TRANSACTIONS, *
      *            UPDATES THE PPY MASTER AND PRODUCES THE CONTROL     *
      *            REPORT. CORPORATE POSITIVE PAY ISSUE FILE MATCHING  *
      *            AND EXCEPTIONS                                      *
      * INPUT    : PPYMST (KSDS), PPYTRN, BANKCTL                      *
      * OUTPUT   : PPYOUT, PPYRNRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, PPSTAT                  *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-01-14 BHS  ORIGINAL                                       *
      * 1994-10-08 PQR  PAID EXCEPTIONS TO OPS QUEUE REQ5181           *
      * 1995-10-21 TWH  STALE STATUS CODES ADDED CHG5285               *
      * 1999-06-09 RJT  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2003-08-18 PQR  VOID TABLE EXPANDED REQ1418                    *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PPYRNEW.
       AUTHOR. B H SANDERS.

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
           SELECT REPORT-FILE ASSIGN TO "PPYRNRPT"
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
                                           VALUE 18.000000.
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
      * WS-ISSUED-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-ISSUED-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000153500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000654500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020000000000735500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021000000000608000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021250000000071000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021300000000190000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021550000000229000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022050000000421000'.
       01  WS-ISSUED-SCHED-TBL REDEFINES WS-ISSUED-SCHED-VALUES.
           05  WS-ISSUED-SCHED-ROW OCCURS 8 TIMES.
               10  WS-ISSUED-SCHED-LOW     PIC 9(09)V99.
               10  WS-ISSUED-SCHED-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-EXCEPT-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-EXCEPT-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000541500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000794000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000750000000563000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000052500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001050000000734500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001150000000063500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001400000000532500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003900000000513000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004400000000469500'.
       01  WS-EXCEPT-GRID-TBL REDEFINES WS-EXCEPT-GRID-VALUES.
           05  WS-EXCEPT-GRID-ROW OCCURS 9 TIMES.
               10  WS-EXCEPT-GRID-LOW      PIC 9(09)V99.
               10  WS-EXCEPT-GRID-RATE     PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-VOID-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-VOID-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000432000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000096000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010100000000399500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015100000000683500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015350000000825500'.
       01  WS-VOID-BAND-TBL REDEFINES WS-VOID-BAND-VALUES.
           05  WS-VOID-BAND-ROW OCCURS 5 TIMES.
               10  WS-VOID-BAND-LOW        PIC 9(09)V99.
               10  WS-VOID-BAND-RATE       PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  PPYRNEW  POSITIVE PAY'.
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
               PERFORM 3000-EDIT-ADDR
               PERFORM 3010-AGE-POST
               PERFORM 3020-COUNT-CYCLE
               PERFORM 3030-PRORATE-ISSUED
               PERFORM 3040-PRORATE-ISSUED
               PERFORM 3050-SET-REGION
               PERFORM 3060-LIMIT-PAID
               PERFORM 3070-ACCUM-STATUS
               PERFORM 3080-ACCRUE-STALE
               PERFORM 3090-DESCRIBE-CYCLE
               PERFORM 3100-DESCRIBE-REGION
               PERFORM 3110-ACCUM-DECISION
               PERFORM 3120-ASSESS-STOP
               PERFORM 3130-ASSESS-STOP
               PERFORM 3140-ASSESS-NOTICE-SENT
               PERFORM 3150-AGE-OPEN
               PERFORM 3160-DESCRIBE-CHANNEL
               PERFORM 3170-AGE-LAST-ACT
               PERFORM 3180-LIMIT-VOID
               PERFORM 3190-ASSESS-NOTICE-SENT
               PERFORM 3200-COUNT-NOTICE
               PERFORM 3210-ASSESS-OVERRIDE
               PERFORM 3220-ASSESS-STOP
               PERFORM 3230-AGE-LAST-PAY
               PERFORM 3240-FLAG-VOID
               PERFORM 3250-ASSESS-OVERRIDE
               PERFORM 3260-COUNT-PAYMENT
               PERFORM 3270-PRORATE-VOID
               PERFORM 3280-AGE-NEXT-DUE
               PERFORM 3290-COUNT-CYCLE
               PERFORM 3300-ASSESS-AUTO-RENEW
               PERFORM 3310-LIMIT-ISSUED
               PERFORM 3320-ACCUM-TRAN
               PERFORM 3330-EDIT-CUST-ID
               PERFORM 3340-SPLIT-PAID
               PERFORM 3350-ASSESS-REVERSAL
               PERFORM 3360-PRORATE-VOID
               PERFORM 3370-EDIT-ADDR
               PERFORM 3380-SET-DECISION
               PERFORM 3390-LIMIT-EXCEPT
               PERFORM 3400-EDIT-REF-NO
               PERFORM 3410-SPLIT-STALE
               PERFORM 3420-FLAG-STALE
               PERFORM 3430-FLAG-PAID
               PERFORM 3440-FLAG-VOID
               PERFORM 3450-AGE-OPEN
               PERFORM 3460-PRORATE-ISSUED
               PERFORM 3470-EDIT-CUST-ID
               PERFORM 3480-AGE-NEXT-DUE
               PERFORM 3490-SET-STATUS
               PERFORM 3500-ACCRUE-EXCEPT
               PERFORM 3510-FLAG-PAID
               PERFORM 3520-ACCUM-MATCH-RULE
               PERFORM 3530-ACCUM-MATCH-RULE
               PERFORM 3540-CHECK-CLOSE
               PERFORM 3550-CHECK-CLOSE
               PERFORM 3560-CHECK-EFF
               PERFORM 3570-SELECT-DISCOUNT
               PERFORM 3580-ASSESS-NOTICE-SENT
               PERFORM 3590-ACCRUE-PAID
               PERFORM 3600-COUNT-SEQ
               PERFORM 3610-CHECK-EFF
               PERFORM 3620-SET-REGION
               PERFORM 3630-EDIT-CONTACT
               PERFORM 3640-PRORATE-ISSUED
               PERFORM 3650-ACCUM-MATCH-RULE
               PERFORM 3660-FLAG-STALE
               PERFORM 3670-SPLIT-ISSUED
               PERFORM 3680-FLAG-VOID
               PERFORM 3690-FLAG-STALE
               PERFORM 3700-SET-REGION
               PERFORM 3710-CHECK-LAST-PAY
               PERFORM 3720-PRORATE-EXCEPT
               PERFORM 3730-SET-TRAN
               PERFORM 3740-ACCRUE-VOID
               PERFORM 3750-EDIT-OPERATOR
               PERFORM 3760-CHECK-OPEN
               PERFORM 3770-ASSESS-OVERRIDE
               PERFORM 3780-ACCUM-STATUS
               PERFORM 3790-SELECT-DISCOUNT
               PERFORM 3800-EDIT-REF-NO
               PERFORM 3810-LIMIT-VOID
               PERFORM 3820-EDIT-MEMO
               PERFORM 3830-LIMIT-ISSUED
               PERFORM 3840-PRORATE-STALE
               PERFORM 3850-AGE-EFF
               PERFORM 3860-EDIT-REJECT
               PERFORM 3870-EDIT-REF-NO
               PERFORM 3880-AGE-LAST-MAINT
               PERFORM 3890-PRORATE-PAID
               PERFORM 3900-SET-TRAN
               PERFORM 3910-EDIT-CONTACT
               PERFORM 3920-LIMIT-STALE
               PERFORM 3930-EDIT-CONTACT
               PERFORM 3940-LIMIT-ITEM
               PERFORM 3950-LIMIT-VOID
               PERFORM 3960-ACCRUE-PAID
               PERFORM 3970-SET-MATCH-RULE
               PERFORM 3980-SPLIT-VOID
               PERFORM 3990-ACCRUE-STALE
               PERFORM 4000-SET-DECISION
               PERFORM 4010-ACCUM-REASON
               PERFORM 4020-PRORATE-VOID
               PERFORM 4030-LIMIT-PAID
               PERFORM 4040-CHECK-CLOSE
               PERFORM 4050-EDIT-REF-NO
               PERFORM 4060-ACCUM-REGION
               PERFORM 4070-FLAG-VOID
               PERFORM 4080-ASSESS-AUTO-RENEW
               PERFORM 4090-DESCRIBE-MATCH-RULE
               PERFORM 4100-ACCUM-REASON
               PERFORM 4110-SPLIT-ISSUED
               PERFORM 4120-ACCUM-REGION
               PERFORM 4130-SET-REGION
               PERFORM 4140-DESCRIBE-MATCH-RULE
               PERFORM 4150-AGE-LAST-MAINT
               PERFORM 4160-ACCRUE-VOID
               PERFORM 4170-COUNT-SEQ
               PERFORM 4180-EDIT-MEMO
               PERFORM 4190-PRORATE-EXCEPT
               PERFORM 4200-FLAG-ISSUED
               PERFORM 4210-CHECK-LAST-ACT
               PERFORM 4220-DESCRIBE-STATUS
               PERFORM 4230-SPLIT-EXCEPT
               PERFORM 4240-EDIT-CONTACT
               PERFORM 4250-EDIT-MEMO
               PERFORM 4260-ACCUM-REGION
               PERFORM 4270-SPLIT-EXCEPT
               PERFORM 4280-SELECT-RATE
               PERFORM 4290-AGE-OPEN
               PERFORM 4300-SELECT-DISCOUNT
               PERFORM 4310-COUNT-EXCEPT
               PERFORM 4320-LIMIT-ISSUED
               PERFORM 4330-FLAG-PAID
               PERFORM 4340-EDIT-ADDR
               PERFORM 4350-CHECK-OPEN
               PERFORM 4360-EDIT-NAME
               PERFORM 4370-FLAG-ISSUED
               PERFORM 4380-COUNT-PAYMENT
               PERFORM 4390-ACCRUE-ISSUED
               PERFORM 4400-ACCRUE-PAID
               PERFORM 4410-PRORATE-EXCEPT
               PERFORM 4420-DESCRIBE-REGION
               PERFORM 4430-FLAG-ITEM
               PERFORM 4440-PRORATE-EXCEPT
               PERFORM 4450-SET-TRAN
               PERFORM 4460-COUNT-CYCLE
               PERFORM 4470-AGE-POST
               PERFORM 4480-SET-CHANNEL
               PERFORM 4490-PRORATE-STALE
               PERFORM 4500-EDIT-ADDR
               PERFORM 4510-ACCRUE-RATE
               PERFORM 4520-CHECK-NEXT-DUE
               PERFORM 4530-ACCUM-DECISION
               PERFORM 4540-SPLIT-EXCEPT
               PERFORM 4550-LIMIT-PAID
               PERFORM 4560-SPLIT-EXCEPT
               PERFORM 4570-SPLIT-EXCEPT
               PERFORM 4580-SELECT-MARGIN
               PERFORM 4590-SPLIT-STALE
               PERFORM 4600-ACCRUE-ISSUED
               PERFORM 4610-SPLIT-PAID
               PERFORM 4620-LIMIT-ITEM
               PERFORM 4630-AGE-EFF
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
      * E095 ADDR REQUIRED
      * E359 ISSUED AMT NEGATIVE
      * E166 ISSUED AMT OVER 50000
      * E527 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3000-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPYM-ADDR = SPACES
                   MOVE 'E095' TO WS-REJECT-CODE
               WHEN PPYM-ISSUED-AMT < 0
                   MOVE 'E359' TO WS-REJECT-CODE
               WHEN PPYM-ISSUED-AMT > 50000.00
                   MOVE 'E166' TO WS-REJECT-CODE
               WHEN PPYM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E527' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3010-AGE-POST.
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
                   ADD PPYM-VOID-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PPYM-VOID-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD PPYM-VOID-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PPYM-VOID-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPYM-VOID-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3020-COUNT-CYCLE.
           IF PPYM-CYCLE-CNT < 9999999
               ADD 1 TO PPYM-CYCLE-CNT
           END-IF
           IF PPYM-CYCLE-CNT NOT < 3
               MOVE 'Y' TO PPYM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ISSUED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ISSUED AMT.
      *----------------------------------------------------------------*
       3030-PRORATE-ISSUED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPYM-ISSUED-AMT ROUNDED =
                   PPYT-ISSUED-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PPYM-ISSUED-AMT
           END-IF
           ADD PPYM-ISSUED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE STALE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ISSUED AMT.
      *----------------------------------------------------------------*
       3040-PRORATE-ISSUED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPYM-ISSUED-AMT ROUNDED =
                   PPYM-STALE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PPYM-ISSUED-AMT
           END-IF
           ADD PPYM-ISSUED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * A -> B WHEN NEXT REVIEW DATE IS PAST
      * B -> D WHEN NEXT REVIEW DATE IS PAST
      * D -> H WHEN NEXT REVIEW DATE IS PAST
      * H -> N WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3050-SET-REGION.
           EVALUATE PPYM-REGION-CD
               WHEN 'A'
                   IF PPYM-NEXT-REVIEW-DATE > 0 AND
                       PPYM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'B' TO PPYM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'B'
                   IF PPYM-NEXT-REVIEW-DATE > 0 AND
                       PPYM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'D' TO PPYM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF PPYM-NEXT-REVIEW-DATE > 0 AND
                       PPYM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'H' TO PPYM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF PPYM-NEXT-REVIEW-DATE > 0 AND
                       PPYM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'N' TO PPYM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAID AMT IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3060-LIMIT-PAID.
           IF PPYT-PAID-AMT < 25.00
               MOVE 25.00 TO PPYT-PAID-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PPYT-PAID-AMT > 1000.00
               MOVE 1000.00 TO PPYT-PAID-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE STALE YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3070-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPYM-STATUS-CD
                   ADD PPYM-STALE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPYM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PPYM-STALE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STALE AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3080-ACCRUE-STALE.
           IF PPYT-STALE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPYT-STALE-AMT * PPYM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO PPYM-PAID-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3090-DESCRIBE-CYCLE.
           EVALUATE PPYM-CYCLE-CD
               WHEN 'A'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'B'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'C'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'R'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'T'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'Y'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3100-DESCRIBE-REGION.
           EVALUATE PPYM-REGION-CD
               WHEN 'A'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'B'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'D'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'H'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'N'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'Y'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE STALE AMT BY DECISION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3110-ACCUM-DECISION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPYM-DECISION-CD
                   ADD PPYT-STALE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPYM-DECISION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PPYT-STALE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE STALE YTD FEE ON THE PAID AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3120-ASSESS-STOP.
           EVALUATE TRUE
               WHEN PPYM-STOP-FLAG = 'Y'
                   MOVE ZERO TO PPYM-STALE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE PPYM-STALE-YTD ROUNDED = PPYT-PAID-AMT *
                       PPYM-RATE / 200
               WHEN PPYM-STATUS-CD = 'DR'
                   MOVE 10.00 TO PPYM-STALE-YTD
               WHEN PPYM-STATUS-CD = 'TK'
                   MOVE 10.00 TO PPYM-STALE-YTD
               WHEN OTHER
                   COMPUTE PPYM-STALE-YTD ROUNDED = PPYT-PAID-AMT *
                       PPYM-RATE / 100
           END-EVALUATE
           IF PPYM-STALE-YTD > 0 AND PPYM-STALE-YTD < 1.00
               MOVE 1.00 TO PPYM-STALE-YTD
           END-IF
           IF PPYM-STALE-YTD > 250.00
               MOVE 250.00 TO PPYM-STALE-YTD
           END-IF
           ADD PPYM-STALE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE EXCEPT FEE ON THE EXCEPT YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3130-ASSESS-STOP.
           EVALUATE TRUE
               WHEN PPYM-STOP-FLAG = 'Y'
                   MOVE ZERO TO PPYM-EXCEPT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYT-REVERSAL-FLAG = 'Y'
                   COMPUTE PPYM-EXCEPT-AMT ROUNDED = PPYM-EXCEPT-YTD *
                       PPYM-DISCOUNT / 200
               WHEN PPYT-CHANNEL-CD = 'BRN'
                   MOVE 20.00 TO PPYM-EXCEPT-AMT
               WHEN PPYT-CHANNEL-CD = 'BAT'
                   MOVE 7.00 TO PPYM-EXCEPT-AMT
               WHEN OTHER
                   COMPUTE PPYM-EXCEPT-AMT ROUNDED = PPYM-EXCEPT-YTD *
                       PPYM-DISCOUNT / 100
           END-EVALUATE
           IF PPYM-EXCEPT-AMT > 0 AND PPYM-EXCEPT-AMT < 1.00
               MOVE 1.00 TO PPYM-EXCEPT-AMT
           END-IF
           IF PPYM-EXCEPT-AMT > 75.00
               MOVE 75.00 TO PPYM-EXCEPT-AMT
           END-IF
           ADD PPYM-EXCEPT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE STALE YTD FEE ON THE ISSUED AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3140-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN PPYM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO PPYM-STALE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYT-REVERSAL-FLAG = 'Y'
                   COMPUTE PPYM-STALE-YTD ROUNDED = PPYT-ISSUED-AMT *
                       PPYM-MARGIN / 200
               WHEN PPYT-CHANNEL-CD = 'BRN'
                   MOVE 7.00 TO PPYM-STALE-YTD
               WHEN PPYT-CHANNEL-CD = 'BAT'
                   MOVE 3.00 TO PPYM-STALE-YTD
               WHEN OTHER
                   COMPUTE PPYM-STALE-YTD ROUNDED = PPYT-ISSUED-AMT *
                       PPYM-MARGIN / 100
           END-EVALUATE
           IF PPYM-STALE-YTD > 0 AND PPYM-STALE-YTD < 2.00
               MOVE 2.00 TO PPYM-STALE-YTD
           END-IF
           IF PPYM-STALE-YTD > 250.00
               MOVE 250.00 TO PPYM-STALE-YTD
           END-IF
           ADD PPYM-STALE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3150-AGE-OPEN.
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
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3160-DESCRIBE-CHANNEL.
           EVALUATE PPYT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3170-AGE-LAST-ACT.
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
                   ADD PPYM-STALE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD PPYM-STALE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD PPYM-STALE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD PPYM-STALE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPYM-STALE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * VOID AMT IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3180-LIMIT-VOID.
           IF PPYT-VOID-AMT < 100.00
               MOVE 100.00 TO PPYT-VOID-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PPYT-VOID-AMT > 500.00
               MOVE 500.00 TO PPYT-VOID-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ISSUED FEE ON THE EXCEPT YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3190-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN PPYM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO PPYT-ISSUED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE PPYT-ISSUED-AMT ROUNDED = PPYM-EXCEPT-YTD *
                       PPYM-RATE / 200
               WHEN PPYM-DECISION-CD = 'AND'
                   MOVE 20.00 TO PPYT-ISSUED-AMT
               WHEN PPYM-DECISION-CD = 'BER'
                   MOVE 12.00 TO PPYT-ISSUED-AMT
               WHEN OTHER
                   COMPUTE PPYT-ISSUED-AMT ROUNDED = PPYM-EXCEPT-YTD *
                       PPYM-RATE / 100
           END-EVALUATE
           IF PPYT-ISSUED-AMT > 0 AND PPYT-ISSUED-AMT < 5.00
               MOVE 5.00 TO PPYT-ISSUED-AMT
           END-IF
           IF PPYT-ISSUED-AMT > 150.00
               MOVE 150.00 TO PPYT-ISSUED-AMT
           END-IF
           ADD PPYT-ISSUED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3200-COUNT-NOTICE.
           IF PPYM-NOTICE-CNT < 9999999
               ADD 1 TO PPYM-NOTICE-CNT
           END-IF
           IF PPYM-NOTICE-CNT NOT < 6
               MOVE 'Y' TO PPYM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE STALE FEE ON THE EXCEPT AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3210-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN PPYM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO PPYM-STALE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE PPYM-STALE-AMT ROUNDED = PPYM-EXCEPT-AMT *
                       PPYM-MARGIN / 200
               WHEN PPYT-CHANNEL-CD = 'BRN'
                   MOVE 10.00 TO PPYM-STALE-AMT
               WHEN PPYT-CHANNEL-CD = 'BAT'
                   MOVE 20.00 TO PPYM-STALE-AMT
               WHEN OTHER
                   COMPUTE PPYM-STALE-AMT ROUNDED = PPYM-EXCEPT-AMT *
                       PPYM-MARGIN / 100
           END-EVALUATE
           IF PPYM-STALE-AMT > 0 AND PPYM-STALE-AMT < 2.00
               MOVE 2.00 TO PPYM-STALE-AMT
           END-IF
           IF PPYM-STALE-AMT > 250.00
               MOVE 250.00 TO PPYM-STALE-AMT
           END-IF
           ADD PPYM-STALE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE PAID FEE ON THE STALE AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3220-ASSESS-STOP.
           EVALUATE TRUE
               WHEN PPYM-STOP-FLAG = 'Y'
                   MOVE ZERO TO PPYM-PAID-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYM-EXEMPT-FLAG = 'Y'
                   COMPUTE PPYM-PAID-AMT ROUNDED = PPYM-STALE-AMT *
                       PPYM-DISCOUNT / 200
               WHEN PPYM-MATCH-RULE-CD = 'A'
                   MOVE 5.00 TO PPYM-PAID-AMT
               WHEN PPYM-MATCH-RULE-CD = 'B'
                   MOVE 10.00 TO PPYM-PAID-AMT
               WHEN OTHER
                   COMPUTE PPYM-PAID-AMT ROUNDED = PPYM-STALE-AMT *
                       PPYM-DISCOUNT / 100
           END-EVALUATE
           IF PPYM-PAID-AMT > 0 AND PPYM-PAID-AMT < 2.00
               MOVE 2.00 TO PPYM-PAID-AMT
           END-IF
           IF PPYM-PAID-AMT > 35.00
               MOVE 35.00 TO PPYM-PAID-AMT
           END-IF
           ADD PPYM-PAID-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3230-AGE-LAST-PAY.
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
                   ADD PPYM-STALE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PPYM-STALE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD PPYM-STALE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PPYM-STALE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPYM-STALE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * VOID AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3240-FLAG-VOID.
           IF PPYM-VOID-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PPYM-VOID-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ISSUED FEE ON THE VOID YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3250-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN PPYM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO PPYT-ISSUED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYM-EXEMPT-FLAG = 'Y'
                   COMPUTE PPYT-ISSUED-AMT ROUNDED = PPYM-VOID-YTD *
                       PPYM-RATE / 200
               WHEN PPYT-CHANNEL-CD = 'BRN'
                   MOVE 10.00 TO PPYT-ISSUED-AMT
               WHEN PPYT-CHANNEL-CD = 'BAT'
                   MOVE 12.00 TO PPYT-ISSUED-AMT
               WHEN OTHER
                   COMPUTE PPYT-ISSUED-AMT ROUNDED = PPYM-VOID-YTD *
                       PPYM-RATE / 100
           END-EVALUATE
           IF PPYT-ISSUED-AMT > 0 AND PPYT-ISSUED-AMT < 10.00
               MOVE 10.00 TO PPYT-ISSUED-AMT
           END-IF
           IF PPYT-ISSUED-AMT > 25.00
               MOVE 25.00 TO PPYT-ISSUED-AMT
           END-IF
           ADD PPYT-ISSUED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3260-COUNT-PAYMENT.
           IF PPYM-PAYMENT-CNT < 9999999
               ADD 1 TO PPYM-PAYMENT-CNT
           END-IF
           IF PPYM-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO PPYM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ISSUED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO VOID AMT.
      *----------------------------------------------------------------*
       3270-PRORATE-VOID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPYM-VOID-AMT ROUNDED =
                   PPYM-ISSUED-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PPYM-VOID-AMT
           END-IF
           ADD PPYM-VOID-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3280-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF PPYM-NEXT-DUE-DATE > 0 AND PPYM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPYM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PPYM-EXCEPT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PPYM-EXCEPT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD PPYM-EXCEPT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PPYM-EXCEPT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPYM-EXCEPT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3290-COUNT-CYCLE.
           IF PPYM-CYCLE-CNT < 9999999
               ADD 1 TO PPYM-CYCLE-CNT
           END-IF
           IF PPYM-CYCLE-CNT NOT < 6
               MOVE 'Y' TO PPYM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAID YTD FEE ON THE ISSUED AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3300-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN PPYM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO PPYM-PAID-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE PPYM-PAID-YTD ROUNDED = PPYM-ISSUED-AMT *
                       PPYM-DISCOUNT / 200
               WHEN PPYM-STATUS-CD = 'DR'
                   MOVE 15.00 TO PPYM-PAID-YTD
               WHEN PPYM-STATUS-CD = 'TK'
                   MOVE 10.00 TO PPYM-PAID-YTD
               WHEN OTHER
                   COMPUTE PPYM-PAID-YTD ROUNDED = PPYM-ISSUED-AMT *
                       PPYM-DISCOUNT / 100
           END-EVALUATE
           IF PPYM-PAID-YTD > 0 AND PPYM-PAID-YTD < 5.00
               MOVE 5.00 TO PPYM-PAID-YTD
           END-IF
           IF PPYM-PAID-YTD > 100.00
               MOVE 100.00 TO PPYM-PAID-YTD
           END-IF
           ADD PPYM-PAID-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ISSUED AMT IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3310-LIMIT-ISSUED.
           IF PPYT-ISSUED-AMT < 50.00
               MOVE 50.00 TO PPYT-ISSUED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PPYT-ISSUED-AMT > 500.00
               MOVE 500.00 TO PPYT-ISSUED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE STALE AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3320-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPYT-TRAN-CD
                   ADD PPYM-STALE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPYT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PPYM-STALE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E464 CUST ID REQUIRED
      * E282 STALE YTD NEGATIVE
      * E200 STALE YTD OVER 1000000
      * E903 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3330-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPYM-CUST-ID = SPACES
                   MOVE 'E464' TO WS-REJECT-CODE
               WHEN PPYM-STALE-YTD < 0
                   MOVE 'E282' TO WS-REJECT-CODE
               WHEN PPYM-STALE-YTD > 1000000.00
                   MOVE 'E200' TO WS-REJECT-CODE
               WHEN PPYT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E903' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PAID YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3340-SPLIT-PAID.
           COMPUTE WS-WORK-AMT = PPYM-PAID-YTD / 2
           COMPUTE WS-WORK-AMT-2 = PPYM-PAID-YTD - (WS-WORK-AMT * 2)
           COMPUTE PPYT-STALE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ISSUED FEE ON THE WORK AMT 2.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3350-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN PPYT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO PPYM-ISSUED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYM-OVERRIDE-FLAG = 'Y'
                   COMPUTE PPYM-ISSUED-AMT ROUNDED = WS-WORK-AMT-2 *
                       PPYM-DISCOUNT / 200
               WHEN PPYT-TRAN-CD = 'IS'
                   MOVE 7.00 TO PPYM-ISSUED-AMT
               WHEN PPYT-TRAN-CD = 'PD'
                   MOVE 20.00 TO PPYM-ISSUED-AMT
               WHEN OTHER
                   COMPUTE PPYM-ISSUED-AMT ROUNDED = WS-WORK-AMT-2 *
                       PPYM-DISCOUNT / 100
           END-EVALUATE
           IF PPYM-ISSUED-AMT > 0 AND PPYM-ISSUED-AMT < 2.00
               MOVE 2.00 TO PPYM-ISSUED-AMT
           END-IF
           IF PPYM-ISSUED-AMT > 75.00
               MOVE 75.00 TO PPYM-ISSUED-AMT
           END-IF
           ADD PPYM-ISSUED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE PAID AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO VOID AMT.
      *----------------------------------------------------------------*
       3360-PRORATE-VOID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPYM-VOID-AMT ROUNDED =
                   PPYM-PAID-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PPYM-VOID-AMT
           END-IF
           ADD PPYM-VOID-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E130 ADDR REQUIRED
      * E585 PAID YTD NEGATIVE
      * E540 PAID YTD OVER 1000000
      * E651 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3370-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPYM-ADDR = SPACES
                   MOVE 'E130' TO WS-REJECT-CODE
               WHEN PPYM-PAID-YTD < 0
                   MOVE 'E585' TO WS-REJECT-CODE
               WHEN PPYM-PAID-YTD > 1000000.00
                   MOVE 'E540' TO WS-REJECT-CODE
               WHEN PPYM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E651' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DECISION CD:
      * AND -> BER WHEN EFF DATE IS PAST
      * BER -> CZW WHEN EFF DATE IS PAST
      * CZW -> SBV WHEN EFF DATE IS PAST
      * SBV -> ZNG WHEN EFF DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3380-SET-DECISION.
           EVALUATE PPYM-DECISION-CD
               WHEN 'AND'
                   IF PPYT-EFF-DATE > 0 AND PPYT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BER' TO PPYM-DECISION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BER'
                   IF PPYT-EFF-DATE > 0 AND PPYT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CZW' TO PPYM-DECISION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CZW'
                   IF PPYT-EFF-DATE > 0 AND PPYT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SBV' TO PPYM-DECISION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SBV'
                   IF PPYT-EFF-DATE > 0 AND PPYT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZNG' TO PPYM-DECISION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT AMT IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3390-LIMIT-EXCEPT.
           IF PPYM-EXCEPT-AMT < 50.00
               MOVE 50.00 TO PPYM-EXCEPT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PPYM-EXCEPT-AMT > 50000.00
               MOVE 50000.00 TO PPYM-EXCEPT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E043 REF NO REQUIRED
      * E081 ISSUED YTD NEGATIVE
      * E253 ISSUED YTD OVER 25000
      * E300 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3400-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPYM-REF-NO = SPACES
                   MOVE 'E043' TO WS-REJECT-CODE
               WHEN PPYM-ISSUED-YTD < 0
                   MOVE 'E081' TO WS-REJECT-CODE
               WHEN PPYM-ISSUED-YTD > 25000.00
                   MOVE 'E253' TO WS-REJECT-CODE
               WHEN PPYM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E300' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3410-SPLIT-STALE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 3)
           COMPUTE PPYT-STALE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STALE AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3420-FLAG-STALE.
           IF PPYM-STALE-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PPYM-STALE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PAID AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3430-FLAG-PAID.
           IF PPYT-PAID-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PPYT-PAID-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * VOID AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3440-FLAG-VOID.
           IF PPYM-VOID-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PPYM-VOID-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-45, 46-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3450-AGE-OPEN.
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
                   ADD PPYM-ISSUED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD PPYM-ISSUED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD PPYM-ISSUED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PPYM-ISSUED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPYM-ISSUED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE ISSUED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3460-PRORATE-ISSUED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   PPYM-ISSUED-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E057 CUST ID REQUIRED
      * E032 STALE YTD NEGATIVE
      * E271 STALE YTD OVER 99999
      * E883 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3470-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPYM-CUST-ID = SPACES
                   MOVE 'E057' TO WS-REJECT-CODE
               WHEN PPYM-STALE-YTD < 0
                   MOVE 'E032' TO WS-REJECT-CODE
               WHEN PPYM-STALE-YTD > 99999.00
                   MOVE 'E271' TO WS-REJECT-CODE
               WHEN PPYM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E883' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3480-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF PPYM-NEXT-DUE-DATE > 0 AND PPYM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPYM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PPYM-ISSUED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD PPYM-ISSUED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD PPYM-ISSUED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PPYM-ISSUED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPYM-ISSUED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * DR -> TK WHEN POST DATE IS PAST
      * TK -> WX WHEN POST DATE IS PAST
      * WX -> DR WHEN POST DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3490-SET-STATUS.
           EVALUATE PPYM-STATUS-CD
               WHEN 'DR'
                   IF PPYT-POST-DATE > 0 AND PPYT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TK' TO PPYM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TK'
                   IF PPYT-POST-DATE > 0 AND PPYT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WX' TO PPYM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WX'
                   IF PPYT-POST-DATE > 0 AND PPYT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DR' TO PPYM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON EXCEPT YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3500-ACCRUE-EXCEPT.
           IF PPYM-EXCEPT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPYM-EXCEPT-YTD * PPYM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO PPYM-PAID-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAID AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3510-FLAG-PAID.
           IF PPYM-PAID-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PPYM-PAID-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAID AMT BY MATCH RULE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3520-ACCUM-MATCH-RULE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPYM-MATCH-RULE-CD
                   ADD PPYM-PAID-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPYM-MATCH-RULE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PPYM-PAID-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE VOID YTD BY MATCH RULE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3530-ACCUM-MATCH-RULE.
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
      * DAYS SINCE CLOSE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3540-CHECK-CLOSE.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO PPYM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3550-CHECK-CLOSE.
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
               MOVE 'Y' TO PPYM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 365 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3560-CHECK-EFF.
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
           IF WS-DAYS > 365
               MOVE 'Y' TO PPYM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ISSUED SCHED TIER FOR THE ISSUED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3570-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-ISSUED-SCHED-RATE(1) TO PPYM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF PPYM-ISSUED-AMT NOT < WS-ISSUED-SCHED-LOW(WS-TX)
                   MOVE WS-ISSUED-SCHED-RATE(WS-TX) TO PPYM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPYM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPYM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE STALE YTD FEE ON THE ISSUED AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3580-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN PPYM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO PPYM-STALE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYT-REVERSAL-FLAG = 'Y'
                   COMPUTE PPYM-STALE-YTD ROUNDED = PPYT-ISSUED-AMT *
                       PPYM-RATE / 200
               WHEN PPYM-STATUS-CD = 'DR'
                   MOVE 12.00 TO PPYM-STALE-YTD
               WHEN PPYM-STATUS-CD = 'TK'
                   MOVE 7.00 TO PPYM-STALE-YTD
               WHEN OTHER
                   COMPUTE PPYM-STALE-YTD ROUNDED = PPYT-ISSUED-AMT *
                       PPYM-RATE / 100
           END-EVALUATE
           IF PPYM-STALE-YTD > 0 AND PPYM-STALE-YTD < 2.00
               MOVE 2.00 TO PPYM-STALE-YTD
           END-IF
           IF PPYM-STALE-YTD > 35.00
               MOVE 35.00 TO PPYM-STALE-YTD
           END-IF
           ADD PPYM-STALE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAID AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3590-ACCRUE-PAID.
           IF PPYM-PAID-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPYM-PAID-AMT * PPYM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO PPYM-ISSUED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3600-COUNT-SEQ.
           IF PPYT-SEQ < 9999999
               ADD 1 TO PPYT-SEQ
           END-IF
           IF PPYT-SEQ NOT < 6
               MOVE 'Y' TO PPYM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 10 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3610-CHECK-EFF.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO PPYM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * A -> B WHEN CLOSE DATE IS PAST
      * B -> D WHEN CLOSE DATE IS PAST
      * D -> H WHEN CLOSE DATE IS PAST
      * H -> N WHEN CLOSE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3620-SET-REGION.
           EVALUATE PPYM-REGION-CD
               WHEN 'A'
                   IF PPYM-CLOSE-DATE > 0 AND PPYM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'B' TO PPYM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'B'
                   IF PPYM-CLOSE-DATE > 0 AND PPYM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO PPYM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF PPYM-CLOSE-DATE > 0 AND PPYM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO PPYM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF PPYM-CLOSE-DATE > 0 AND PPYM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'N' TO PPYM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E911 CONTACT REQUIRED
      * E696 ISSUED AMT NEGATIVE
      * E539 ISSUED AMT OVER 50000
      * E714 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3630-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPYM-CONTACT = SPACES
                   MOVE 'E911' TO WS-REJECT-CODE
               WHEN PPYM-ISSUED-AMT < 0
                   MOVE 'E696' TO WS-REJECT-CODE
               WHEN PPYM-ISSUED-AMT > 50000.00
                   MOVE 'E539' TO WS-REJECT-CODE
               WHEN PPYM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E714' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ISSUED YTD.
      *----------------------------------------------------------------*
       3640-PRORATE-ISSUED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPYM-ISSUED-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PPYM-ISSUED-YTD
           END-IF
           ADD PPYM-ISSUED-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE STALE AMT BY MATCH RULE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3650-ACCUM-MATCH-RULE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPYM-MATCH-RULE-CD
                   ADD PPYM-STALE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPYM-MATCH-RULE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PPYM-STALE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STALE YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3660-FLAG-STALE.
           IF PPYM-STALE-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PPYM-STALE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ISSUED AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3670-SPLIT-ISSUED.
           COMPUTE WS-WORK-AMT = PPYT-ISSUED-AMT / 4
           COMPUTE WS-WORK-AMT-2 = PPYT-ISSUED-AMT - (WS-WORK-AMT * 4)
           COMPUTE PPYT-VOID-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * VOID AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3680-FLAG-VOID.
           IF PPYT-VOID-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PPYT-VOID-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STALE AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3690-FLAG-STALE.
           IF PPYM-STALE-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PPYM-STALE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * A -> B WHEN EXPIRY DATE IS PAST
      * B -> D WHEN EXPIRY DATE IS PAST
      * D -> H WHEN EXPIRY DATE IS PAST
      * H -> N WHEN EXPIRY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3700-SET-REGION.
           EVALUATE PPYM-REGION-CD
               WHEN 'A'
                   IF PPYM-EXPIRY-DATE > 0 AND PPYM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'B' TO PPYM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'B'
                   IF PPYM-EXPIRY-DATE > 0 AND PPYM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO PPYM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF PPYM-EXPIRY-DATE > 0 AND PPYM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO PPYM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF PPYM-EXPIRY-DATE > 0 AND PPYM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'N' TO PPYM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3710-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF PPYM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPYM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO PPYT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ISSUED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO EXCEPT YTD.
      *----------------------------------------------------------------*
       3720-PRORATE-EXCEPT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPYM-EXCEPT-YTD ROUNDED =
                   PPYM-ISSUED-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PPYM-EXCEPT-YTD
           END-IF
           ADD PPYM-EXCEPT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * IS -> PD WHEN NEXT DUE DATE IS PAST
      * PD -> EX WHEN NEXT DUE DATE IS PAST
      * EX -> VD WHEN NEXT DUE DATE IS PAST
      * VD -> PR WHEN NEXT DUE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3730-SET-TRAN.
           EVALUATE PPYT-TRAN-CD
               WHEN 'IS'
                   IF PPYM-NEXT-DUE-DATE > 0 AND PPYM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PD' TO PPYT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PD'
                   IF PPYM-NEXT-DUE-DATE > 0 AND PPYM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EX' TO PPYT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'EX'
                   IF PPYM-NEXT-DUE-DATE > 0 AND PPYM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VD' TO PPYT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VD'
                   IF PPYM-NEXT-DUE-DATE > 0 AND PPYM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PR' TO PPYT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VOID YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3740-ACCRUE-VOID.
           IF PPYM-VOID-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPYM-VOID-YTD * PPYM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO PPYM-ISSUED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E319 OPERATOR REQUIRED
      * E180 EXCEPT AMT NEGATIVE
      * E076 EXCEPT AMT OVER 5000
      * E118 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3750-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPYT-OPERATOR = SPACES
                   MOVE 'E319' TO WS-REJECT-CODE
               WHEN PPYM-EXCEPT-AMT < 0
                   MOVE 'E180' TO WS-REJECT-CODE
               WHEN PPYM-EXCEPT-AMT > 5000.00
                   MOVE 'E076' TO WS-REJECT-CODE
               WHEN PPYM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E118' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 10 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3760-CHECK-OPEN.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO PPYM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE VOID AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3770-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN PPYM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = PPYM-VOID-AMT *
                       PPYM-MARGIN / 200
               WHEN PPYM-REGION-CD = 'A'
                   MOVE 12.00 TO WS-WORK-AMT-2
               WHEN PPYM-REGION-CD = 'B'
                   MOVE 10.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = PPYM-VOID-AMT *
                       PPYM-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 2.00
               MOVE 2.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 25.00
               MOVE 25.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE ISSUED YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3780-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPYM-STATUS-CD
                   ADD PPYM-ISSUED-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPYM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PPYM-ISSUED-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE EXCEPT GRID TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3790-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-EXCEPT-GRID-RATE(1) TO PPYM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF WS-WORK-AMT-2 NOT < WS-EXCEPT-GRID-LOW(WS-TX)
                   MOVE WS-EXCEPT-GRID-RATE(WS-TX) TO PPYM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPYM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPYM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E443 REF NO REQUIRED
      * E600 VOID AMT NEGATIVE
      * E179 VOID AMT OVER 25000
      * E619 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3800-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPYM-REF-NO = SPACES
                   MOVE 'E443' TO WS-REJECT-CODE
               WHEN PPYT-VOID-AMT < 0
                   MOVE 'E600' TO WS-REJECT-CODE
               WHEN PPYT-VOID-AMT > 25000.00
                   MOVE 'E179' TO WS-REJECT-CODE
               WHEN PPYM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E619' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * VOID AMT IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3810-LIMIT-VOID.
           IF PPYM-VOID-AMT < 25.00
               MOVE 25.00 TO PPYM-VOID-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PPYM-VOID-AMT > 5000.00
               MOVE 5000.00 TO PPYM-VOID-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E775 MEMO REQUIRED
      * E267 STALE YTD NEGATIVE
      * E813 STALE YTD OVER 1000000
      * E691 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3820-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPYT-MEMO = SPACES
                   MOVE 'E775' TO WS-REJECT-CODE
               WHEN PPYM-STALE-YTD < 0
                   MOVE 'E267' TO WS-REJECT-CODE
               WHEN PPYM-STALE-YTD > 1000000.00
                   MOVE 'E813' TO WS-REJECT-CODE
               WHEN PPYM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E691' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ISSUED AMT IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3830-LIMIT-ISSUED.
           IF PPYT-ISSUED-AMT < 10.00
               MOVE 10.00 TO PPYT-ISSUED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PPYT-ISSUED-AMT > 5000.00
               MOVE 5000.00 TO PPYT-ISSUED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ISSUED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO STALE YTD.
      *----------------------------------------------------------------*
       3840-PRORATE-STALE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPYM-STALE-YTD ROUNDED =
                   PPYM-ISSUED-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PPYM-STALE-YTD
           END-IF
           ADD PPYM-STALE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3850-AGE-EFF.
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
                   ADD PPYM-ISSUED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PPYM-ISSUED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD PPYM-ISSUED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD PPYM-ISSUED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPYM-ISSUED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E467 REJECT CD REQUIRED
      * E637 ISSUED AMT NEGATIVE
      * E799 ISSUED AMT OVER 25000
      * E229 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3860-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPYT-REJECT-CD = SPACES
                   MOVE 'E467' TO WS-REJECT-CODE
               WHEN PPYM-ISSUED-AMT < 0
                   MOVE 'E637' TO WS-REJECT-CODE
               WHEN PPYM-ISSUED-AMT > 25000.00
                   MOVE 'E799' TO WS-REJECT-CODE
               WHEN PPYT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E229' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E481 REF NO REQUIRED
      * E474 ISSUED YTD NEGATIVE
      * E020 ISSUED YTD OVER 1000000
      * E211 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3870-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPYM-REF-NO = SPACES
                   MOVE 'E481' TO WS-REJECT-CODE
               WHEN PPYM-ISSUED-YTD < 0
                   MOVE 'E474' TO WS-REJECT-CODE
               WHEN PPYM-ISSUED-YTD > 1000000.00
                   MOVE 'E020' TO WS-REJECT-CODE
               WHEN PPYM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E211' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3880-AGE-LAST-MAINT.
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
                   ADD PPYT-VOID-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PPYT-VOID-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD PPYT-VOID-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD PPYT-VOID-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPYT-VOID-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE STALE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAID AMT.
      *----------------------------------------------------------------*
       3890-PRORATE-PAID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPYM-PAID-AMT ROUNDED =
                   PPYT-STALE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PPYM-PAID-AMT
           END-IF
           ADD PPYM-PAID-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * IS -> PD WHEN EFF DATE IS PAST
      * PD -> EX WHEN EFF DATE IS PAST
      * EX -> VD WHEN EFF DATE IS PAST
      * VD -> PR WHEN EFF DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3900-SET-TRAN.
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
      * E320 CONTACT REQUIRED
      * E668 STALE AMT NEGATIVE
      * E037 STALE AMT OVER 1000000
      * E567 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3910-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPYM-CONTACT = SPACES
                   MOVE 'E320' TO WS-REJECT-CODE
               WHEN PPYM-STALE-AMT < 0
                   MOVE 'E668' TO WS-REJECT-CODE
               WHEN PPYM-STALE-AMT > 1000000.00
                   MOVE 'E037' TO WS-REJECT-CODE
               WHEN PPYM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E567' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STALE AMT IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3920-LIMIT-STALE.
           IF PPYM-STALE-AMT < 0.00
               MOVE 0.00 TO PPYM-STALE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PPYM-STALE-AMT > 5000.00
               MOVE 5000.00 TO PPYM-STALE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E657 CONTACT REQUIRED
      * E755 ISSUED AMT NEGATIVE
      * E261 ISSUED AMT OVER 5000
      * E623 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3930-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPYM-CONTACT = SPACES
                   MOVE 'E657' TO WS-REJECT-CODE
               WHEN PPYT-ISSUED-AMT < 0
                   MOVE 'E755' TO WS-REJECT-CODE
               WHEN PPYT-ISSUED-AMT > 5000.00
                   MOVE 'E261' TO WS-REJECT-CODE
               WHEN PPYM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E623' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3940-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 10.00
               MOVE 10.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 50000.00
               MOVE 50000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * VOID AMT IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3950-LIMIT-VOID.
           IF PPYM-VOID-AMT < 10.00
               MOVE 10.00 TO PPYM-VOID-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PPYM-VOID-AMT > 10000.00
               MOVE 10000.00 TO PPYM-VOID-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAID YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3960-ACCRUE-PAID.
           IF PPYM-PAID-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPYM-PAID-YTD * PPYM-RATE / 100 / 365
               ADD WS-WORK-AMT TO PPYM-ISSUED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR MATCH RULE CD:
      * A -> B WHEN OPEN DATE IS PAST
      * B -> F WHEN OPEN DATE IS PAST
      * F -> M WHEN OPEN DATE IS PAST
      * M -> S WHEN OPEN DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3970-SET-MATCH-RULE.
           EVALUATE PPYM-MATCH-RULE-CD
               WHEN 'A'
                   IF PPYM-OPEN-DATE > 0 AND PPYM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'B' TO PPYM-MATCH-RULE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'B'
                   IF PPYM-OPEN-DATE > 0 AND PPYM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'F' TO PPYM-MATCH-RULE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'F'
                   IF PPYM-OPEN-DATE > 0 AND PPYM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO PPYM-MATCH-RULE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF PPYM-OPEN-DATE > 0 AND PPYM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO PPYM-MATCH-RULE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT VOID AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3980-SPLIT-VOID.
           COMPUTE WS-WORK-AMT = PPYT-VOID-AMT / 3
           COMPUTE WS-WORK-AMT-2 = PPYT-VOID-AMT - (WS-WORK-AMT * 3)
           COMPUTE PPYT-ISSUED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STALE AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3990-ACCRUE-STALE.
           IF PPYM-STALE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPYM-STALE-AMT * PPYM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO PPYM-PAID-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DECISION CD:
      * AND -> BER WHEN OPEN DATE IS PAST
      * BER -> CZW WHEN OPEN DATE IS PAST
      * CZW -> SBV WHEN OPEN DATE IS PAST
      * SBV -> ZNG WHEN OPEN DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4000-SET-DECISION.
           EVALUATE PPYM-DECISION-CD
               WHEN 'AND'
                   IF PPYM-OPEN-DATE > 0 AND PPYM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BER' TO PPYM-DECISION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BER'
                   IF PPYM-OPEN-DATE > 0 AND PPYM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CZW' TO PPYM-DECISION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CZW'
                   IF PPYM-OPEN-DATE > 0 AND PPYM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SBV' TO PPYM-DECISION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SBV'
                   IF PPYM-OPEN-DATE > 0 AND PPYM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZNG' TO PPYM-DECISION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE EXCEPT YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4010-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPYM-REASON-CD
                   ADD PPYM-EXCEPT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPYM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PPYM-EXCEPT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE EXCEPT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO VOID AMT.
      *----------------------------------------------------------------*
       4020-PRORATE-VOID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPYT-VOID-AMT ROUNDED =
                   PPYM-EXCEPT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PPYT-VOID-AMT
           END-IF
           ADD PPYT-VOID-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PAID AMT IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4030-LIMIT-PAID.
           IF PPYT-PAID-AMT < 0.00
               MOVE 0.00 TO PPYT-PAID-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PPYT-PAID-AMT > 5000.00
               MOVE 5000.00 TO PPYT-PAID-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4040-CHECK-CLOSE.
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
               MOVE 'Y' TO PPYM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E090 REF NO REQUIRED
      * E261 PAID AMT NEGATIVE
      * E699 PAID AMT OVER 10000
      * E859 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       4050-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPYM-REF-NO = SPACES
                   MOVE 'E090' TO WS-REJECT-CODE
               WHEN PPYT-PAID-AMT < 0
                   MOVE 'E261' TO WS-REJECT-CODE
               WHEN PPYT-PAID-AMT > 10000.00
                   MOVE 'E699' TO WS-REJECT-CODE
               WHEN PPYM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E859' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4060-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPYM-REGION-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPYM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * VOID AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4070-FLAG-VOID.
           IF PPYM-VOID-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PPYM-VOID-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE STALE FEE ON THE WORK AMT 2.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4080-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN PPYM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO PPYM-STALE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYT-REVERSAL-FLAG = 'Y'
                   COMPUTE PPYM-STALE-AMT ROUNDED = WS-WORK-AMT-2 *
                       PPYM-DISCOUNT / 200
               WHEN PPYM-REGION-CD = 'A'
                   MOVE 3.00 TO PPYM-STALE-AMT
               WHEN PPYM-REGION-CD = 'B'
                   MOVE 7.00 TO PPYM-STALE-AMT
               WHEN OTHER
                   COMPUTE PPYM-STALE-AMT ROUNDED = WS-WORK-AMT-2 *
                       PPYM-DISCOUNT / 100
           END-EVALUATE
           IF PPYM-STALE-AMT > 0 AND PPYM-STALE-AMT < 5.00
               MOVE 5.00 TO PPYM-STALE-AMT
           END-IF
           IF PPYM-STALE-AMT > 50.00
               MOVE 50.00 TO PPYM-STALE-AMT
           END-IF
           ADD PPYM-STALE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE MATCH RULE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4090-DESCRIBE-MATCH-RULE.
           EVALUATE PPYM-MATCH-RULE-CD
               WHEN 'A'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'B'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'F'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'M'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'S'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'V'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE PAID AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4100-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPYM-REASON-CD
                   ADD PPYM-PAID-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPYM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PPYM-PAID-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ISSUED AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4110-SPLIT-ISSUED.
           COMPUTE WS-WORK-AMT = PPYT-ISSUED-AMT / 4
           COMPUTE WS-WORK-AMT-2 = PPYT-ISSUED-AMT - (WS-WORK-AMT * 4)
           COMPUTE PPYM-STALE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAID AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4120-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPYM-REGION-CD
                   ADD PPYM-PAID-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPYM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PPYM-PAID-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * A -> B WHEN POST DATE IS PAST
      * B -> D WHEN POST DATE IS PAST
      * D -> H WHEN POST DATE IS PAST
      * H -> N WHEN POST DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4130-SET-REGION.
           EVALUATE PPYM-REGION-CD
               WHEN 'A'
                   IF PPYT-POST-DATE > 0 AND PPYT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'B' TO PPYM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'B'
                   IF PPYT-POST-DATE > 0 AND PPYT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO PPYM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF PPYT-POST-DATE > 0 AND PPYT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO PPYM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF PPYT-POST-DATE > 0 AND PPYT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'N' TO PPYM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE MATCH RULE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4140-DESCRIBE-MATCH-RULE.
           EVALUATE PPYM-MATCH-RULE-CD
               WHEN 'A'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'B'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'F'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'M'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'S'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'V'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4150-AGE-LAST-MAINT.
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
                   ADD PPYM-VOID-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PPYM-VOID-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD PPYM-VOID-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PPYM-VOID-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPYM-VOID-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VOID AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4160-ACCRUE-VOID.
           IF PPYM-VOID-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPYM-VOID-AMT * PPYM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO PPYM-VOID-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       4170-COUNT-SEQ.
           IF PPYT-SEQ < 9999999
               ADD 1 TO PPYT-SEQ
           END-IF
           IF PPYT-SEQ NOT < 5
               MOVE 'Y' TO PPYM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E852 MEMO REQUIRED
      * E888 PAID AMT NEGATIVE
      * E149 PAID AMT OVER 250000
      * E974 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       4180-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPYT-MEMO = SPACES
                   MOVE 'E852' TO WS-REJECT-CODE
               WHEN PPYT-PAID-AMT < 0
                   MOVE 'E888' TO WS-REJECT-CODE
               WHEN PPYT-PAID-AMT > 250000.00
                   MOVE 'E149' TO WS-REJECT-CODE
               WHEN PPYT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E974' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ISSUED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO EXCEPT YTD.
      *----------------------------------------------------------------*
       4190-PRORATE-EXCEPT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPYM-EXCEPT-YTD ROUNDED =
                   PPYM-ISSUED-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PPYM-EXCEPT-YTD
           END-IF
           ADD PPYM-EXCEPT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ISSUED AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4200-FLAG-ISSUED.
           IF PPYM-ISSUED-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PPYM-ISSUED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 180 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4210-CHECK-LAST-ACT.
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
           IF WS-DAYS > 180
               MOVE 'Y' TO PPYM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4220-DESCRIBE-STATUS.
           EVALUATE PPYM-STATUS-CD
               WHEN 'DR'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'TK'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'WX'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT EXCEPT YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4230-SPLIT-EXCEPT.
           COMPUTE WS-WORK-AMT = PPYM-EXCEPT-YTD / 12
           COMPUTE WS-WORK-AMT-2 = PPYM-EXCEPT-YTD - (WS-WORK-AMT * 12)
           COMPUTE PPYT-STALE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E434 CONTACT REQUIRED
      * E252 VOID AMT NEGATIVE
      * E386 VOID AMT OVER 5000
      * E395 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       4240-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPYM-CONTACT = SPACES
                   MOVE 'E434' TO WS-REJECT-CODE
               WHEN PPYM-VOID-AMT < 0
                   MOVE 'E252' TO WS-REJECT-CODE
               WHEN PPYM-VOID-AMT > 5000.00
                   MOVE 'E386' TO WS-REJECT-CODE
               WHEN PPYM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E395' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E260 MEMO REQUIRED
      * E519 PAID AMT NEGATIVE
      * E747 PAID AMT OVER 250000
      * E813 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       4250-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPYT-MEMO = SPACES
                   MOVE 'E260' TO WS-REJECT-CODE
               WHEN PPYM-PAID-AMT < 0
                   MOVE 'E519' TO WS-REJECT-CODE
               WHEN PPYM-PAID-AMT > 250000.00
                   MOVE 'E747' TO WS-REJECT-CODE
               WHEN PPYM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E813' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE STALE AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4260-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPYM-REGION-CD
                   ADD PPYT-STALE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPYM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PPYT-STALE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4270-SPLIT-EXCEPT.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 3)
           COMPUTE PPYM-EXCEPT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ISSUED SCHED TIER FOR THE PAID YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4280-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-ISSUED-SCHED-RATE(1) TO PPYM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF PPYM-PAID-YTD NOT < WS-ISSUED-SCHED-LOW(WS-TX)
                   MOVE WS-ISSUED-SCHED-RATE(WS-TX) TO PPYM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPYM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPYM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-90, 91-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4290-AGE-OPEN.
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
                   ADD PPYT-PAID-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD PPYT-PAID-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD PPYT-PAID-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PPYT-PAID-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPYT-PAID-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE EXCEPT GRID TIER FOR THE PAID YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4300-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-EXCEPT-GRID-RATE(1) TO PPYM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF PPYM-PAID-YTD NOT < WS-EXCEPT-GRID-LOW(WS-TX)
                   MOVE WS-EXCEPT-GRID-RATE(WS-TX) TO PPYM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPYM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPYM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4310-COUNT-EXCEPT.
           IF PPYM-EXCEPT-CNT < 9999999
               ADD 1 TO PPYM-EXCEPT-CNT
           END-IF
           IF PPYM-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO PPYM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ISSUED AMT IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4320-LIMIT-ISSUED.
           IF PPYT-ISSUED-AMT < 100.00
               MOVE 100.00 TO PPYT-ISSUED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PPYT-ISSUED-AMT > 2500.00
               MOVE 2500.00 TO PPYT-ISSUED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAID YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4330-FLAG-PAID.
           IF PPYM-PAID-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PPYM-PAID-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E020 ADDR REQUIRED
      * E402 PAID AMT NEGATIVE
      * E244 PAID AMT OVER 5000
      * E511 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       4340-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPYM-ADDR = SPACES
                   MOVE 'E020' TO WS-REJECT-CODE
               WHEN PPYT-PAID-AMT < 0
                   MOVE 'E402' TO WS-REJECT-CODE
               WHEN PPYT-PAID-AMT > 5000.00
                   MOVE 'E244' TO WS-REJECT-CODE
               WHEN PPYM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E511' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 365 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4350-CHECK-OPEN.
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
           IF WS-DAYS > 365
               MOVE 'Y' TO PPYM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E063 NAME REQUIRED
      * E039 EXCEPT AMT NEGATIVE
      * E277 EXCEPT AMT OVER 50000
      * E257 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       4360-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPYM-NAME = SPACES
                   MOVE 'E063' TO WS-REJECT-CODE
               WHEN PPYM-EXCEPT-AMT < 0
                   MOVE 'E039' TO WS-REJECT-CODE
               WHEN PPYM-EXCEPT-AMT > 50000.00
                   MOVE 'E277' TO WS-REJECT-CODE
               WHEN PPYT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E257' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ISSUED AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4370-FLAG-ISSUED.
           IF PPYT-ISSUED-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PPYT-ISSUED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       4380-COUNT-PAYMENT.
           IF PPYM-PAYMENT-CNT < 9999999
               ADD 1 TO PPYM-PAYMENT-CNT
           END-IF
           IF PPYM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO PPYM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ISSUED AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4390-ACCRUE-ISSUED.
           IF PPYT-ISSUED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPYT-ISSUED-AMT * PPYM-RATE / 100 / 365
               ADD WS-WORK-AMT TO PPYM-STALE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAID YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4400-ACCRUE-PAID.
           IF PPYM-PAID-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPYM-PAID-YTD * PPYM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO PPYM-EXCEPT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE VOID AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO EXCEPT YTD.
      *----------------------------------------------------------------*
       4410-PRORATE-EXCEPT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPYM-EXCEPT-YTD ROUNDED =
                   PPYM-VOID-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PPYM-EXCEPT-YTD
           END-IF
           ADD PPYM-EXCEPT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4420-DESCRIBE-REGION.
           EVALUATE PPYM-REGION-CD
               WHEN 'A'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'B'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'D'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'H'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'N'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'Y'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4430-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE EXCEPT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       4440-PRORATE-EXCEPT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   PPYM-EXCEPT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * IS -> PD WHEN LAST MAINT DATE IS PAST
      * PD -> EX WHEN LAST MAINT DATE IS PAST
      * EX -> VD WHEN LAST MAINT DATE IS PAST
      * VD -> PR WHEN LAST MAINT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4450-SET-TRAN.
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
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4460-COUNT-CYCLE.
           IF PPYM-CYCLE-CNT < 9999999
               ADD 1 TO PPYM-CYCLE-CNT
           END-IF
           IF PPYM-CYCLE-CNT NOT < 5
               MOVE 'Y' TO PPYM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4470-AGE-POST.
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
                   ADD PPYM-VOID-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD PPYM-VOID-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD PPYM-VOID-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PPYM-VOID-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPYM-VOID-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN LAST MAINT DATE IS PAST
      * BAT -> MAL WHEN LAST MAINT DATE IS PAST
      * MAL -> TEL WHEN LAST MAINT DATE IS PAST
      * TEL -> ONL WHEN LAST MAINT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4480-SET-CHANNEL.
           EVALUATE PPYT-CHANNEL-CD
               WHEN 'BRN'
                   IF PPYM-LAST-MAINT-DATE > 0 AND PPYM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'BAT' TO PPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF PPYM-LAST-MAINT-DATE > 0 AND PPYM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'MAL' TO PPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF PPYM-LAST-MAINT-DATE > 0 AND PPYM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'TEL' TO PPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF PPYM-LAST-MAINT-DATE > 0 AND PPYM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'ONL' TO PPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE VOID AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO STALE YTD.
      *----------------------------------------------------------------*
       4490-PRORATE-STALE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPYM-STALE-YTD ROUNDED =
                   PPYM-VOID-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PPYM-STALE-YTD
           END-IF
           ADD PPYM-STALE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E559 ADDR REQUIRED
      * E682 VOID AMT NEGATIVE
      * E883 VOID AMT OVER 10000
      * E805 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       4500-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPYM-ADDR = SPACES
                   MOVE 'E559' TO WS-REJECT-CODE
               WHEN PPYM-VOID-AMT < 0
                   MOVE 'E682' TO WS-REJECT-CODE
               WHEN PPYM-VOID-AMT > 10000.00
                   MOVE 'E883' TO WS-REJECT-CODE
               WHEN PPYM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E805' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4510-ACCRUE-RATE.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * PPYM-RATE / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4520-CHECK-NEXT-DUE.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO PPYM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ISSUED AMT BY DECISION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4530-ACCUM-DECISION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPYM-DECISION-CD
                   ADD PPYM-ISSUED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPYM-DECISION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PPYM-ISSUED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT EXCEPT YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4540-SPLIT-EXCEPT.
           COMPUTE WS-WORK-AMT = PPYM-EXCEPT-YTD / 3
           COMPUTE WS-WORK-AMT-2 = PPYM-EXCEPT-YTD - (WS-WORK-AMT * 3)
           COMPUTE PPYM-STALE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAID AMT IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4550-LIMIT-PAID.
           IF PPYM-PAID-AMT < 10.00
               MOVE 10.00 TO PPYM-PAID-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PPYM-PAID-AMT > 50000.00
               MOVE 50000.00 TO PPYM-PAID-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT EXCEPT AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4560-SPLIT-EXCEPT.
           COMPUTE WS-WORK-AMT = PPYM-EXCEPT-AMT / 12
           COMPUTE WS-WORK-AMT-2 = PPYM-EXCEPT-AMT - (WS-WORK-AMT * 12)
           COMPUTE PPYT-ISSUED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT EXCEPT YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4570-SPLIT-EXCEPT.
           COMPUTE WS-WORK-AMT = PPYM-EXCEPT-YTD / 3
           COMPUTE WS-WORK-AMT-2 = PPYM-EXCEPT-YTD - (WS-WORK-AMT * 3)
           COMPUTE PPYM-PAID-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ISSUED SCHED TIER FOR THE PAID YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4580-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-ISSUED-SCHED-RATE(1) TO PPYM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF PPYM-PAID-YTD NOT < WS-ISSUED-SCHED-LOW(WS-TX)
                   MOVE WS-ISSUED-SCHED-RATE(WS-TX) TO PPYM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPYM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPYM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT STALE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4590-SPLIT-STALE.
           COMPUTE WS-WORK-AMT = PPYT-STALE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = PPYT-STALE-AMT - (WS-WORK-AMT * 4)
           COMPUTE PPYM-STALE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ISSUED YTD AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4600-ACCRUE-ISSUED.
           IF PPYM-ISSUED-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPYM-ISSUED-YTD * PPYM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO PPYM-EXCEPT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PAID AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4610-SPLIT-PAID.
           COMPUTE WS-WORK-AMT = PPYM-PAID-AMT / 4
           COMPUTE WS-WORK-AMT-2 = PPYM-PAID-AMT - (WS-WORK-AMT * 4)
           COMPUTE PPYM-VOID-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4620-LIMIT-ITEM.
           IF WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 5000.00
               MOVE 5000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4630-AGE-EFF.
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
                   ADD PPYT-STALE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD PPYT-STALE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD PPYT-STALE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PPYT-STALE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPYT-STALE-AMT TO WS-AGE-AMT(5)
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
                   MOVE PPSTAT-PAID-AMT
                     TO PPYM-PAID-AMT
                   MOVE PPSTAT-OVERRIDE-FLAG
                     TO PPYM-OVERRIDE-FLAG
                   MOVE PPSTAT-EXEMPT-FLAG
                     TO PPYM-EXEMPT-FLAG
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
           DISPLAY 'PPYRNEW READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'PPYRNEW' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
