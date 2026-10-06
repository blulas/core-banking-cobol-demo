      *================================================================*
      * PROGRAM  : PPYEXCP                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : POSITIVE PAY - EXCEPTION PROCESSING. READS THE PPY  *
      *            TRANSACTIONS, UPDATES THE PPY MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. CORPORATE POSITIVE PAY ISSUE    *
      *            FILE MATCHING AND EXCEPTIONS                        *
      * INPUT    : PPYMST (KSDS), PPYTRN, BANKCTL                      *
      * OUTPUT   : PPYOUT, PPYEXRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, PPELG, PPPEN1           *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-06-07 SKN  ORIGINAL                                       *
      * 1993-03-26 TWH  ISSUED LIMIT REVISED REQ1856                   *
      * 1997-01-12 BHS  ISSUED CALCULATION CORRECTED INC1537           *
      * 1997-07-19 TWH  EXCEPT STATUS CODES ADDED PRB2068              *
      * 1999-04-10 JLM  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2001-10-08 GAF  PAID CALCULATION CORRECTED CHG2373             *
      * 2001-01-01 RJT  STALE TABLE EXPANDED CHG5480                   *
      * 2003-09-08 LMB  NEW VOID REPORT COLUMNS INC3190                *
      * 2003-02-06 EWS  PAID NOTICE TEXT PER LEGAL INC2218             *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PPYEXCP.
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
           SELECT REPORT-FILE ASSIGN TO "PPYEXRPT"
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
      * WS-STALE-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-STALE-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000558000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000503500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007500000000033000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012500000000094000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015000000000869000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016000000000205000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016250000000818000'.
       01  WS-STALE-SCHED-TBL REDEFINES WS-STALE-SCHED-VALUES.
           05  WS-STALE-SCHED-ROW OCCURS 7 TIMES.
               10  WS-STALE-SCHED-LOW      PIC 9(09)V99.
               10  WS-STALE-SCHED-RATE     PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-VOID-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-VOID-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000715000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000736500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000150000000248000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000650000000008000'.
       01  WS-VOID-TIER-TBL REDEFINES WS-VOID-TIER-VALUES.
           05  WS-VOID-TIER-ROW OCCURS 4 TIMES.
               10  WS-VOID-TIER-LOW        PIC 9(09)V99.
               10  WS-VOID-TIER-RATE       PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-VOID-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-VOID-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000225000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000209000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012500000000376000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012750000000632500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022750000000573500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022850000000420000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00032850000000706000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00033350000000849500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00033600000000255000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00036100000000823500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00046100000000798000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00047100000000876500'.
       01  WS-VOID-SCHED-TBL REDEFINES WS-VOID-SCHED-VALUES.
           05  WS-VOID-SCHED-ROW OCCURS 12 TIMES.
               10  WS-VOID-SCHED-LOW       PIC 9(09)V99.
               10  WS-VOID-SCHED-RATE      PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-PAID-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PAID-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000423500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000211000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003000000000509500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003100000000102500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003600000000419000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004600000000412000'.
       01  WS-PAID-TIER-TBL REDEFINES WS-PAID-TIER-VALUES.
           05  WS-PAID-TIER-ROW OCCURS 6 TIMES.
               10  WS-PAID-TIER-LOW        PIC 9(09)V99.
               10  WS-PAID-TIER-RATE       PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  PPYEXCP  POSITIVE PAY'.
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
       COPY PPPEN1P.

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
               PERFORM 3000-COUNT-NOTICE
               PERFORM 3010-LIMIT-VOID
               PERFORM 3020-FLAG-PAID
               PERFORM 3030-SET-REASON
               PERFORM 3040-AGE-EFF
               PERFORM 3050-SPLIT-VOID
               PERFORM 3060-COUNT-EXCEPT
               PERFORM 3070-SPLIT-STALE
               PERFORM 3080-EDIT-ADDR
               PERFORM 3090-DESCRIBE-REASON
               PERFORM 3100-AGE-EFF
               PERFORM 3110-ASSESS-OVERRIDE
               PERFORM 3120-SET-REGION
               PERFORM 3130-CHECK-EFF
               PERFORM 3140-COUNT-PAYMENT
               PERFORM 3150-COUNT-PAYMENT
               PERFORM 3160-COUNT-NOTICE
               PERFORM 3170-LIMIT-PAID
               PERFORM 3180-CHECK-NEXT-REVIEW
               PERFORM 3190-PRORATE-PAID
               PERFORM 3200-CHECK-LAST-ACT
               PERFORM 3210-SET-TRAN
               PERFORM 3220-SPLIT-PAID
               PERFORM 3230-SPLIT-VOID
               PERFORM 3240-DESCRIBE-MATCH-RULE
               PERFORM 3250-DESCRIBE-CYCLE
               PERFORM 3260-ASSESS-STOP
               PERFORM 3270-SELECT-RATE
               PERFORM 3280-PRORATE-ISSUED
               PERFORM 3290-SELECT-MARGIN
               PERFORM 3300-PRORATE-VOID
               PERFORM 3310-ACCUM-REGION
               PERFORM 3320-EDIT-DESC
               PERFORM 3330-SELECT-MARGIN
               PERFORM 3340-SELECT-DISCOUNT
               PERFORM 3350-ACCUM-CYCLE
               PERFORM 3360-PRORATE-STALE
               PERFORM 3370-CHECK-NEXT-DUE
               PERFORM 3380-CHECK-EXPIRY
               PERFORM 3390-ASSESS-STOP
               PERFORM 3400-AGE-LAST-MAINT
               PERFORM 3410-ASSESS-AUTO-RENEW
               PERFORM 3420-SELECT-DISCOUNT
               PERFORM 3430-SPLIT-STALE
               PERFORM 3440-AGE-CLOSE
               PERFORM 3450-EDIT-CUST-ID
               PERFORM 3460-FLAG-ITEM
               PERFORM 3470-AGE-EFFECTIVE
               PERFORM 3480-DESCRIBE-CYCLE
               PERFORM 3490-COUNT-PAYMENT
               PERFORM 3500-PRORATE-EXCEPT
               PERFORM 3510-SELECT-DISCOUNT
               PERFORM 3520-SPLIT-VOID
               PERFORM 3530-EDIT-ADDR
               PERFORM 3540-ACCRUE-MARGIN
               PERFORM 3550-SELECT-RATE
               PERFORM 3560-PRORATE-STALE
               PERFORM 3570-SPLIT-ISSUED
               PERFORM 3580-SET-TRAN
               PERFORM 3590-AGE-EXPIRY
               PERFORM 3600-ASSESS-OVERRIDE
               PERFORM 3610-FLAG-VOID
               PERFORM 3620-COUNT-NOTICE
               PERFORM 3630-CHECK-EXPIRY
               PERFORM 3640-DESCRIBE-MATCH-RULE
               PERFORM 3650-ASSESS-STOP
               PERFORM 3660-DESCRIBE-REGION
               PERFORM 3670-ASSESS-REVERSAL
               PERFORM 3680-ACCRUE-EXCEPT
               PERFORM 3690-COUNT-EXCEPT
               PERFORM 3700-COUNT-EXCEPT
               PERFORM 3710-LIMIT-VOID
               PERFORM 3720-ACCUM-STATUS
               PERFORM 3730-COUNT-EXCEPT
               PERFORM 3740-SET-DECISION
               PERFORM 3750-ACCUM-CHANNEL
               PERFORM 3760-FLAG-VOID
               PERFORM 3770-CHECK-CLOSE
               PERFORM 3780-FLAG-VOID
               PERFORM 3790-EDIT-REJECT
               PERFORM 3800-SPLIT-ISSUED
               PERFORM 3810-ACCUM-CYCLE
               PERFORM 3820-SET-MATCH-RULE
               PERFORM 3830-SET-DECISION
               PERFORM 3840-FLAG-VOID
               PERFORM 3850-EDIT-REJECT
               PERFORM 3860-COUNT-SEQ
               PERFORM 3870-AGE-LAST-ACT
               PERFORM 3880-ACCRUE-EXCEPT
               PERFORM 3890-PRORATE-ISSUED
               PERFORM 3900-LIMIT-ISSUED
               PERFORM 3910-ACCRUE-VOID
               PERFORM 3920-COUNT-EXCEPT
               PERFORM 3930-CHECK-OPEN
               PERFORM 3940-SELECT-DISCOUNT
               PERFORM 3950-FLAG-VOID
               PERFORM 3960-SET-REGION
               PERFORM 3970-COUNT-EXCEPT
               PERFORM 3980-COUNT-NOTICE
               PERFORM 3990-AGE-POST
               PERFORM 4000-EDIT-OPERATOR
               PERFORM 4010-DESCRIBE-REASON
               PERFORM 4020-FLAG-ITEM
               PERFORM 4030-SELECT-DISCOUNT
               PERFORM 4040-SET-CHANNEL
               PERFORM 4050-PRORATE-VOID
               PERFORM 4060-EDIT-NAME
               PERFORM 4070-LIMIT-PAID
               PERFORM 4080-ACCUM-TRAN
               PERFORM 4090-DESCRIBE-DECISION
               PERFORM 4100-DESCRIBE-REGION
               PERFORM 4110-LIMIT-PAID
               PERFORM 4120-SELECT-DISCOUNT
               PERFORM 4130-AGE-NEXT-REVIEW
               PERFORM 4140-LIMIT-VOID
               PERFORM 4150-ASSESS-OVERRIDE
               PERFORM 4160-FLAG-PAID
               PERFORM 4170-CHECK-POST
               PERFORM 4180-ASSESS-NOTICE-SENT
               PERFORM 4190-SET-CYCLE
               PERFORM 4200-LIMIT-ISSUED
               PERFORM 4210-EDIT-NAME
               PERFORM 4220-FLAG-VOID
               PERFORM 4230-ASSESS-NOTICE-SENT
               PERFORM 4240-SELECT-DISCOUNT
               PERFORM 4250-COUNT-EXCEPT
               PERFORM 4260-DESCRIBE-MATCH-RULE
               PERFORM 4270-DESCRIBE-REGION
               PERFORM 7020-CALL-PPELG
               PERFORM 7010-CALL-PPPEN1
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
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3000-COUNT-NOTICE.
           IF PPYM-NOTICE-CNT < 9999999
               ADD 1 TO PPYM-NOTICE-CNT
           END-IF
           IF PPYM-NOTICE-CNT NOT < 10
               MOVE 'Y' TO PPYM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * VOID YTD IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3010-LIMIT-VOID.
           IF PPYM-VOID-YTD < 100.00
               MOVE 100.00 TO PPYM-VOID-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PPYM-VOID-YTD > 2500.00
               MOVE 2500.00 TO PPYM-VOID-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAID AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3020-FLAG-PAID.
           IF PPYT-PAID-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PPYT-PAID-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * B -> M WHEN NEXT DUE DATE IS PAST
      * M -> N WHEN NEXT DUE DATE IS PAST
      * N -> U WHEN NEXT DUE DATE IS PAST
      * U -> V WHEN NEXT DUE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3030-SET-REASON.
           EVALUATE PPYM-REASON-CD
               WHEN 'B'
                   IF PPYM-NEXT-DUE-DATE > 0 AND PPYM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO PPYM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF PPYM-NEXT-DUE-DATE > 0 AND PPYM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'N' TO PPYM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'N'
                   IF PPYM-NEXT-DUE-DATE > 0 AND PPYM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO PPYM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF PPYM-NEXT-DUE-DATE > 0 AND PPYM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'V' TO PPYM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3040-AGE-EFF.
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
               WHEN WS-DAYS NOT > 45
                   ADD PPYM-EXCEPT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD PPYM-EXCEPT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPYM-EXCEPT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT VOID AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3050-SPLIT-VOID.
           COMPUTE WS-WORK-AMT = PPYM-VOID-AMT / 3
           COMPUTE WS-WORK-AMT-2 = PPYM-VOID-AMT - (WS-WORK-AMT * 3)
           COMPUTE PPYM-ISSUED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3060-COUNT-EXCEPT.
           IF PPYM-EXCEPT-CNT < 9999999
               ADD 1 TO PPYM-EXCEPT-CNT
           END-IF
           IF PPYM-EXCEPT-CNT NOT < 3
               MOVE 'Y' TO PPYM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT STALE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3070-SPLIT-STALE.
           COMPUTE WS-WORK-AMT = PPYT-STALE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = PPYT-STALE-AMT - (WS-WORK-AMT * 3)
           COMPUTE PPYM-EXCEPT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E335 ADDR REQUIRED
      * E233 VOID AMT NEGATIVE
      * E462 VOID AMT OVER 1000000
      * E790 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3080-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPYM-ADDR = SPACES
                   MOVE 'E335' TO WS-REJECT-CODE
               WHEN PPYT-VOID-AMT < 0
                   MOVE 'E233' TO WS-REJECT-CODE
               WHEN PPYT-VOID-AMT > 1000000.00
                   MOVE 'E462' TO WS-REJECT-CODE
               WHEN PPYM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E790' TO WS-REJECT-CODE
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
       3090-DESCRIBE-REASON.
           EVALUATE PPYM-REASON-CD
               WHEN 'B'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'M'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'N'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'U'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'V'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'Z'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-15, 16-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3100-AGE-EFF.
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
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE EXCEPT FEE ON THE ISSUED YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3110-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN PPYM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO PPYM-EXCEPT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYM-EXEMPT-FLAG = 'Y'
                   COMPUTE PPYM-EXCEPT-AMT ROUNDED = PPYM-ISSUED-YTD *
                       PPYM-DISCOUNT / 200
               WHEN PPYT-TRAN-CD = 'IS'
                   MOVE 7.00 TO PPYM-EXCEPT-AMT
               WHEN PPYT-TRAN-CD = 'PD'
                   MOVE 3.00 TO PPYM-EXCEPT-AMT
               WHEN OTHER
                   COMPUTE PPYM-EXCEPT-AMT ROUNDED = PPYM-ISSUED-YTD *
                       PPYM-DISCOUNT / 100
           END-EVALUATE
           IF PPYM-EXCEPT-AMT > 0 AND PPYM-EXCEPT-AMT < 2.00
               MOVE 2.00 TO PPYM-EXCEPT-AMT
           END-IF
           IF PPYM-EXCEPT-AMT > 35.00
               MOVE 35.00 TO PPYM-EXCEPT-AMT
           END-IF
           ADD PPYM-EXCEPT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * A -> B WHEN CLOSE DATE IS PAST
      * B -> D WHEN CLOSE DATE IS PAST
      * D -> H WHEN CLOSE DATE IS PAST
      * H -> N WHEN CLOSE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3120-SET-REGION.
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
      * DAYS SINCE EFF DATE. MORE THAN 365 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3130-CHECK-EFF.
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
               MOVE 'Y' TO PPYM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3140-COUNT-PAYMENT.
           IF PPYM-PAYMENT-CNT < 9999999
               ADD 1 TO PPYM-PAYMENT-CNT
           END-IF
           IF PPYM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO PPYM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3150-COUNT-PAYMENT.
           IF PPYM-PAYMENT-CNT < 9999999
               ADD 1 TO PPYM-PAYMENT-CNT
           END-IF
           IF PPYM-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO PPYM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3160-COUNT-NOTICE.
           IF PPYM-NOTICE-CNT < 9999999
               ADD 1 TO PPYM-NOTICE-CNT
           END-IF
           IF PPYM-NOTICE-CNT NOT < 12
               MOVE 'Y' TO PPYM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAID AMT IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3170-LIMIT-PAID.
           IF PPYM-PAID-AMT < 10.00
               MOVE 10.00 TO PPYM-PAID-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PPYM-PAID-AMT > 5000.00
               MOVE 5000.00 TO PPYM-PAID-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 90 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3180-CHECK-NEXT-REVIEW.
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
           IF WS-DAYS > 90
               MOVE 'Y' TO PPYM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PAID AMT.
      *----------------------------------------------------------------*
       3190-PRORATE-PAID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPYM-PAID-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PPYM-PAID-AMT
           END-IF
           ADD PPYM-PAID-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 45 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3200-CHECK-LAST-ACT.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO PPYM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * IS -> PD WHEN OPEN DATE IS PAST
      * PD -> EX WHEN OPEN DATE IS PAST
      * EX -> VD WHEN OPEN DATE IS PAST
      * VD -> PR WHEN OPEN DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3210-SET-TRAN.
           EVALUATE PPYT-TRAN-CD
               WHEN 'IS'
                   IF PPYM-OPEN-DATE > 0 AND PPYM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PD' TO PPYT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PD'
                   IF PPYM-OPEN-DATE > 0 AND PPYM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EX' TO PPYT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'EX'
                   IF PPYM-OPEN-DATE > 0 AND PPYM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VD' TO PPYT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VD'
                   IF PPYM-OPEN-DATE > 0 AND PPYM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PR' TO PPYT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT PAID YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3220-SPLIT-PAID.
           COMPUTE WS-WORK-AMT = PPYM-PAID-YTD / 3
           COMPUTE WS-WORK-AMT-2 = PPYM-PAID-YTD - (WS-WORK-AMT * 3)
           COMPUTE PPYM-VOID-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3230-SPLIT-VOID.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 2)
           COMPUTE PPYM-VOID-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE MATCH RULE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3240-DESCRIBE-MATCH-RULE.
           EVALUATE PPYM-MATCH-RULE-CD
               WHEN 'A'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'B'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'F'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'M'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'S'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'V'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3250-DESCRIBE-CYCLE.
           EVALUATE PPYM-CYCLE-CD
               WHEN 'A'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'B'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'C'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'R'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'T'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'Y'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE STALE FEE ON THE WORK AMT 2.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3260-ASSESS-STOP.
           EVALUATE TRUE
               WHEN PPYM-STOP-FLAG = 'Y'
                   MOVE ZERO TO PPYM-STALE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE PPYM-STALE-AMT ROUNDED = WS-WORK-AMT-2 *
                       PPYM-RATE / 200
               WHEN PPYT-TRAN-CD = 'IS'
                   MOVE 5.00 TO PPYM-STALE-AMT
               WHEN PPYT-TRAN-CD = 'PD'
                   MOVE 5.00 TO PPYM-STALE-AMT
               WHEN OTHER
                   COMPUTE PPYM-STALE-AMT ROUNDED = WS-WORK-AMT-2 *
                       PPYM-RATE / 100
           END-EVALUATE
           IF PPYM-STALE-AMT > 0 AND PPYM-STALE-AMT < 5.00
               MOVE 5.00 TO PPYM-STALE-AMT
           END-IF
           IF PPYM-STALE-AMT > 100.00
               MOVE 100.00 TO PPYM-STALE-AMT
           END-IF
           ADD PPYM-STALE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE VOID TIER TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3270-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-VOID-TIER-RATE(1) TO PPYM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF WS-WORK-AMT-2 NOT < WS-VOID-TIER-LOW(WS-TX)
                   MOVE WS-VOID-TIER-RATE(WS-TX) TO PPYM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPYM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPYM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE EXCEPT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ISSUED AMT.
      *----------------------------------------------------------------*
       3280-PRORATE-ISSUED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPYM-ISSUED-AMT ROUNDED =
                   PPYM-EXCEPT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PPYM-ISSUED-AMT
           END-IF
           ADD PPYM-ISSUED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE PAID TIER TIER FOR THE PAID AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3290-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PAID-TIER-RATE(1) TO PPYM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF PPYM-PAID-AMT NOT < WS-PAID-TIER-LOW(WS-TX)
                   MOVE WS-PAID-TIER-RATE(WS-TX) TO PPYM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPYM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPYM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE EXCEPT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO VOID AMT.
      *----------------------------------------------------------------*
       3300-PRORATE-VOID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPYM-VOID-AMT ROUNDED =
                   PPYM-EXCEPT-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PPYM-VOID-AMT
           END-IF
           ADD PPYM-VOID-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE ISSUED AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3310-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPYM-REGION-CD
                   ADD PPYM-ISSUED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPYM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PPYM-ISSUED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E784 DESC REQUIRED
      * E196 WORK AMT NEGATIVE
      * E815 WORK AMT OVER 25000
      * E613 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3320-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPYM-DESC = SPACES
                   MOVE 'E784' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E196' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 25000.00
                   MOVE 'E815' TO WS-REJECT-CODE
               WHEN PPYM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E613' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE VOID TIER TIER FOR THE PAID AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3330-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-VOID-TIER-RATE(1) TO PPYM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF PPYT-PAID-AMT NOT < WS-VOID-TIER-LOW(WS-TX)
                   MOVE WS-VOID-TIER-RATE(WS-TX) TO PPYM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPYM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPYM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAID TIER TIER FOR THE VOID AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3340-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-PAID-TIER-RATE(1) TO PPYM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF PPYM-VOID-AMT NOT < WS-PAID-TIER-LOW(WS-TX)
                   MOVE WS-PAID-TIER-RATE(WS-TX) TO PPYM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPYM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPYM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAID AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3350-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPYM-CYCLE-CD
                   ADD PPYT-PAID-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPYM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PPYT-PAID-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ISSUED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO STALE YTD.
      *----------------------------------------------------------------*
       3360-PRORATE-STALE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPYM-STALE-YTD ROUNDED =
                   PPYM-ISSUED-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PPYM-STALE-YTD
           END-IF
           ADD PPYM-STALE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3370-CHECK-NEXT-DUE.
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
           IF WS-DAYS > 365
               MOVE 'Y' TO PPYM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 45 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3380-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF PPYM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPYM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO PPYM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE VOID FEE ON THE EXCEPT AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3390-ASSESS-STOP.
           EVALUATE TRUE
               WHEN PPYM-STOP-FLAG = 'Y'
                   MOVE ZERO TO PPYT-VOID-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE PPYT-VOID-AMT ROUNDED = PPYM-EXCEPT-AMT *
                       PPYM-RATE / 200
               WHEN PPYM-CYCLE-CD = 'A'
                   MOVE 10.00 TO PPYT-VOID-AMT
               WHEN PPYM-CYCLE-CD = 'B'
                   MOVE 7.00 TO PPYT-VOID-AMT
               WHEN OTHER
                   COMPUTE PPYT-VOID-AMT ROUNDED = PPYM-EXCEPT-AMT *
                       PPYM-RATE / 100
           END-EVALUATE
           IF PPYT-VOID-AMT > 0 AND PPYT-VOID-AMT < 1.00
               MOVE 1.00 TO PPYT-VOID-AMT
           END-IF
           IF PPYT-VOID-AMT > 250.00
               MOVE 250.00 TO PPYT-VOID-AMT
           END-IF
           ADD PPYT-VOID-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-90, 91-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3400-AGE-LAST-MAINT.
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
                   ADD PPYM-EXCEPT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD PPYM-EXCEPT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD PPYM-EXCEPT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PPYM-EXCEPT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPYM-EXCEPT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE ISSUED FEE ON THE PAID YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3410-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN PPYM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO PPYM-ISSUED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYT-REVERSAL-FLAG = 'Y'
                   COMPUTE PPYM-ISSUED-AMT ROUNDED = PPYM-PAID-YTD *
                       PPYM-RATE / 200
               WHEN PPYM-CYCLE-CD = 'A'
                   MOVE 3.00 TO PPYM-ISSUED-AMT
               WHEN PPYM-CYCLE-CD = 'B'
                   MOVE 5.00 TO PPYM-ISSUED-AMT
               WHEN OTHER
                   COMPUTE PPYM-ISSUED-AMT ROUNDED = PPYM-PAID-YTD *
                       PPYM-RATE / 100
           END-EVALUATE
           IF PPYM-ISSUED-AMT > 0 AND PPYM-ISSUED-AMT < 10.00
               MOVE 10.00 TO PPYM-ISSUED-AMT
           END-IF
           IF PPYM-ISSUED-AMT > 250.00
               MOVE 250.00 TO PPYM-ISSUED-AMT
           END-IF
           ADD PPYM-ISSUED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE VOID SCHED TIER FOR THE VOID YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3420-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-VOID-SCHED-RATE(1) TO PPYM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF PPYM-VOID-YTD NOT < WS-VOID-SCHED-LOW(WS-TX)
                   MOVE WS-VOID-SCHED-RATE(WS-TX) TO PPYM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPYM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPYM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT STALE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3430-SPLIT-STALE.
           COMPUTE WS-WORK-AMT = PPYM-STALE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = PPYM-STALE-AMT - (WS-WORK-AMT * 12)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3440-AGE-CLOSE.
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
                   ADD PPYT-PAID-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD PPYT-PAID-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD PPYT-PAID-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PPYT-PAID-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPYT-PAID-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E656 CUST ID REQUIRED
      * E510 VOID AMT NEGATIVE
      * E269 VOID AMT OVER 1000000
      * E992 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3450-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPYM-CUST-ID = SPACES
                   MOVE 'E656' TO WS-REJECT-CODE
               WHEN PPYM-VOID-AMT < 0
                   MOVE 'E510' TO WS-REJECT-CODE
               WHEN PPYM-VOID-AMT > 1000000.00
                   MOVE 'E269' TO WS-REJECT-CODE
               WHEN PPYM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E992' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3460-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3470-AGE-EFFECTIVE.
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
                   ADD PPYT-STALE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD PPYT-STALE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD PPYT-STALE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PPYT-STALE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPYT-STALE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3480-DESCRIBE-CYCLE.
           EVALUATE PPYM-CYCLE-CD
               WHEN 'A'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'B'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'C'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'R'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'T'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'Y'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3490-COUNT-PAYMENT.
           IF PPYM-PAYMENT-CNT < 9999999
               ADD 1 TO PPYM-PAYMENT-CNT
           END-IF
           IF PPYM-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO PPYM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE VOID AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO EXCEPT YTD.
      *----------------------------------------------------------------*
       3500-PRORATE-EXCEPT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPYM-EXCEPT-YTD ROUNDED =
                   PPYM-VOID-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PPYM-EXCEPT-YTD
           END-IF
           ADD PPYM-EXCEPT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE VOID SCHED TIER FOR THE PAID AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3510-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-VOID-SCHED-RATE(1) TO PPYM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF PPYT-PAID-AMT NOT < WS-VOID-SCHED-LOW(WS-TX)
                   MOVE WS-VOID-SCHED-RATE(WS-TX) TO PPYM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPYM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPYM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT VOID AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3520-SPLIT-VOID.
           COMPUTE WS-WORK-AMT = PPYM-VOID-AMT / 2
           COMPUTE WS-WORK-AMT-2 = PPYM-VOID-AMT - (WS-WORK-AMT * 2)
           COMPUTE PPYM-STALE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E856 ADDR REQUIRED
      * E827 PAID AMT NEGATIVE
      * E867 PAID AMT OVER 1000000
      * E082 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3530-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPYM-ADDR = SPACES
                   MOVE 'E856' TO WS-REJECT-CODE
               WHEN PPYT-PAID-AMT < 0
                   MOVE 'E827' TO WS-REJECT-CODE
               WHEN PPYT-PAID-AMT > 1000000.00
                   MOVE 'E867' TO WS-REJECT-CODE
               WHEN PPYM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E082' TO WS-REJECT-CODE
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
       3540-ACCRUE-MARGIN.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * PPYM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO PPYM-VOID-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE VOID TIER TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3550-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-VOID-TIER-RATE(1) TO PPYM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF WS-WORK-AMT NOT < WS-VOID-TIER-LOW(WS-TX)
                   MOVE WS-VOID-TIER-RATE(WS-TX) TO PPYM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPYM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPYM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE VOID AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO STALE AMT.
      *----------------------------------------------------------------*
       3560-PRORATE-STALE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPYT-STALE-AMT ROUNDED =
                   PPYT-VOID-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PPYT-STALE-AMT
           END-IF
           ADD PPYT-STALE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT ISSUED AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3570-SPLIT-ISSUED.
           COMPUTE WS-WORK-AMT = PPYT-ISSUED-AMT / 3
           COMPUTE WS-WORK-AMT-2 = PPYT-ISSUED-AMT - (WS-WORK-AMT * 3)
           COMPUTE PPYM-ISSUED-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * IS -> PD WHEN EFFECTIVE DATE IS PAST
      * PD -> EX WHEN EFFECTIVE DATE IS PAST
      * EX -> VD WHEN EFFECTIVE DATE IS PAST
      * VD -> PR WHEN EFFECTIVE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3580-SET-TRAN.
           EVALUATE PPYT-TRAN-CD
               WHEN 'IS'
                   IF PPYM-EFFECTIVE-DATE > 0 AND PPYM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PD' TO PPYT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PD'
                   IF PPYM-EFFECTIVE-DATE > 0 AND PPYM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EX' TO PPYT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'EX'
                   IF PPYM-EFFECTIVE-DATE > 0 AND PPYM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VD' TO PPYT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VD'
                   IF PPYM-EFFECTIVE-DATE > 0 AND PPYM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PR' TO PPYT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3590-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF PPYM-EXPIRY-DATE > 0 AND PPYM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPYM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PPYT-STALE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD PPYT-STALE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD PPYT-STALE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PPYT-STALE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPYT-STALE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE PAID YTD FEE ON THE STALE AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3600-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN PPYM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO PPYM-PAID-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYM-EXEMPT-FLAG = 'Y'
                   COMPUTE PPYM-PAID-YTD ROUNDED = PPYM-STALE-AMT *
                       PPYM-MARGIN / 200
               WHEN PPYM-DECISION-CD = 'AND'
                   MOVE 3.00 TO PPYM-PAID-YTD
               WHEN PPYM-DECISION-CD = 'BER'
                   MOVE 3.00 TO PPYM-PAID-YTD
               WHEN OTHER
                   COMPUTE PPYM-PAID-YTD ROUNDED = PPYM-STALE-AMT *
                       PPYM-MARGIN / 100
           END-EVALUATE
           IF PPYM-PAID-YTD > 0 AND PPYM-PAID-YTD < 10.00
               MOVE 10.00 TO PPYM-PAID-YTD
           END-IF
           IF PPYM-PAID-YTD > 25.00
               MOVE 25.00 TO PPYM-PAID-YTD
           END-IF
           ADD PPYM-PAID-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * VOID AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3610-FLAG-VOID.
           IF PPYM-VOID-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PPYM-VOID-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3620-COUNT-NOTICE.
           IF PPYM-NOTICE-CNT < 9999999
               ADD 1 TO PPYM-NOTICE-CNT
           END-IF
           IF PPYM-NOTICE-CNT NOT < 5
               MOVE 'Y' TO PPYM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 15 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3630-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF PPYM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPYM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO PPYM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE MATCH RULE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3640-DESCRIBE-MATCH-RULE.
           EVALUATE PPYM-MATCH-RULE-CD
               WHEN 'A'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'B'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'F'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'M'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'S'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'V'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE STALE YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3650-ASSESS-STOP.
           EVALUATE TRUE
               WHEN PPYM-STOP-FLAG = 'Y'
                   MOVE ZERO TO PPYM-STALE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYT-REVERSAL-FLAG = 'Y'
                   COMPUTE PPYM-STALE-YTD ROUNDED = WS-WORK-AMT-2 *
                       PPYM-MARGIN / 200
               WHEN PPYM-CYCLE-CD = 'A'
                   MOVE 15.00 TO PPYM-STALE-YTD
               WHEN PPYM-CYCLE-CD = 'B'
                   MOVE 12.00 TO PPYM-STALE-YTD
               WHEN OTHER
                   COMPUTE PPYM-STALE-YTD ROUNDED = WS-WORK-AMT-2 *
                       PPYM-MARGIN / 100
           END-EVALUATE
           IF PPYM-STALE-YTD > 0 AND PPYM-STALE-YTD < 5.00
               MOVE 5.00 TO PPYM-STALE-YTD
           END-IF
           IF PPYM-STALE-YTD > 250.00
               MOVE 250.00 TO PPYM-STALE-YTD
           END-IF
           ADD PPYM-STALE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3660-DESCRIBE-REGION.
           EVALUATE PPYM-REGION-CD
               WHEN 'A'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'B'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'D'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'H'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'N'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'Y'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE PAID YTD FEE ON THE EXCEPT YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3670-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN PPYT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO PPYM-PAID-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYM-STOP-FLAG = 'Y'
                   COMPUTE PPYM-PAID-YTD ROUNDED = PPYM-EXCEPT-YTD *
                       PPYM-MARGIN / 200
               WHEN PPYM-STATUS-CD = 'DR'
                   MOVE 12.00 TO PPYM-PAID-YTD
               WHEN PPYM-STATUS-CD = 'TK'
                   MOVE 15.00 TO PPYM-PAID-YTD
               WHEN OTHER
                   COMPUTE PPYM-PAID-YTD ROUNDED = PPYM-EXCEPT-YTD *
                       PPYM-MARGIN / 100
           END-EVALUATE
           IF PPYM-PAID-YTD > 0 AND PPYM-PAID-YTD < 5.00
               MOVE 5.00 TO PPYM-PAID-YTD
           END-IF
           IF PPYM-PAID-YTD > 150.00
               MOVE 150.00 TO PPYM-PAID-YTD
           END-IF
           ADD PPYM-PAID-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON EXCEPT YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3680-ACCRUE-EXCEPT.
           IF PPYM-EXCEPT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPYM-EXCEPT-YTD * PPYM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO PPYM-STALE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3690-COUNT-EXCEPT.
           IF PPYM-EXCEPT-CNT < 9999999
               ADD 1 TO PPYM-EXCEPT-CNT
           END-IF
           IF PPYM-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO PPYM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3700-COUNT-EXCEPT.
           IF PPYM-EXCEPT-CNT < 9999999
               ADD 1 TO PPYM-EXCEPT-CNT
           END-IF
           IF PPYM-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO PPYM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * VOID AMT IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3710-LIMIT-VOID.
           IF PPYT-VOID-AMT < 100.00
               MOVE 100.00 TO PPYT-VOID-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PPYT-VOID-AMT > 5000.00
               MOVE 5000.00 TO PPYT-VOID-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE STALE AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3720-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPYM-STATUS-CD
                   ADD PPYT-STALE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPYM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PPYT-STALE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3730-COUNT-EXCEPT.
           IF PPYM-EXCEPT-CNT < 9999999
               ADD 1 TO PPYM-EXCEPT-CNT
           END-IF
           IF PPYM-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO PPYM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DECISION CD:
      * AND -> BER WHEN POST DATE IS PAST
      * BER -> CZW WHEN POST DATE IS PAST
      * CZW -> SBV WHEN POST DATE IS PAST
      * SBV -> ZNG WHEN POST DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3740-SET-DECISION.
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
      * ACCUMULATE WORK AMT 2 BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3750-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPYT-CHANNEL-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPYT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * VOID AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3760-FLAG-VOID.
           IF PPYM-VOID-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PPYM-VOID-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3770-CHECK-CLOSE.
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
               MOVE 'Y' TO PPYM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * VOID AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3780-FLAG-VOID.
           IF PPYT-VOID-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PPYT-VOID-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E249 REJECT CD REQUIRED
      * E224 VOID AMT NEGATIVE
      * E056 VOID AMT OVER 5000
      * E468 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3790-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPYT-REJECT-CD = SPACES
                   MOVE 'E249' TO WS-REJECT-CODE
               WHEN PPYT-VOID-AMT < 0
                   MOVE 'E224' TO WS-REJECT-CODE
               WHEN PPYT-VOID-AMT > 5000.00
                   MOVE 'E056' TO WS-REJECT-CODE
               WHEN PPYM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E468' TO WS-REJECT-CODE
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
       3800-SPLIT-ISSUED.
           COMPUTE WS-WORK-AMT = PPYM-ISSUED-YTD / 3
           COMPUTE WS-WORK-AMT-2 = PPYM-ISSUED-YTD - (WS-WORK-AMT * 3)
           COMPUTE PPYT-PAID-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE VOID AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3810-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPYM-CYCLE-CD
                   ADD PPYM-VOID-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPYM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PPYM-VOID-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR MATCH RULE CD:
      * A -> B WHEN LAST MAINT DATE IS PAST
      * B -> F WHEN LAST MAINT DATE IS PAST
      * F -> M WHEN LAST MAINT DATE IS PAST
      * M -> S WHEN LAST MAINT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3820-SET-MATCH-RULE.
           EVALUATE PPYM-MATCH-RULE-CD
               WHEN 'A'
                   IF PPYM-LAST-MAINT-DATE > 0 AND PPYM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'B' TO PPYM-MATCH-RULE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'B'
                   IF PPYM-LAST-MAINT-DATE > 0 AND PPYM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'F' TO PPYM-MATCH-RULE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'F'
                   IF PPYM-LAST-MAINT-DATE > 0 AND PPYM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'M' TO PPYM-MATCH-RULE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF PPYM-LAST-MAINT-DATE > 0 AND PPYM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'S' TO PPYM-MATCH-RULE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DECISION CD:
      * AND -> BER WHEN CLOSE DATE IS PAST
      * BER -> CZW WHEN CLOSE DATE IS PAST
      * CZW -> SBV WHEN CLOSE DATE IS PAST
      * SBV -> ZNG WHEN CLOSE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3830-SET-DECISION.
           EVALUATE PPYM-DECISION-CD
               WHEN 'AND'
                   IF PPYM-CLOSE-DATE > 0 AND PPYM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BER' TO PPYM-DECISION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BER'
                   IF PPYM-CLOSE-DATE > 0 AND PPYM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CZW' TO PPYM-DECISION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CZW'
                   IF PPYM-CLOSE-DATE > 0 AND PPYM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SBV' TO PPYM-DECISION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SBV'
                   IF PPYM-CLOSE-DATE > 0 AND PPYM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZNG' TO PPYM-DECISION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * VOID AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3840-FLAG-VOID.
           IF PPYM-VOID-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PPYM-VOID-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E625 REJECT CD REQUIRED
      * E013 PAID AMT NEGATIVE
      * E203 PAID AMT OVER 10000
      * E290 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3850-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPYT-REJECT-CD = SPACES
                   MOVE 'E625' TO WS-REJECT-CODE
               WHEN PPYM-PAID-AMT < 0
                   MOVE 'E013' TO WS-REJECT-CODE
               WHEN PPYM-PAID-AMT > 10000.00
                   MOVE 'E203' TO WS-REJECT-CODE
               WHEN PPYM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E290' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3860-COUNT-SEQ.
           IF PPYT-SEQ < 9999999
               ADD 1 TO PPYT-SEQ
           END-IF
           IF PPYT-SEQ NOT < 12
               MOVE 'Y' TO PPYM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3870-AGE-LAST-ACT.
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
               WHEN WS-DAYS NOT > 90
                   ADD PPYM-STALE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD PPYM-STALE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PPYM-STALE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPYM-STALE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON EXCEPT YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3880-ACCRUE-EXCEPT.
           IF PPYM-EXCEPT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPYM-EXCEPT-YTD * PPYM-RATE / 100 / 360
               ADD WS-WORK-AMT TO PPYT-STALE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE STALE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ISSUED AMT.
      *----------------------------------------------------------------*
       3890-PRORATE-ISSUED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPYT-ISSUED-AMT ROUNDED =
                   PPYM-STALE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PPYT-ISSUED-AMT
           END-IF
           ADD PPYT-ISSUED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ISSUED YTD IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3900-LIMIT-ISSUED.
           IF PPYM-ISSUED-YTD < 25.00
               MOVE 25.00 TO PPYM-ISSUED-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PPYM-ISSUED-YTD > 2500.00
               MOVE 2500.00 TO PPYM-ISSUED-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VOID YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3910-ACCRUE-VOID.
           IF PPYM-VOID-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPYM-VOID-YTD * PPYM-RATE / 100 / 365
               ADD WS-WORK-AMT TO PPYM-ISSUED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3920-COUNT-EXCEPT.
           IF PPYM-EXCEPT-CNT < 9999999
               ADD 1 TO PPYM-EXCEPT-CNT
           END-IF
           IF PPYM-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO PPYM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 365 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3930-CHECK-OPEN.
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
               MOVE 'Y' TO PPYM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE VOID SCHED TIER FOR THE VOID AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3940-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-VOID-SCHED-RATE(1) TO PPYM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF PPYT-VOID-AMT NOT < WS-VOID-SCHED-LOW(WS-TX)
                   MOVE WS-VOID-SCHED-RATE(WS-TX) TO PPYM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPYM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPYM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * VOID AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3950-FLAG-VOID.
           IF PPYT-VOID-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PPYT-VOID-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * A -> B WHEN LAST MAINT DATE IS PAST
      * B -> D WHEN LAST MAINT DATE IS PAST
      * D -> H WHEN LAST MAINT DATE IS PAST
      * H -> N WHEN LAST MAINT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3960-SET-REGION.
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
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3970-COUNT-EXCEPT.
           IF PPYM-EXCEPT-CNT < 9999999
               ADD 1 TO PPYM-EXCEPT-CNT
           END-IF
           IF PPYM-EXCEPT-CNT NOT < 12
               MOVE 'Y' TO PPYM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3980-COUNT-NOTICE.
           IF PPYM-NOTICE-CNT < 9999999
               ADD 1 TO PPYM-NOTICE-CNT
           END-IF
           IF PPYM-NOTICE-CNT NOT < 6
               MOVE 'Y' TO PPYM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3990-AGE-POST.
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
                   ADD PPYM-ISSUED-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PPYM-ISSUED-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD PPYM-ISSUED-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD PPYM-ISSUED-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPYM-ISSUED-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E647 OPERATOR REQUIRED
      * E725 VOID AMT NEGATIVE
      * E137 VOID AMT OVER 99999
      * E375 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       4000-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPYT-OPERATOR = SPACES
                   MOVE 'E647' TO WS-REJECT-CODE
               WHEN PPYM-VOID-AMT < 0
                   MOVE 'E725' TO WS-REJECT-CODE
               WHEN PPYM-VOID-AMT > 99999.00
                   MOVE 'E137' TO WS-REJECT-CODE
               WHEN PPYM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E375' TO WS-REJECT-CODE
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
       4010-DESCRIBE-REASON.
           EVALUATE PPYM-REASON-CD
               WHEN 'B'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'M'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'N'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'U'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'V'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'Z'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4020-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE VOID TIER TIER FOR THE STALE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4030-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-VOID-TIER-RATE(1) TO PPYM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF PPYM-STALE-AMT NOT < WS-VOID-TIER-LOW(WS-TX)
                   MOVE WS-VOID-TIER-RATE(WS-TX) TO PPYM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPYM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPYM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN NEXT REVIEW DATE IS PAST
      * BAT -> MAL WHEN NEXT REVIEW DATE IS PAST
      * MAL -> TEL WHEN NEXT REVIEW DATE IS PAST
      * TEL -> ONL WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4040-SET-CHANNEL.
           EVALUATE PPYT-CHANNEL-CD
               WHEN 'BRN'
                   IF PPYM-NEXT-REVIEW-DATE > 0 AND
                       PPYM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'BAT' TO PPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF PPYM-NEXT-REVIEW-DATE > 0 AND
                       PPYM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'MAL' TO PPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF PPYM-NEXT-REVIEW-DATE > 0 AND
                       PPYM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'TEL' TO PPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF PPYM-NEXT-REVIEW-DATE > 0 AND
                       PPYM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'ONL' TO PPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE ISSUED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO VOID AMT.
      *----------------------------------------------------------------*
       4050-PRORATE-VOID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPYM-VOID-AMT ROUNDED =
                   PPYM-ISSUED-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PPYM-VOID-AMT
           END-IF
           ADD PPYM-VOID-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E580 NAME REQUIRED
      * E475 WORK AMT 2 NEGATIVE
      * E876 WORK AMT 2 OVER 10000
      * E058 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       4060-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPYM-NAME = SPACES
                   MOVE 'E580' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E475' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 10000.00
                   MOVE 'E876' TO WS-REJECT-CODE
               WHEN PPYM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E058' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAID AMT IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4070-LIMIT-PAID.
           IF PPYM-PAID-AMT < 100.00
               MOVE 100.00 TO PPYM-PAID-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PPYM-PAID-AMT > 5000.00
               MOVE 5000.00 TO PPYM-PAID-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAID AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4080-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPYT-TRAN-CD
                   ADD PPYM-PAID-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPYT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PPYM-PAID-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE DECISION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4090-DESCRIBE-DECISION.
           EVALUATE PPYM-DECISION-CD
               WHEN 'AND'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'BER'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'CZW'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'SBV'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'ZNG'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'ZPS'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4100-DESCRIBE-REGION.
           EVALUATE PPYM-REGION-CD
               WHEN 'A'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'B'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'D'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'H'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'N'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'Y'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAID AMT IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4110-LIMIT-PAID.
           IF PPYT-PAID-AMT < 0.00
               MOVE 0.00 TO PPYT-PAID-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PPYT-PAID-AMT > 2500.00
               MOVE 2500.00 TO PPYT-PAID-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAID TIER TIER FOR THE VOID AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4120-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-PAID-TIER-RATE(1) TO PPYM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF PPYM-VOID-AMT NOT < WS-PAID-TIER-LOW(WS-TX)
                   MOVE WS-PAID-TIER-RATE(WS-TX) TO PPYM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPYM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPYM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-60, 61-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4130-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF PPYM-NEXT-REVIEW-DATE > 0 AND PPYM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PPYM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PPYT-PAID-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD PPYT-PAID-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD PPYT-PAID-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PPYT-PAID-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPYT-PAID-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * VOID AMT IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4140-LIMIT-VOID.
           IF PPYM-VOID-AMT < 25.00
               MOVE 25.00 TO PPYM-VOID-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PPYM-VOID-AMT > 500.00
               MOVE 500.00 TO PPYM-VOID-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE EXCEPT YTD FEE ON THE STALE AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4150-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN PPYM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO PPYM-EXCEPT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYM-STOP-FLAG = 'Y'
                   COMPUTE PPYM-EXCEPT-YTD ROUNDED = PPYM-STALE-AMT *
                       PPYM-MARGIN / 200
               WHEN PPYT-CHANNEL-CD = 'BRN'
                   MOVE 7.00 TO PPYM-EXCEPT-YTD
               WHEN PPYT-CHANNEL-CD = 'BAT'
                   MOVE 12.00 TO PPYM-EXCEPT-YTD
               WHEN OTHER
                   COMPUTE PPYM-EXCEPT-YTD ROUNDED = PPYM-STALE-AMT *
                       PPYM-MARGIN / 100
           END-EVALUATE
           IF PPYM-EXCEPT-YTD > 0 AND PPYM-EXCEPT-YTD < 5.00
               MOVE 5.00 TO PPYM-EXCEPT-YTD
           END-IF
           IF PPYM-EXCEPT-YTD > 25.00
               MOVE 25.00 TO PPYM-EXCEPT-YTD
           END-IF
           ADD PPYM-EXCEPT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PAID YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4160-FLAG-PAID.
           IF PPYM-PAID-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PPYM-PAID-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 60 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4170-CHECK-POST.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO PPYM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ISSUED FEE ON THE ISSUED AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4180-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN PPYM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO PPYT-ISSUED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYT-REVERSAL-FLAG = 'Y'
                   COMPUTE PPYT-ISSUED-AMT ROUNDED = PPYM-ISSUED-AMT *
                       PPYM-MARGIN / 200
               WHEN PPYT-CHANNEL-CD = 'BRN'
                   MOVE 7.00 TO PPYT-ISSUED-AMT
               WHEN PPYT-CHANNEL-CD = 'BAT'
                   MOVE 7.00 TO PPYT-ISSUED-AMT
               WHEN OTHER
                   COMPUTE PPYT-ISSUED-AMT ROUNDED = PPYM-ISSUED-AMT *
                       PPYM-MARGIN / 100
           END-EVALUATE
           IF PPYT-ISSUED-AMT > 0 AND PPYT-ISSUED-AMT < 5.00
               MOVE 5.00 TO PPYT-ISSUED-AMT
           END-IF
           IF PPYT-ISSUED-AMT > 50.00
               MOVE 50.00 TO PPYT-ISSUED-AMT
           END-IF
           ADD PPYT-ISSUED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * A -> B WHEN LAST PAY DATE IS PAST
      * B -> C WHEN LAST PAY DATE IS PAST
      * C -> R WHEN LAST PAY DATE IS PAST
      * R -> T WHEN LAST PAY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4190-SET-CYCLE.
           EVALUATE PPYM-CYCLE-CD
               WHEN 'A'
                   IF PPYM-LAST-PAY-DATE > 0 AND PPYM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'B' TO PPYM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'B'
                   IF PPYM-LAST-PAY-DATE > 0 AND PPYM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'C' TO PPYM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'C'
                   IF PPYM-LAST-PAY-DATE > 0 AND PPYM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'R' TO PPYM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF PPYM-LAST-PAY-DATE > 0 AND PPYM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO PPYM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ISSUED AMT IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4200-LIMIT-ISSUED.
           IF PPYT-ISSUED-AMT < 10.00
               MOVE 10.00 TO PPYT-ISSUED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PPYT-ISSUED-AMT > 1000.00
               MOVE 1000.00 TO PPYT-ISSUED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E677 NAME REQUIRED
      * E803 EXCEPT AMT NEGATIVE
      * E449 EXCEPT AMT OVER 25000
      * E188 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       4210-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPYM-NAME = SPACES
                   MOVE 'E677' TO WS-REJECT-CODE
               WHEN PPYM-EXCEPT-AMT < 0
                   MOVE 'E803' TO WS-REJECT-CODE
               WHEN PPYM-EXCEPT-AMT > 25000.00
                   MOVE 'E449' TO WS-REJECT-CODE
               WHEN PPYM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E188' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * VOID AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4220-FLAG-VOID.
           IF PPYM-VOID-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PPYM-VOID-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE VOID FEE ON THE WORK AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4230-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN PPYM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO PPYM-VOID-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYM-HOLD-FLAG = 'Y'
                   COMPUTE PPYM-VOID-AMT ROUNDED = WS-WORK-AMT *
                       PPYM-DISCOUNT / 200
               WHEN PPYT-CHANNEL-CD = 'BRN'
                   MOVE 7.00 TO PPYM-VOID-AMT
               WHEN PPYT-CHANNEL-CD = 'BAT'
                   MOVE 3.00 TO PPYM-VOID-AMT
               WHEN OTHER
                   COMPUTE PPYM-VOID-AMT ROUNDED = WS-WORK-AMT *
                       PPYM-DISCOUNT / 100
           END-EVALUATE
           IF PPYM-VOID-AMT > 0 AND PPYM-VOID-AMT < 5.00
               MOVE 5.00 TO PPYM-VOID-AMT
           END-IF
           IF PPYM-VOID-AMT > 35.00
               MOVE 35.00 TO PPYM-VOID-AMT
           END-IF
           ADD PPYM-VOID-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE PAID TIER TIER FOR THE PAID AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4240-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-PAID-TIER-RATE(1) TO PPYM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF PPYM-PAID-AMT NOT < WS-PAID-TIER-LOW(WS-TX)
                   MOVE WS-PAID-TIER-RATE(WS-TX) TO PPYM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPYM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPYM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       4250-COUNT-EXCEPT.
           IF PPYM-EXCEPT-CNT < 9999999
               ADD 1 TO PPYM-EXCEPT-CNT
           END-IF
           IF PPYM-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO PPYM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE MATCH RULE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4260-DESCRIBE-MATCH-RULE.
           EVALUATE PPYM-MATCH-RULE-CD
               WHEN 'A'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'B'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'F'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'M'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'S'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'V'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4270-DESCRIBE-REGION.
           EVALUATE PPYM-REGION-CD
               WHEN 'A'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'B'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'D'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'H'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'N'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'Y'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

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
                   MOVE PPELG-VOID-AMT
                     TO PPYM-VOID-AMT
                   CONTINUE
               WHEN 08
                   MOVE 'RELG' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-PPELG' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE PPY FIELDS TO PPPEN1 AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7010-CALL-PPPEN1.
           INITIALIZE PPPEN1-PARMS
           MOVE PPYM-ISSUED-AMT
             TO PPPEN1-ISSUED-AMT
           MOVE PPYM-VOID-AMT
             TO PPPEN1-VOID-AMT
           MOVE PPYM-STALE-YTD
             TO PPPEN1-STALE-YTD
           MOVE PPYM-EXCEPT-AMT
             TO PPPEN1-EXCEPT-AMT
           MOVE PPYM-PAID-AMT
             TO PPPEN1-PAID-AMT
           MOVE PPYM-PAID-YTD
             TO PPPEN1-PAID-YTD
           MOVE PPYM-CLOSE-DATE
             TO PPPEN1-CLOSE-DATE
           MOVE PPYM-LAST-MAINT-DATE
             TO PPPEN1-LAST-MAINT-DATE
           MOVE PPYM-EXPIRY-DATE
             TO PPPEN1-EXPIRY-DATE
           MOVE PPYM-OPEN-DATE
             TO PPPEN1-OPEN-DATE
           MOVE PPYM-DECISION-CD
             TO PPPEN1-DECISION-CD
           MOVE PPYM-REASON-CD
             TO PPPEN1-REASON-CD
           MOVE PPYM-REGION-CD
             TO PPPEN1-REGION-CD
           MOVE PPYM-CYCLE-CD
             TO PPPEN1-CYCLE-CD
           MOVE PPYM-HOLD-FLAG
             TO PPPEN1-HOLD-FLAG
           MOVE PPYM-OVERRIDE-FLAG
             TO PPPEN1-OVERRIDE-FLAG
           MOVE PPYM-NOTICE-SENT-FLAG
             TO PPPEN1-NOTICE-SENT-FLAG
           MOVE PPYM-CYCLE-CNT
             TO PPPEN1-CYCLE-CNT
           MOVE PPYM-PAYMENT-CNT
             TO PPPEN1-PAYMENT-CNT
           MOVE PPYM-DISCOUNT
             TO PPPEN1-DISCOUNT
           MOVE PPYM-RATE
             TO PPPEN1-RATE
           MOVE PPYM-DESC
             TO PPPEN1-DESC
           MOVE PPYM-CUST-ID
             TO PPPEN1-CUST-ID
           CALL 'PPPEN1' USING PPPEN1-PARMS
           EVALUATE PPPEN1-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE PPPEN1-VOID-AMT
                     TO PPYM-VOID-AMT
                   MOVE PPPEN1-STALE-YTD
                     TO PPYM-STALE-YTD
                   MOVE PPPEN1-PAID-YTD
                     TO PPYM-PAID-YTD
                   MOVE PPPEN1-HOLD-FLAG
                     TO PPYM-HOLD-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'REN1' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-PPPEN1' TO ABND-PARAGRAPH
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
           DISPLAY 'PPYEXCP READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'PPYEXCP' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
