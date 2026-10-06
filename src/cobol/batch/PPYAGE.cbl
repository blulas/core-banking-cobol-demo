      *================================================================*
      * PROGRAM  : PPYAGE                                              *
      * TYPE     : BATCH                                               *
      * PURPOSE  : POSITIVE PAY - AGING. READS THE PPY TRANSACTIONS,   *
      *            UPDATES THE PPY MASTER AND PRODUCES THE CONTROL     *
      *            REPORT. CORPORATE POSITIVE PAY ISSUE FILE MATCHING  *
      *            AND EXCEPTIONS                                      *
      * INPUT    : PPYMST (KSDS), PPYTRN, BANKCTL                      *
      * OUTPUT   : PPYOUT, PPYAGRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, PPLIM, PPFEE            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-11-17 LMB  ORIGINAL                                       *
      * 1991-01-27 DPS  EXCEPT EXCEPTIONS TO OPS QUEUE CHG6106         *
      * 1992-09-05 GAF  EXCEPT STATUS CODES ADDED INC7017              *
      * 1993-02-23 EWS  NEW EXCEPT REPORT COLUMNS REQ7950              *
      * 1995-05-15 BHS  PERFORMANCE - VOID READ AHEAD INC1575          *
      * 2000-07-27 PQR  EXCEPT CUTOFF MOVED PRB6249                    *
      * 2001-11-19 TWH  PAID EXCEPTIONS TO OPS QUEUE PRB5321           *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PPYAGE.
       AUTHOR. C R PATEL.

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
           SELECT REPORT-FILE ASSIGN TO "PPYAGRPT"
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
                                           VALUE 22.000000.
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
      * WS-VOID-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-VOID-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000303500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000604000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001050000000143500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011050000000884000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011150000000860000'.
       01  WS-VOID-TIER-TBL REDEFINES WS-VOID-TIER-VALUES.
           05  WS-VOID-TIER-ROW OCCURS 5 TIMES.
               10  WS-VOID-TIER-LOW        PIC 9(09)V99.
               10  WS-VOID-TIER-RATE       PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-ISSUED-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-ISSUED-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000179000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000390500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007500000000621500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017500000000546000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022500000000897000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022550000000758000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022650000000437500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022900000000188000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00027900000000539000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00030400000000709000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00032900000000023000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00032950000000411000'.
       01  WS-ISSUED-BAND-TBL REDEFINES WS-ISSUED-BAND-VALUES.
           05  WS-ISSUED-BAND-ROW OCCURS 12 TIMES.
               10  WS-ISSUED-BAND-LOW      PIC 9(09)V99.
               10  WS-ISSUED-BAND-RATE     PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  PPYAGE  POSITIVE PAY'.
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
       COPY PPLIMP.
       COPY PPFEEP.

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
               PERFORM 3000-PRORATE-VOID
               PERFORM 3010-ACCRUE-MARGIN
               PERFORM 3020-ACCRUE-VOID
               PERFORM 3030-FLAG-STALE
               PERFORM 3040-SET-CHANNEL
               PERFORM 3050-FLAG-STALE
               PERFORM 3060-CHECK-EFF
               PERFORM 3070-LIMIT-STALE
               PERFORM 3080-ACCUM-DECISION
               PERFORM 3090-AGE-LAST-MAINT
               PERFORM 3100-EDIT-ADDR
               PERFORM 3110-COUNT-CYCLE
               PERFORM 3120-AGE-LAST-ACT
               PERFORM 3130-DESCRIBE-STATUS
               PERFORM 3140-DESCRIBE-CYCLE
               PERFORM 3150-PRORATE-VOID
               PERFORM 3160-SPLIT-PAID
               PERFORM 3170-SPLIT-PAID
               PERFORM 3180-SPLIT-VOID
               PERFORM 3190-ACCUM-CHANNEL
               PERFORM 3200-AGE-EFF
               PERFORM 3210-EDIT-DESC
               PERFORM 3220-EDIT-MEMO
               PERFORM 3230-DESCRIBE-STATUS
               PERFORM 3240-AGE-EFFECTIVE
               PERFORM 3250-LIMIT-ITEM
               PERFORM 3260-SPLIT-EXCEPT
               PERFORM 3270-SELECT-DISCOUNT
               PERFORM 3280-ACCRUE-MARGIN
               PERFORM 3290-PRORATE-PAID
               PERFORM 3300-PRORATE-ISSUED
               PERFORM 3310-SELECT-DISCOUNT
               PERFORM 3320-COUNT-PAYMENT
               PERFORM 3330-COUNT-PAYMENT
               PERFORM 3340-CHECK-EFFECTIVE
               PERFORM 3350-EDIT-NAME
               PERFORM 3360-SPLIT-VOID
               PERFORM 3370-ASSESS-OVERRIDE
               PERFORM 3380-LIMIT-EXCEPT
               PERFORM 3390-SPLIT-PAID
               PERFORM 3400-PRORATE-STALE
               PERFORM 3410-PRORATE-VOID
               PERFORM 3420-ASSESS-HOLD
               PERFORM 3430-PRORATE-PAID
               PERFORM 3440-LIMIT-ITEM
               PERFORM 3450-SET-REASON
               PERFORM 3460-SPLIT-EXCEPT
               PERFORM 3470-CHECK-NEXT-DUE
               PERFORM 3480-COUNT-PAYMENT
               PERFORM 3490-DESCRIBE-REGION
               PERFORM 3500-LIMIT-EXCEPT
               PERFORM 3510-ASSESS-REVERSAL
               PERFORM 3520-AGE-CLOSE
               PERFORM 3530-ASSESS-NOTICE-SENT
               PERFORM 3540-ACCUM-CYCLE
               PERFORM 3550-DESCRIBE-CHANNEL
               PERFORM 3560-LIMIT-VOID
               PERFORM 3570-COUNT-SEQ
               PERFORM 3580-ASSESS-NOTICE-SENT
               PERFORM 3590-PRORATE-VOID
               PERFORM 3600-ACCRUE-VOID
               PERFORM 3610-CHECK-EFFECTIVE
               PERFORM 3620-SET-REASON
               PERFORM 3630-AGE-LAST-PAY
               PERFORM 3640-SET-CHANNEL
               PERFORM 3650-PRORATE-VOID
               PERFORM 3660-ASSESS-EXEMPT
               PERFORM 3670-ASSESS-HOLD
               PERFORM 3680-CHECK-OPEN
               PERFORM 3690-ASSESS-EXEMPT
               PERFORM 3700-SELECT-MARGIN
               PERFORM 3710-FLAG-ISSUED
               PERFORM 3720-ASSESS-OVERRIDE
               PERFORM 3730-PRORATE-STALE
               PERFORM 3740-EDIT-ADDR
               PERFORM 3750-PRORATE-ISSUED
               PERFORM 3760-AGE-LAST-MAINT
               PERFORM 3770-PRORATE-PAID
               PERFORM 3780-LIMIT-VOID
               PERFORM 3790-EDIT-REJECT
               PERFORM 3800-EDIT-REF-NO
               PERFORM 3810-CHECK-EFF
               PERFORM 3820-EDIT-MEMO
               PERFORM 3830-AGE-LAST-MAINT
               PERFORM 3840-EDIT-DESC
               PERFORM 3850-PRORATE-ISSUED
               PERFORM 3860-PRORATE-EXCEPT
               PERFORM 3870-SET-DECISION
               PERFORM 3880-EDIT-ADDR
               PERFORM 3890-ACCUM-MATCH-RULE
               PERFORM 3900-CHECK-OPEN
               PERFORM 3910-LIMIT-VOID
               PERFORM 3920-ACCUM-DECISION
               PERFORM 3930-SELECT-MARGIN
               PERFORM 3940-SELECT-DISCOUNT
               PERFORM 3950-PRORATE-ISSUED
               PERFORM 3960-SPLIT-EXCEPT
               PERFORM 3970-COUNT-CYCLE
               PERFORM 3980-DESCRIBE-REGION
               PERFORM 3990-EDIT-REF-NO
               PERFORM 4000-LIMIT-VOID
               PERFORM 4010-FLAG-ITEM
               PERFORM 4020-EDIT-CONTACT
               PERFORM 7000-CALL-PPLIM
               PERFORM 7040-CALL-PPFEE
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
      * PRORATE EXCEPT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO VOID YTD.
      *----------------------------------------------------------------*
       3000-PRORATE-VOID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPYM-VOID-YTD ROUNDED =
                   PPYM-EXCEPT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PPYM-VOID-YTD
           END-IF
           ADD PPYM-VOID-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3010-ACCRUE-MARGIN.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * PPYM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO PPYM-VOID-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VOID AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3020-ACCRUE-VOID.
           IF PPYT-VOID-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPYT-VOID-AMT * PPYM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO PPYM-VOID-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STALE YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3030-FLAG-STALE.
           IF PPYM-STALE-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PPYM-STALE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN POST DATE IS PAST
      * BAT -> MAL WHEN POST DATE IS PAST
      * MAL -> TEL WHEN POST DATE IS PAST
      * TEL -> ONL WHEN POST DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3040-SET-CHANNEL.
           EVALUATE PPYT-CHANNEL-CD
               WHEN 'BRN'
                   IF PPYT-POST-DATE > 0 AND PPYT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO PPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF PPYT-POST-DATE > 0 AND PPYT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO PPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF PPYT-POST-DATE > 0 AND PPYT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO PPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF PPYT-POST-DATE > 0 AND PPYT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO PPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STALE AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3050-FLAG-STALE.
           IF PPYM-STALE-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PPYM-STALE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 10 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3060-CHECK-EFF.
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
               MOVE 'Y' TO PPYM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STALE YTD IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3070-LIMIT-STALE.
           IF PPYM-STALE-YTD < 50.00
               MOVE 50.00 TO PPYM-STALE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PPYM-STALE-YTD > 1000.00
               MOVE 1000.00 TO PPYM-STALE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE STALE AMT BY DECISION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3080-ACCUM-DECISION.
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
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3090-AGE-LAST-MAINT.
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
                   ADD PPYM-EXCEPT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD PPYM-EXCEPT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD PPYM-EXCEPT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PPYM-EXCEPT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPYM-EXCEPT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E128 ADDR REQUIRED
      * E262 PAID YTD NEGATIVE
      * E902 PAID YTD OVER 250000
      * E408 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3100-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPYM-ADDR = SPACES
                   MOVE 'E128' TO WS-REJECT-CODE
               WHEN PPYM-PAID-YTD < 0
                   MOVE 'E262' TO WS-REJECT-CODE
               WHEN PPYM-PAID-YTD > 250000.00
                   MOVE 'E902' TO WS-REJECT-CODE
               WHEN PPYM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E408' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3110-COUNT-CYCLE.
           IF PPYM-CYCLE-CNT < 9999999
               ADD 1 TO PPYM-CYCLE-CNT
           END-IF
           IF PPYM-CYCLE-CNT NOT < 3
               MOVE 'Y' TO PPYM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3120-AGE-LAST-ACT.
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
                   ADD PPYM-STALE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD PPYM-STALE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD PPYM-STALE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PPYM-STALE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPYM-STALE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3130-DESCRIBE-STATUS.
           EVALUATE PPYM-STATUS-CD
               WHEN 'DR'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'TK'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'WX'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3140-DESCRIBE-CYCLE.
           EVALUATE PPYM-CYCLE-CD
               WHEN 'A'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'B'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'C'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'R'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'T'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'Y'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE STALE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO VOID AMT.
      *----------------------------------------------------------------*
       3150-PRORATE-VOID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPYM-VOID-AMT ROUNDED =
                   PPYM-STALE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PPYM-VOID-AMT
           END-IF
           ADD PPYM-VOID-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT PAID YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3160-SPLIT-PAID.
           COMPUTE WS-WORK-AMT = PPYM-PAID-YTD / 2
           COMPUTE WS-WORK-AMT-2 = PPYM-PAID-YTD - (WS-WORK-AMT * 2)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PAID AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3170-SPLIT-PAID.
           COMPUTE WS-WORK-AMT = PPYM-PAID-AMT / 12
           COMPUTE WS-WORK-AMT-2 = PPYM-PAID-AMT - (WS-WORK-AMT * 12)
           COMPUTE PPYM-ISSUED-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT VOID AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3180-SPLIT-VOID.
           COMPUTE WS-WORK-AMT = PPYM-VOID-AMT / 12
           COMPUTE WS-WORK-AMT-2 = PPYM-VOID-AMT - (WS-WORK-AMT * 12)
           COMPUTE PPYT-STALE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE STALE AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3190-ACCUM-CHANNEL.
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
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3200-AGE-EFF.
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
               WHEN WS-DAYS NOT > 15
                   ADD PPYM-STALE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD PPYM-STALE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD PPYM-STALE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPYM-STALE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E840 DESC REQUIRED
      * E908 WORK AMT NEGATIVE
      * E581 WORK AMT OVER 10000
      * E467 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3210-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPYM-DESC = SPACES
                   MOVE 'E840' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E908' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 10000.00
                   MOVE 'E581' TO WS-REJECT-CODE
               WHEN PPYM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E467' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E615 MEMO REQUIRED
      * E147 WORK AMT 2 NEGATIVE
      * E369 WORK AMT 2 OVER 50000
      * E127 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3220-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPYT-MEMO = SPACES
                   MOVE 'E615' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E147' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 50000.00
                   MOVE 'E369' TO WS-REJECT-CODE
               WHEN PPYT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E127' TO WS-REJECT-CODE
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
       3230-DESCRIBE-STATUS.
           EVALUATE PPYM-STATUS-CD
               WHEN 'DR'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'TK'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'WX'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3240-AGE-EFFECTIVE.
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
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3250-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 50.00
               MOVE 50.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 500.00
               MOVE 500.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT EXCEPT AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3260-SPLIT-EXCEPT.
           COMPUTE WS-WORK-AMT = PPYM-EXCEPT-AMT / 4
           COMPUTE WS-WORK-AMT-2 = PPYM-EXCEPT-AMT - (WS-WORK-AMT * 4)
           COMPUTE PPYT-ISSUED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE VOID TIER TIER FOR THE STALE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3270-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-VOID-TIER-RATE(1) TO PPYM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF PPYT-STALE-AMT NOT < WS-VOID-TIER-LOW(WS-TX)
                   MOVE WS-VOID-TIER-RATE(WS-TX) TO PPYM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPYM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPYM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3280-ACCRUE-MARGIN.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * PPYM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO PPYT-PAID-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ISSUED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAID YTD.
      *----------------------------------------------------------------*
       3290-PRORATE-PAID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPYM-PAID-YTD ROUNDED =
                   PPYM-ISSUED-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PPYM-PAID-YTD
           END-IF
           ADD PPYM-PAID-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE PAID AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ISSUED AMT.
      *----------------------------------------------------------------*
       3300-PRORATE-ISSUED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPYM-ISSUED-AMT ROUNDED =
                   PPYM-PAID-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PPYM-ISSUED-AMT
           END-IF
           ADD PPYM-ISSUED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE VOID TIER TIER FOR THE VOID AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3310-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-VOID-TIER-RATE(1) TO PPYM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF PPYT-VOID-AMT NOT < WS-VOID-TIER-LOW(WS-TX)
                   MOVE WS-VOID-TIER-RATE(WS-TX) TO PPYM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPYM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPYM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3320-COUNT-PAYMENT.
           IF PPYM-PAYMENT-CNT < 9999999
               ADD 1 TO PPYM-PAYMENT-CNT
           END-IF
           IF PPYM-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO PPYM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3330-COUNT-PAYMENT.
           IF PPYM-PAYMENT-CNT < 9999999
               ADD 1 TO PPYM-PAYMENT-CNT
           END-IF
           IF PPYM-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO PPYM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3340-CHECK-EFFECTIVE.
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
           IF WS-DAYS > 120
               MOVE 'Y' TO PPYM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E306 NAME REQUIRED
      * E287 ISSUED AMT NEGATIVE
      * E967 ISSUED AMT OVER 50000
      * E913 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3350-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPYM-NAME = SPACES
                   MOVE 'E306' TO WS-REJECT-CODE
               WHEN PPYT-ISSUED-AMT < 0
                   MOVE 'E287' TO WS-REJECT-CODE
               WHEN PPYT-ISSUED-AMT > 50000.00
                   MOVE 'E967' TO WS-REJECT-CODE
               WHEN PPYM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E913' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT VOID YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3360-SPLIT-VOID.
           COMPUTE WS-WORK-AMT = PPYM-VOID-YTD / 3
           COMPUTE WS-WORK-AMT-2 = PPYM-VOID-YTD - (WS-WORK-AMT * 3)
           COMPUTE PPYM-ISSUED-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE VOID FEE ON THE ISSUED AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3370-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN PPYM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO PPYM-VOID-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE PPYM-VOID-AMT ROUNDED = PPYT-ISSUED-AMT *
                       PPYM-RATE / 200
               WHEN PPYM-STATUS-CD = 'DR'
                   MOVE 7.00 TO PPYM-VOID-AMT
               WHEN PPYM-STATUS-CD = 'TK'
                   MOVE 15.00 TO PPYM-VOID-AMT
               WHEN OTHER
                   COMPUTE PPYM-VOID-AMT ROUNDED = PPYT-ISSUED-AMT *
                       PPYM-RATE / 100
           END-EVALUATE
           IF PPYM-VOID-AMT > 0 AND PPYM-VOID-AMT < 10.00
               MOVE 10.00 TO PPYM-VOID-AMT
           END-IF
           IF PPYM-VOID-AMT > 35.00
               MOVE 35.00 TO PPYM-VOID-AMT
           END-IF
           ADD PPYM-VOID-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * EXCEPT YTD IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3380-LIMIT-EXCEPT.
           IF PPYM-EXCEPT-YTD < 25.00
               MOVE 25.00 TO PPYM-EXCEPT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PPYM-EXCEPT-YTD > 50000.00
               MOVE 50000.00 TO PPYM-EXCEPT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3390-SPLIT-PAID.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 3)
           COMPUTE PPYM-PAID-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE EXCEPT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO STALE AMT.
      *----------------------------------------------------------------*
       3400-PRORATE-STALE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPYT-STALE-AMT ROUNDED =
                   PPYM-EXCEPT-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PPYT-STALE-AMT
           END-IF
           ADD PPYT-STALE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO VOID AMT.
      *----------------------------------------------------------------*
       3410-PRORATE-VOID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPYT-VOID-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PPYT-VOID-AMT
           END-IF
           ADD PPYT-VOID-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE ISSUED AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3420-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN PPYM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYM-STOP-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = PPYT-ISSUED-AMT *
                       PPYM-RATE / 200
               WHEN PPYM-REGION-CD = 'A'
                   MOVE 10.00 TO WS-WORK-AMT-2
               WHEN PPYM-REGION-CD = 'B'
                   MOVE 3.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = PPYT-ISSUED-AMT *
                       PPYM-RATE / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 2.00
               MOVE 2.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE VOID AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PAID AMT.
      *----------------------------------------------------------------*
       3430-PRORATE-PAID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPYM-PAID-AMT ROUNDED =
                   PPYM-VOID-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PPYM-PAID-AMT
           END-IF
           ADD PPYM-PAID-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3440-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 1000.00
               MOVE 1000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * B -> M WHEN EFFECTIVE DATE IS PAST
      * M -> N WHEN EFFECTIVE DATE IS PAST
      * N -> U WHEN EFFECTIVE DATE IS PAST
      * U -> V WHEN EFFECTIVE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3450-SET-REASON.
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
      * SPLIT EXCEPT AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3460-SPLIT-EXCEPT.
           COMPUTE WS-WORK-AMT = PPYM-EXCEPT-AMT / 4
           COMPUTE WS-WORK-AMT-2 = PPYM-EXCEPT-AMT - (WS-WORK-AMT * 4)
           COMPUTE PPYM-STALE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3470-CHECK-NEXT-DUE.
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
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3480-COUNT-PAYMENT.
           IF PPYM-PAYMENT-CNT < 9999999
               ADD 1 TO PPYM-PAYMENT-CNT
           END-IF
           IF PPYM-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO PPYM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3490-DESCRIBE-REGION.
           EVALUATE PPYM-REGION-CD
               WHEN 'A'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'B'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'D'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'H'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'N'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'Y'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT YTD IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3500-LIMIT-EXCEPT.
           IF PPYM-EXCEPT-YTD < 25.00
               MOVE 25.00 TO PPYM-EXCEPT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PPYM-EXCEPT-YTD > 500.00
               MOVE 500.00 TO PPYM-EXCEPT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE STALE FEE ON THE ISSUED AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3510-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN PPYT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO PPYT-STALE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE PPYT-STALE-AMT ROUNDED = PPYM-ISSUED-AMT *
                       PPYM-MARGIN / 200
               WHEN PPYT-TRAN-CD = 'IS'
                   MOVE 12.00 TO PPYT-STALE-AMT
               WHEN PPYT-TRAN-CD = 'PD'
                   MOVE 15.00 TO PPYT-STALE-AMT
               WHEN OTHER
                   COMPUTE PPYT-STALE-AMT ROUNDED = PPYM-ISSUED-AMT *
                       PPYM-MARGIN / 100
           END-EVALUATE
           IF PPYT-STALE-AMT > 0 AND PPYT-STALE-AMT < 1.00
               MOVE 1.00 TO PPYT-STALE-AMT
           END-IF
           IF PPYT-STALE-AMT > 150.00
               MOVE 150.00 TO PPYT-STALE-AMT
           END-IF
           ADD PPYT-STALE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-90, 91-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3520-AGE-CLOSE.
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
                   ADD PPYT-VOID-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD PPYT-VOID-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD PPYT-VOID-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PPYT-VOID-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPYT-VOID-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE STALE YTD FEE ON THE STALE AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3530-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN PPYM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO PPYM-STALE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYM-OVERRIDE-FLAG = 'Y'
                   COMPUTE PPYM-STALE-YTD ROUNDED = PPYT-STALE-AMT *
                       PPYM-MARGIN / 200
               WHEN PPYM-STATUS-CD = 'DR'
                   MOVE 7.00 TO PPYM-STALE-YTD
               WHEN PPYM-STATUS-CD = 'TK'
                   MOVE 10.00 TO PPYM-STALE-YTD
               WHEN OTHER
                   COMPUTE PPYM-STALE-YTD ROUNDED = PPYT-STALE-AMT *
                       PPYM-MARGIN / 100
           END-EVALUATE
           IF PPYM-STALE-YTD > 0 AND PPYM-STALE-YTD < 2.00
               MOVE 2.00 TO PPYM-STALE-YTD
           END-IF
           IF PPYM-STALE-YTD > 75.00
               MOVE 75.00 TO PPYM-STALE-YTD
           END-IF
           ADD PPYM-STALE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE STALE YTD BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3540-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPYM-CYCLE-CD
                   ADD PPYM-STALE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPYM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PPYM-STALE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3550-DESCRIBE-CHANNEL.
           EVALUATE PPYT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * VOID YTD IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3560-LIMIT-VOID.
           IF PPYM-VOID-YTD < 25.00
               MOVE 25.00 TO PPYM-VOID-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PPYM-VOID-YTD > 500.00
               MOVE 500.00 TO PPYM-VOID-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3570-COUNT-SEQ.
           IF PPYT-SEQ < 9999999
               ADD 1 TO PPYT-SEQ
           END-IF
           IF PPYT-SEQ NOT < 3
               MOVE 'Y' TO PPYM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE EXCEPT YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3580-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN PPYM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO PPYM-EXCEPT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYM-EXEMPT-FLAG = 'Y'
                   COMPUTE PPYM-EXCEPT-YTD ROUNDED = WS-WORK-AMT-2 *
                       PPYM-DISCOUNT / 200
               WHEN PPYM-CYCLE-CD = 'A'
                   MOVE 3.00 TO PPYM-EXCEPT-YTD
               WHEN PPYM-CYCLE-CD = 'B'
                   MOVE 3.00 TO PPYM-EXCEPT-YTD
               WHEN OTHER
                   COMPUTE PPYM-EXCEPT-YTD ROUNDED = WS-WORK-AMT-2 *
                       PPYM-DISCOUNT / 100
           END-EVALUATE
           IF PPYM-EXCEPT-YTD > 0 AND PPYM-EXCEPT-YTD < 1.00
               MOVE 1.00 TO PPYM-EXCEPT-YTD
           END-IF
           IF PPYM-EXCEPT-YTD > 150.00
               MOVE 150.00 TO PPYM-EXCEPT-YTD
           END-IF
           ADD PPYM-EXCEPT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE PAID YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO VOID YTD.
      *----------------------------------------------------------------*
       3590-PRORATE-VOID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPYM-VOID-YTD ROUNDED =
                   PPYM-PAID-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PPYM-VOID-YTD
           END-IF
           ADD PPYM-VOID-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VOID AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3600-ACCRUE-VOID.
           IF PPYT-VOID-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PPYT-VOID-AMT * PPYM-RATE / 100 / 360
               ADD WS-WORK-AMT TO PPYM-VOID-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3610-CHECK-EFFECTIVE.
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
           IF WS-DAYS > 120
               MOVE 'Y' TO PPYM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * B -> M WHEN LAST PAY DATE IS PAST
      * M -> N WHEN LAST PAY DATE IS PAST
      * N -> U WHEN LAST PAY DATE IS PAST
      * U -> V WHEN LAST PAY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3620-SET-REASON.
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
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3630-AGE-LAST-PAY.
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
               WHEN WS-DAYS NOT > 45
                   ADD PPYM-STALE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD PPYM-STALE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD PPYM-STALE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPYM-STALE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN EFF DATE IS PAST
      * BAT -> MAL WHEN EFF DATE IS PAST
      * MAL -> TEL WHEN EFF DATE IS PAST
      * TEL -> ONL WHEN EFF DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3640-SET-CHANNEL.
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
      * PRORATE ISSUED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO VOID AMT.
      *----------------------------------------------------------------*
       3650-PRORATE-VOID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPYM-VOID-AMT ROUNDED =
                   PPYT-ISSUED-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PPYM-VOID-AMT
           END-IF
           ADD PPYM-VOID-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE STALE AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3660-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN PPYM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = PPYM-STALE-AMT *
                       PPYM-MARGIN / 200
               WHEN PPYM-CYCLE-CD = 'A'
                   MOVE 3.00 TO WS-WORK-AMT-2
               WHEN PPYM-CYCLE-CD = 'B'
                   MOVE 15.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = PPYM-STALE-AMT *
                       PPYM-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 5.00
               MOVE 5.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 150.00
               MOVE 150.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE STALE AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3670-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN PPYM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = PPYM-STALE-AMT *
                       PPYM-MARGIN / 200
               WHEN PPYM-STATUS-CD = 'DR'
                   MOVE 12.00 TO WS-WORK-AMT-2
               WHEN PPYM-STATUS-CD = 'TK'
                   MOVE 5.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = PPYM-STALE-AMT *
                       PPYM-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 10.00
               MOVE 10.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 25.00
               MOVE 25.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 120 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3680-CHECK-OPEN.
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
           IF WS-DAYS > 120
               MOVE 'Y' TO PPYT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE EXCEPT YTD FEE ON THE STALE AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3690-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN PPYM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO PPYM-EXCEPT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYT-REVERSAL-FLAG = 'Y'
                   COMPUTE PPYM-EXCEPT-YTD ROUNDED = PPYT-STALE-AMT *
                       PPYM-DISCOUNT / 200
               WHEN PPYM-REGION-CD = 'A'
                   MOVE 15.00 TO PPYM-EXCEPT-YTD
               WHEN PPYM-REGION-CD = 'B'
                   MOVE 20.00 TO PPYM-EXCEPT-YTD
               WHEN OTHER
                   COMPUTE PPYM-EXCEPT-YTD ROUNDED = PPYT-STALE-AMT *
                       PPYM-DISCOUNT / 100
           END-EVALUATE
           IF PPYM-EXCEPT-YTD > 0 AND PPYM-EXCEPT-YTD < 2.00
               MOVE 2.00 TO PPYM-EXCEPT-YTD
           END-IF
           IF PPYM-EXCEPT-YTD > 150.00
               MOVE 150.00 TO PPYM-EXCEPT-YTD
           END-IF
           ADD PPYM-EXCEPT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE VOID TIER TIER FOR THE VOID AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3700-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-VOID-TIER-RATE(1) TO PPYM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF PPYM-VOID-AMT NOT < WS-VOID-TIER-LOW(WS-TX)
                   MOVE WS-VOID-TIER-RATE(WS-TX) TO PPYM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPYM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPYM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ISSUED AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3710-FLAG-ISSUED.
           IF PPYT-ISSUED-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PPYT-ISSUED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ISSUED YTD FEE ON THE VOID AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3720-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN PPYM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO PPYM-ISSUED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PPYT-REVERSAL-FLAG = 'Y'
                   COMPUTE PPYM-ISSUED-YTD ROUNDED = PPYT-VOID-AMT *
                       PPYM-MARGIN / 200
               WHEN PPYT-CHANNEL-CD = 'BRN'
                   MOVE 10.00 TO PPYM-ISSUED-YTD
               WHEN PPYT-CHANNEL-CD = 'BAT'
                   MOVE 15.00 TO PPYM-ISSUED-YTD
               WHEN OTHER
                   COMPUTE PPYM-ISSUED-YTD ROUNDED = PPYT-VOID-AMT *
                       PPYM-MARGIN / 100
           END-EVALUATE
           IF PPYM-ISSUED-YTD > 0 AND PPYM-ISSUED-YTD < 5.00
               MOVE 5.00 TO PPYM-ISSUED-YTD
           END-IF
           IF PPYM-ISSUED-YTD > 75.00
               MOVE 75.00 TO PPYM-ISSUED-YTD
           END-IF
           ADD PPYM-ISSUED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE ISSUED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO STALE YTD.
      *----------------------------------------------------------------*
       3730-PRORATE-STALE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPYM-STALE-YTD ROUNDED =
                   PPYM-ISSUED-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PPYM-STALE-YTD
           END-IF
           ADD PPYM-STALE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E775 ADDR REQUIRED
      * E208 EXCEPT AMT NEGATIVE
      * E368 EXCEPT AMT OVER 250000
      * E512 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3740-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPYM-ADDR = SPACES
                   MOVE 'E775' TO WS-REJECT-CODE
               WHEN PPYM-EXCEPT-AMT < 0
                   MOVE 'E208' TO WS-REJECT-CODE
               WHEN PPYM-EXCEPT-AMT > 250000.00
                   MOVE 'E368' TO WS-REJECT-CODE
               WHEN PPYM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E512' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE VOID YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ISSUED AMT.
      *----------------------------------------------------------------*
       3750-PRORATE-ISSUED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPYT-ISSUED-AMT ROUNDED =
                   PPYM-VOID-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PPYT-ISSUED-AMT
           END-IF
           ADD PPYT-ISSUED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3760-AGE-LAST-MAINT.
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
                   ADD PPYM-PAID-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD PPYM-PAID-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD PPYM-PAID-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PPYM-PAID-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPYM-PAID-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE STALE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PAID AMT.
      *----------------------------------------------------------------*
       3770-PRORATE-PAID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPYT-PAID-AMT ROUNDED =
                   PPYM-STALE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PPYT-PAID-AMT
           END-IF
           ADD PPYT-PAID-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * VOID YTD IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3780-LIMIT-VOID.
           IF PPYM-VOID-YTD < 50.00
               MOVE 50.00 TO PPYM-VOID-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PPYM-VOID-YTD > 2500.00
               MOVE 2500.00 TO PPYM-VOID-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E436 REJECT CD REQUIRED
      * E497 PAID YTD NEGATIVE
      * E500 PAID YTD OVER 25000
      * E271 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3790-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPYT-REJECT-CD = SPACES
                   MOVE 'E436' TO WS-REJECT-CODE
               WHEN PPYM-PAID-YTD < 0
                   MOVE 'E497' TO WS-REJECT-CODE
               WHEN PPYM-PAID-YTD > 25000.00
                   MOVE 'E500' TO WS-REJECT-CODE
               WHEN PPYM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E271' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E333 REF NO REQUIRED
      * E103 VOID AMT NEGATIVE
      * E847 VOID AMT OVER 25000
      * E305 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3800-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPYM-REF-NO = SPACES
                   MOVE 'E333' TO WS-REJECT-CODE
               WHEN PPYM-VOID-AMT < 0
                   MOVE 'E103' TO WS-REJECT-CODE
               WHEN PPYM-VOID-AMT > 25000.00
                   MOVE 'E847' TO WS-REJECT-CODE
               WHEN PPYM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E305' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 90 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3810-CHECK-EFF.
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
           IF WS-DAYS > 90
               MOVE 'Y' TO PPYM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E119 MEMO REQUIRED
      * E403 PAID YTD NEGATIVE
      * E235 PAID YTD OVER 99999
      * E791 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3820-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPYT-MEMO = SPACES
                   MOVE 'E119' TO WS-REJECT-CODE
               WHEN PPYM-PAID-YTD < 0
                   MOVE 'E403' TO WS-REJECT-CODE
               WHEN PPYM-PAID-YTD > 99999.00
                   MOVE 'E235' TO WS-REJECT-CODE
               WHEN PPYM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E791' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3830-AGE-LAST-MAINT.
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
               WHEN WS-DAYS NOT > 30
                   ADD PPYM-ISSUED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD PPYM-ISSUED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PPYM-ISSUED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PPYM-ISSUED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E578 DESC REQUIRED
      * E545 WORK AMT NEGATIVE
      * E139 WORK AMT OVER 25000
      * E685 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3840-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPYM-DESC = SPACES
                   MOVE 'E578' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E545' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 25000.00
                   MOVE 'E139' TO WS-REJECT-CODE
               WHEN PPYM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E685' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ISSUED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3850-PRORATE-ISSUED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   PPYM-ISSUED-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE VOID AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO EXCEPT AMT.
      *----------------------------------------------------------------*
       3860-PRORATE-EXCEPT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPYM-EXCEPT-AMT ROUNDED =
                   PPYM-VOID-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PPYM-EXCEPT-AMT
           END-IF
           ADD PPYM-EXCEPT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DECISION CD:
      * AND -> BER WHEN LAST PAY DATE IS PAST
      * BER -> CZW WHEN LAST PAY DATE IS PAST
      * CZW -> SBV WHEN LAST PAY DATE IS PAST
      * SBV -> ZNG WHEN LAST PAY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3870-SET-DECISION.
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
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E176 ADDR REQUIRED
      * E483 VOID YTD NEGATIVE
      * E691 VOID YTD OVER 1000000
      * E105 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3880-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPYM-ADDR = SPACES
                   MOVE 'E176' TO WS-REJECT-CODE
               WHEN PPYM-VOID-YTD < 0
                   MOVE 'E483' TO WS-REJECT-CODE
               WHEN PPYM-VOID-YTD > 1000000.00
                   MOVE 'E691' TO WS-REJECT-CODE
               WHEN PPYM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E105' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ISSUED AMT BY MATCH RULE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3890-ACCUM-MATCH-RULE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPYM-MATCH-RULE-CD
                   ADD PPYM-ISSUED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPYM-MATCH-RULE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PPYM-ISSUED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 90 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3900-CHECK-OPEN.
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
           IF WS-DAYS > 90
               MOVE 'Y' TO PPYM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * VOID AMT IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3910-LIMIT-VOID.
           IF PPYT-VOID-AMT < 25.00
               MOVE 25.00 TO PPYT-VOID-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PPYT-VOID-AMT > 5000.00
               MOVE 5000.00 TO PPYT-VOID-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE VOID YTD BY DECISION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3920-ACCUM-DECISION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PPYM-DECISION-CD
                   ADD PPYM-VOID-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PPYM-DECISION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PPYM-VOID-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ISSUED BAND TIER FOR THE PAID AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3930-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-ISSUED-BAND-RATE(1) TO PPYM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF PPYT-PAID-AMT NOT < WS-ISSUED-BAND-LOW(WS-TX)
                   MOVE WS-ISSUED-BAND-RATE(WS-TX) TO PPYM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPYM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPYM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ISSUED BAND TIER FOR THE PAID AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3940-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-ISSUED-BAND-RATE(1) TO PPYM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF PPYM-PAID-AMT NOT < WS-ISSUED-BAND-LOW(WS-TX)
                   MOVE WS-ISSUED-BAND-RATE(WS-TX) TO PPYM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PPYM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PPYM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE STALE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ISSUED YTD.
      *----------------------------------------------------------------*
       3950-PRORATE-ISSUED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PPYM-ISSUED-YTD ROUNDED =
                   PPYT-STALE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PPYM-ISSUED-YTD
           END-IF
           ADD PPYM-ISSUED-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3960-SPLIT-EXCEPT.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 4)
           COMPUTE PPYM-EXCEPT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3970-COUNT-CYCLE.
           IF PPYM-CYCLE-CNT < 9999999
               ADD 1 TO PPYM-CYCLE-CNT
           END-IF
           IF PPYM-CYCLE-CNT NOT < 6
               MOVE 'Y' TO PPYM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3980-DESCRIBE-REGION.
           EVALUATE PPYM-REGION-CD
               WHEN 'A'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'B'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'D'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'H'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'N'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'Y'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E427 REF NO REQUIRED
      * E123 VOID YTD NEGATIVE
      * E618 VOID YTD OVER 99999
      * E653 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3990-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPYM-REF-NO = SPACES
                   MOVE 'E427' TO WS-REJECT-CODE
               WHEN PPYM-VOID-YTD < 0
                   MOVE 'E123' TO WS-REJECT-CODE
               WHEN PPYM-VOID-YTD > 99999.00
                   MOVE 'E618' TO WS-REJECT-CODE
               WHEN PPYM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E653' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * VOID YTD IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4000-LIMIT-VOID.
           IF PPYM-VOID-YTD < 10.00
               MOVE 10.00 TO PPYM-VOID-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PPYM-VOID-YTD > 1000.00
               MOVE 1000.00 TO PPYM-VOID-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4010-FLAG-ITEM.
           IF WS-WORK-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E608 CONTACT REQUIRED
      * E950 ISSUED AMT NEGATIVE
      * E292 ISSUED AMT OVER 50000
      * E601 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       4020-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PPYM-CONTACT = SPACES
                   MOVE 'E608' TO WS-REJECT-CODE
               WHEN PPYT-ISSUED-AMT < 0
                   MOVE 'E950' TO WS-REJECT-CODE
               WHEN PPYT-ISSUED-AMT > 50000.00
                   MOVE 'E292' TO WS-REJECT-CODE
               WHEN PPYT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E601' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE PPY FIELDS TO PPLIM AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7000-CALL-PPLIM.
           INITIALIZE PPLIM-PARMS
           MOVE PPYM-STALE-YTD
             TO PPLIM-STALE-YTD
           MOVE PPYM-EXCEPT-YTD
             TO PPLIM-EXCEPT-YTD
           MOVE PPYM-VOID-YTD
             TO PPLIM-VOID-YTD
           MOVE PPYM-PAID-YTD
             TO PPLIM-PAID-YTD
           MOVE PPYM-ISSUED-YTD
             TO PPLIM-ISSUED-YTD
           MOVE PPYM-PAID-AMT
             TO PPLIM-PAID-AMT
           MOVE PPYM-NEXT-REVIEW-DATE
             TO PPLIM-NEXT-REVIEW-DATE
           MOVE PPYM-CLOSE-DATE
             TO PPLIM-CLOSE-DATE
           MOVE PPYM-OPEN-DATE
             TO PPLIM-OPEN-DATE
           MOVE PPYM-EXPIRY-DATE
             TO PPLIM-EXPIRY-DATE
           MOVE PPYM-MATCH-RULE-CD
             TO PPLIM-MATCH-RULE-CD
           MOVE PPYM-DECISION-CD
             TO PPLIM-DECISION-CD
           MOVE PPYM-REASON-CD
             TO PPLIM-REASON-CD
           MOVE PPYM-REGION-CD
             TO PPLIM-REGION-CD
           MOVE PPYM-NOTICE-SENT-FLAG
             TO PPLIM-NOTICE-SENT-FLAG
           MOVE PPYM-AUTO-RENEW-FLAG
             TO PPLIM-AUTO-RENEW-FLAG
           MOVE PPYM-EXEMPT-FLAG
             TO PPLIM-EXEMPT-FLAG
           MOVE PPYM-CYCLE-CNT
             TO PPLIM-CYCLE-CNT
           MOVE PPYM-NOTICE-CNT
             TO PPLIM-NOTICE-CNT
           MOVE PPYM-RATE
             TO PPLIM-RATE
           MOVE PPYM-DISCOUNT
             TO PPLIM-DISCOUNT
           MOVE PPYM-NAME
             TO PPLIM-NAME
           MOVE PPYM-REF-NO
             TO PPLIM-REF-NO
           CALL 'PPLIM' USING PPLIM-PARMS
           EVALUATE PPLIM-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE PPLIM-STALE-YTD
                     TO PPYM-STALE-YTD
                   MOVE PPLIM-PAID-YTD
                     TO PPYM-PAID-YTD
                   MOVE PPLIM-PAID-AMT
                     TO PPYM-PAID-AMT
                   CONTINUE
               WHEN 08
                   MOVE 'RLIM' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-PPLIM' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE PPY FIELDS TO PPFEE AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7040-CALL-PPFEE.
           INITIALIZE PPFEE-PARMS
           MOVE PPYM-STALE-YTD
             TO PPFEE-STALE-YTD
           MOVE PPYM-PAID-YTD
             TO PPFEE-PAID-YTD
           MOVE PPYM-EXCEPT-AMT
             TO PPFEE-EXCEPT-AMT
           MOVE PPYM-VOID-YTD
             TO PPFEE-VOID-YTD
           MOVE PPYM-VOID-AMT
             TO PPFEE-VOID-AMT
           MOVE PPYM-PAID-AMT
             TO PPFEE-PAID-AMT
           MOVE PPYM-NEXT-REVIEW-DATE
             TO PPFEE-NEXT-REVIEW-DATE
           MOVE PPYM-LAST-PAY-DATE
             TO PPFEE-LAST-PAY-DATE
           MOVE PPYM-CLOSE-DATE
             TO PPFEE-CLOSE-DATE
           MOVE PPYM-OPEN-DATE
             TO PPFEE-OPEN-DATE
           MOVE PPYM-MATCH-RULE-CD
             TO PPFEE-MATCH-RULE-CD
           MOVE PPYM-REGION-CD
             TO PPFEE-REGION-CD
           MOVE PPYM-STATUS-CD
             TO PPFEE-STATUS-CD
           MOVE PPYM-REASON-CD
             TO PPFEE-REASON-CD
           MOVE PPYM-NOTICE-SENT-FLAG
             TO PPFEE-NOTICE-SENT-FLAG
           MOVE PPYM-EXEMPT-FLAG
             TO PPFEE-EXEMPT-FLAG
           MOVE PPYM-OVERRIDE-FLAG
             TO PPFEE-OVERRIDE-FLAG
           MOVE PPYM-PAYMENT-CNT
             TO PPFEE-PAYMENT-CNT
           MOVE PPYM-NOTICE-CNT
             TO PPFEE-NOTICE-CNT
           MOVE PPYM-DISCOUNT
             TO PPFEE-DISCOUNT
           MOVE PPYM-MARGIN
             TO PPFEE-MARGIN
           MOVE PPYM-CONTACT
             TO PPFEE-CONTACT
           MOVE PPYM-REF-NO
             TO PPFEE-REF-NO
           CALL 'PPFEE' USING PPFEE-PARMS
           EVALUATE PPFEE-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE PPFEE-VOID-YTD
                     TO PPYM-VOID-YTD
                   MOVE PPFEE-VOID-AMT
                     TO PPYM-VOID-AMT
                   MOVE PPFEE-NOTICE-SENT-FLAG
                     TO PPYM-NOTICE-SENT-FLAG
                   MOVE PPFEE-EXEMPT-FLAG
                     TO PPYM-EXEMPT-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RFEE' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7040-CALL-PPFEE' TO ABND-PARAGRAPH
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
           DISPLAY 'PPYAGE READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'PPYAGE' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
