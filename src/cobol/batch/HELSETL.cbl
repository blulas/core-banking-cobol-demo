      *================================================================*
      * PROGRAM  : HELSETL                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : HOME EQUITY LINES - SETTLEMENT. READS THE HEL       *
      *            TRANSACTIONS, UPDATES THE HEL MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. HOME EQUITY LINE DRAWS, BILLING,*
      *            RATE CHANGE AND FREEZE                              *
      * INPUT    : HELMST (KSDS), HELTRN, BANKCTL                      *
      * OUTPUT   : HELOUT, HELSERPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, HEFEE                   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-02-03 PQR  ORIGINAL                                       *
      * 1996-02-26 LMB  LIMIT FEE SCHEDULE UPDATE CHG9385              *
      * 1997-04-13 SKN  LIMIT TABLE EXPANDED REQ5007                   *
      * 1998-10-13 SKN  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1998-02-15 LMB  NEW MIN-PAY REPORT COLUMNS REQ6896             *
      * 2001-07-05 SKN  BALANCE FEE SCHEDULE UPDATE CHG3699            *
      * 2001-11-11 RJT  AUDIT FINDING - INTEREST CONTROL TOTALS INC0445*
      * 2008-10-18 TWH  BALANCE CALCULATION CORRECTED INC9791          *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELSETL.
       AUTHOR. D P SHAW.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "HELMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS HELM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "HELTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "HELOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "HELSERPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY HELMST.
       FD  TRAN-FILE.
       COPY HELTRN.
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
                                           VALUE 11.000000.
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
      * WS-DRAW-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-DRAW-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000492000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000255500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000750000000196500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003250000000416000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013250000000274000'.
       01  WS-DRAW-SCHED-TBL REDEFINES WS-DRAW-SCHED-VALUES.
           05  WS-DRAW-SCHED-ROW OCCURS 5 TIMES.
               10  WS-DRAW-SCHED-LOW       PIC 9(09)V99.
               10  WS-DRAW-SCHED-RATE      PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-INTEREST-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-INTEREST-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000241000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000314000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003000000000399000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003250000000010000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013250000000150000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013350000000880500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013450000000050000'.
       01  WS-INTEREST-TIER-TBL REDEFINES WS-INTEREST-TIER-VALUES.
           05  WS-INTEREST-TIER-ROW OCCURS 7 TIMES.
               10  WS-INTEREST-TIER-LOW    PIC 9(09)V99.
               10  WS-INTEREST-TIER-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-MIN-PAY-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-MIN-PAY-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000194000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000137000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001250000000234000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001300000000629500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001400000000505000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006400000000607000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016400000000199500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016900000000047500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017900000000807000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017950000000247000'.
       01  WS-MIN-PAY-BAND-TBL REDEFINES WS-MIN-PAY-BAND-VALUES.
           05  WS-MIN-PAY-BAND-ROW OCCURS 10 TIMES.
               10  WS-MIN-PAY-BAND-LOW     PIC 9(09)V99.
               10  WS-MIN-PAY-BAND-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-APPRAISE-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-APPRAISE-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000453500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000306000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003500000000055000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008500000000789000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00009000000000155000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011500000000112000'.
       01  WS-APPRAISE-TIER-TBL REDEFINES WS-APPRAISE-TIER-VALUES.
           05  WS-APPRAISE-TIER-ROW OCCURS 6 TIMES.
               10  WS-APPRAISE-TIER-LOW    PIC 9(09)V99.
               10  WS-APPRAISE-TIER-RATE   PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  HELSETL  HOME EQUITY LINES'.
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
       COPY HEFEEP.

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
               MOVE 'HELMST' TO ABND-FILE-NAME
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
           MOVE HELT-KEY TO HELM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-EDIT-REF-NO
               PERFORM 3010-LIMIT-ANNUAL-FEE
               PERFORM 3020-COUNT-PAYMENT
               PERFORM 3030-FLAG-MIN-PAY
               PERFORM 3040-SPLIT-MIN-PAY
               PERFORM 3050-COUNT-EXCEPT
               PERFORM 3060-PRORATE-MIN-PAY
               PERFORM 3070-FLAG-ITEM
               PERFORM 3080-AGE-POST
               PERFORM 3090-ACCUM-TYPE
               PERFORM 3100-ASSESS-AUTO-RENEW
               PERFORM 3110-CHECK-LAST-PAY
               PERFORM 3120-PRORATE-ANNUAL-FEE
               PERFORM 3130-PRORATE-LIMIT
               PERFORM 3140-EDIT-NAME
               PERFORM 3150-ASSESS-PREFERRED
               PERFORM 3160-AGE-EFFECTIVE
               PERFORM 3170-LIMIT-INTEREST
               PERFORM 3180-DESCRIBE-REGION
               PERFORM 3190-ACCRUE-BALANCE
               PERFORM 3200-COUNT-EXCEPT
               PERFORM 3210-SELECT-MARGIN
               PERFORM 3220-ACCUM-REGION
               PERFORM 3230-ACCUM-DRAW-PERIOD
               PERFORM 3240-ASSESS-REVERSAL
               PERFORM 3250-AGE-NEXT-DUE
               PERFORM 3260-DESCRIBE-TRAN
               PERFORM 3270-FLAG-BALANCE
               PERFORM 3280-ACCUM-REGION
               PERFORM 3290-COUNT-SEQ
               PERFORM 3300-FLAG-INTEREST
               PERFORM 3310-COUNT-RETRY
               PERFORM 3320-COUNT-EXCEPT
               PERFORM 3330-LIMIT-MIN-PAY
               PERFORM 3340-CHECK-LAST-MAINT
               PERFORM 3350-CHECK-EXPIRY
               PERFORM 3360-ACCUM-LIEN-POS
               PERFORM 3370-SPLIT-APPRAISE
               PERFORM 3380-FLAG-LIMIT
               PERFORM 3390-SET-REASON
               PERFORM 3400-SELECT-MARGIN
               PERFORM 3410-SELECT-PENALTY-PCT
               PERFORM 3420-ACCUM-REASON
               PERFORM 3430-ACCUM-REGION
               PERFORM 3440-ACCRUE-MIN-PAY
               PERFORM 3450-EDIT-CUST-ID
               PERFORM 3460-AGE-EFF
               PERFORM 3470-EDIT-NAME
               PERFORM 3480-SELECT-PENALTY-PCT
               PERFORM 3490-SET-LIEN-POS
               PERFORM 3500-CHECK-POST
               PERFORM 3510-DESCRIBE-LIEN-POS
               PERFORM 3520-COUNT-PAYMENT
               PERFORM 3530-SET-REGION
               PERFORM 3540-ACCUM-DRAW-PERIOD
               PERFORM 3550-SET-TYPE
               PERFORM 3560-ACCRUE-ANNUAL-FEE
               PERFORM 3570-ACCRUE-BALANCE
               PERFORM 3580-SELECT-PENALTY-PCT
               PERFORM 3590-SPLIT-BALANCE
               PERFORM 3600-ASSESS-PREFERRED
               PERFORM 3610-DESCRIBE-REASON
               PERFORM 3620-ASSESS-SUSPEND
               PERFORM 3630-ACCRUE-INTEREST
               PERFORM 3640-SET-TRAN
               PERFORM 3650-SELECT-MARGIN
               PERFORM 3660-FLAG-LIMIT
               PERFORM 3670-ACCRUE-MIN-PAY
               PERFORM 3680-SELECT-DISCOUNT
               PERFORM 3690-FLAG-APPRAISE
               PERFORM 3700-PRORATE-APPRAISE
               PERFORM 3710-ACCRUE-INTEREST
               PERFORM 3720-SET-LIEN-POS
               PERFORM 3730-DESCRIBE-LIEN-POS
               PERFORM 3740-LIMIT-ANNUAL-FEE
               PERFORM 3750-EDIT-DESC
               PERFORM 3760-SELECT-MARGIN
               PERFORM 3770-EDIT-CUST-ID
               PERFORM 3780-CHECK-POST
               PERFORM 3790-CHECK-CLOSE
               PERFORM 3800-ACCRUE-MIN-PAY
               PERFORM 3810-ASSESS-REVERSAL
               PERFORM 3820-SELECT-DISCOUNT
               PERFORM 3830-AGE-EXPIRY
               PERFORM 3840-ACCRUE-DRAW
               PERFORM 3850-PRORATE-ANNUAL-FEE
               PERFORM 3860-LIMIT-APPRAISE
               PERFORM 3870-ASSESS-SUSPEND
               PERFORM 3880-COUNT-PAYMENT
               PERFORM 3890-LIMIT-ITEM
               PERFORM 3900-COUNT-EXCEPT
               PERFORM 3910-DESCRIBE-CHANNEL
               PERFORM 3920-FLAG-MIN-PAY
               PERFORM 3930-SET-REGION
               PERFORM 3940-SELECT-MARGIN
               PERFORM 3950-SET-CHANNEL
               PERFORM 3960-EDIT-NAME
               PERFORM 3970-EDIT-REF-NO
               PERFORM 3980-SET-LIEN-POS
               PERFORM 3990-SET-STATUS
               PERFORM 4000-LIMIT-MIN-PAY
               PERFORM 4010-AGE-CLOSE
               PERFORM 4020-LIMIT-ANNUAL-FEE
               PERFORM 4030-EDIT-CONTACT
               PERFORM 4040-SET-DRAW-PERIOD
               PERFORM 4050-EDIT-REF-NO
               PERFORM 4060-AGE-NEXT-REVIEW
               PERFORM 4070-ACCUM-STATUS
               PERFORM 4080-LIMIT-APPRAISE
               PERFORM 4090-ACCRUE-BALANCE
               PERFORM 4100-ACCUM-TYPE
               PERFORM 4110-FLAG-MIN-PAY
               PERFORM 4120-EDIT-CUST-ID
               PERFORM 4130-SPLIT-DRAW
               PERFORM 4140-COUNT-EXCEPT
               PERFORM 7030-CALL-HEFEE
               REWRITE HEL-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'HELMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM HEL-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E504 REF NO REQUIRED
      * E178 MIN PAY YTD NEGATIVE
      * E200 MIN PAY YTD OVER 1000000
      * E598 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3000-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELM-REF-NO = SPACES
                   MOVE 'E504' TO WS-REJECT-CODE
               WHEN HELM-MIN-PAY-YTD < 0
                   MOVE 'E178' TO WS-REJECT-CODE
               WHEN HELM-MIN-PAY-YTD > 1000000.00
                   MOVE 'E200' TO WS-REJECT-CODE
               WHEN HELM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E598' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ANNUAL FEE YTD IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3010-LIMIT-ANNUAL-FEE.
           IF HELM-ANNUAL-FEE-YTD < 100.00
               MOVE 100.00 TO HELM-ANNUAL-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HELM-ANNUAL-FEE-YTD > 500.00
               MOVE 500.00 TO HELM-ANNUAL-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3020-COUNT-PAYMENT.
           IF HELM-PAYMENT-CNT < 9999999
               ADD 1 TO HELM-PAYMENT-CNT
           END-IF
           IF HELM-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO HELM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * MIN PAY AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3030-FLAG-MIN-PAY.
           IF HELM-MIN-PAY-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE HELM-MIN-PAY-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT MIN PAY YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3040-SPLIT-MIN-PAY.
           COMPUTE WS-WORK-AMT = HELM-MIN-PAY-YTD / 2
           COMPUTE WS-WORK-AMT-2 = HELM-MIN-PAY-YTD - (WS-WORK-AMT * 2)
           COMPUTE HELM-DRAW-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3050-COUNT-EXCEPT.
           IF HELM-EXCEPT-CNT < 9999999
               ADD 1 TO HELM-EXCEPT-CNT
           END-IF
           IF HELM-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO HELM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DRAW AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO MIN PAY AMT.
      *----------------------------------------------------------------*
       3060-PRORATE-MIN-PAY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HELM-MIN-PAY-AMT ROUNDED =
                   HELT-DRAW-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO HELM-MIN-PAY-AMT
           END-IF
           ADD HELM-MIN-PAY-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3070-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3080-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF HELT-POST-DATE > 0 AND HELT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD HELM-INTEREST-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD HELM-INTEREST-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD HELM-INTEREST-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD HELM-INTEREST-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HELM-INTEREST-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE LIMIT AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3090-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HELM-TYPE-CD
                   ADD HELM-LIMIT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HELM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE HELM-LIMIT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE BALANCE YTD FEE ON THE DRAW AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3100-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN HELM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO HELM-BALANCE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE HELM-BALANCE-YTD ROUNDED = HELM-DRAW-AMT *
                       HELM-MARGIN / 200
               WHEN HELM-DRAW-PERIOD-CD = 'CCV'
                   MOVE 10.00 TO HELM-BALANCE-YTD
               WHEN HELM-DRAW-PERIOD-CD = 'JSU'
                   MOVE 3.00 TO HELM-BALANCE-YTD
               WHEN OTHER
                   COMPUTE HELM-BALANCE-YTD ROUNDED = HELM-DRAW-AMT *
                       HELM-MARGIN / 100
           END-EVALUATE
           IF HELM-BALANCE-YTD > 0 AND HELM-BALANCE-YTD < 2.00
               MOVE 2.00 TO HELM-BALANCE-YTD
           END-IF
           IF HELM-BALANCE-YTD > 75.00
               MOVE 75.00 TO HELM-BALANCE-YTD
           END-IF
           ADD HELM-BALANCE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3110-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF HELM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO HELM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DRAW AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ANNUAL FEE AMT.
      *----------------------------------------------------------------*
       3120-PRORATE-ANNUAL-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HELT-ANNUAL-FEE-AMT ROUNDED =
                   HELM-DRAW-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO HELT-ANNUAL-FEE-AMT
           END-IF
           ADD HELT-ANNUAL-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE LIMIT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO LIMIT AMT.
      *----------------------------------------------------------------*
       3130-PRORATE-LIMIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HELM-LIMIT-AMT ROUNDED =
                   HELM-LIMIT-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO HELM-LIMIT-AMT
           END-IF
           ADD HELM-LIMIT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E424 NAME REQUIRED
      * E065 APPRAISE AMT NEGATIVE
      * E764 APPRAISE AMT OVER 25000
      * E193 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3140-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELM-NAME = SPACES
                   MOVE 'E424' TO WS-REJECT-CODE
               WHEN HELT-APPRAISE-AMT < 0
                   MOVE 'E065' TO WS-REJECT-CODE
               WHEN HELT-APPRAISE-AMT > 25000.00
                   MOVE 'E764' TO WS-REJECT-CODE
               WHEN HELM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E193' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE APPRAISE FEE ON THE ANNUAL FEE YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3150-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN HELM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO HELM-APPRAISE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE HELM-APPRAISE-AMT ROUNDED =
                       HELM-ANNUAL-FEE-YTD * HELM-PENALTY-PCT / 200
               WHEN HELM-TYPE-CD = 'CD'
                   MOVE 15.00 TO HELM-APPRAISE-AMT
               WHEN HELM-TYPE-CD = 'JW'
                   MOVE 20.00 TO HELM-APPRAISE-AMT
               WHEN OTHER
                   COMPUTE HELM-APPRAISE-AMT ROUNDED =
                       HELM-ANNUAL-FEE-YTD * HELM-PENALTY-PCT / 100
           END-EVALUATE
           IF HELM-APPRAISE-AMT > 0 AND HELM-APPRAISE-AMT < 2.00
               MOVE 2.00 TO HELM-APPRAISE-AMT
           END-IF
           IF HELM-APPRAISE-AMT > 250.00
               MOVE 250.00 TO HELM-APPRAISE-AMT
           END-IF
           ADD HELM-APPRAISE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3160-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF HELM-EFFECTIVE-DATE > 0 AND HELM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD HELM-DRAW-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD HELM-DRAW-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD HELM-DRAW-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD HELM-DRAW-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HELM-DRAW-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * INTEREST YTD IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3170-LIMIT-INTEREST.
           IF HELM-INTEREST-YTD < 25.00
               MOVE 25.00 TO HELM-INTEREST-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HELM-INTEREST-YTD > 5000.00
               MOVE 5000.00 TO HELM-INTEREST-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3180-DESCRIBE-REGION.
           EVALUATE HELM-REGION-CD
               WHEN 'BSD'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'HKW'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'HPB'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'ULJ'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON BALANCE YTD AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3190-ACCRUE-BALANCE.
           IF HELM-BALANCE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HELM-BALANCE-YTD * HELM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3200-COUNT-EXCEPT.
           IF HELM-EXCEPT-CNT < 9999999
               ADD 1 TO HELM-EXCEPT-CNT
           END-IF
           IF HELM-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO HELM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE INTEREST TIER TIER FOR THE APPRAISE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3210-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-INTEREST-TIER-RATE(1) TO HELM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF HELM-APPRAISE-AMT NOT < WS-INTEREST-TIER-LOW(WS-TX)
                   MOVE WS-INTEREST-TIER-RATE(WS-TX) TO HELM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE BALANCE AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3220-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HELM-REGION-CD
                   ADD HELM-BALANCE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HELM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE HELM-BALANCE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE MIN PAY YTD BY DRAW PERIOD CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3230-ACCUM-DRAW-PERIOD.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HELM-DRAW-PERIOD-CD
                   ADD HELM-MIN-PAY-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HELM-DRAW-PERIOD-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE HELM-MIN-PAY-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE APPRAISE FEE ON THE ANNUAL FEE YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3240-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN HELT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO HELT-APPRAISE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELM-SUSPEND-FLAG = 'Y'
                   COMPUTE HELT-APPRAISE-AMT ROUNDED =
                       HELM-ANNUAL-FEE-YTD * HELM-MARGIN / 200
               WHEN HELM-LIEN-POS-CD = 'AXX'
                   MOVE 7.00 TO HELT-APPRAISE-AMT
               WHEN HELM-LIEN-POS-CD = 'PVA'
                   MOVE 5.00 TO HELT-APPRAISE-AMT
               WHEN OTHER
                   COMPUTE HELT-APPRAISE-AMT ROUNDED =
                       HELM-ANNUAL-FEE-YTD * HELM-MARGIN / 100
           END-EVALUATE
           IF HELT-APPRAISE-AMT > 0 AND HELT-APPRAISE-AMT < 1.00
               MOVE 1.00 TO HELT-APPRAISE-AMT
           END-IF
           IF HELT-APPRAISE-AMT > 25.00
               MOVE 25.00 TO HELT-APPRAISE-AMT
           END-IF
           ADD HELT-APPRAISE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3250-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF HELM-NEXT-DUE-DATE > 0 AND HELM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD HELT-LIMIT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD HELT-LIMIT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD HELT-LIMIT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD HELT-LIMIT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HELT-LIMIT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3260-DESCRIBE-TRAN.
           EVALUATE HELT-TRAN-CD
               WHEN 'DR'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'PY'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'FZ'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'RC'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'AF'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * BALANCE AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3270-FLAG-BALANCE.
           IF HELM-BALANCE-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE HELM-BALANCE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE BALANCE YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3280-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HELM-REGION-CD
                   ADD HELM-BALANCE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HELM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE HELM-BALANCE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3290-COUNT-SEQ.
           IF HELT-SEQ < 9999999
               ADD 1 TO HELT-SEQ
           END-IF
           IF HELT-SEQ NOT < 10
               MOVE 'Y' TO HELM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * INTEREST AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3300-FLAG-INTEREST.
           IF HELM-INTEREST-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE HELM-INTEREST-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3310-COUNT-RETRY.
           IF HELM-RETRY-CNT < 9999999
               ADD 1 TO HELM-RETRY-CNT
           END-IF
           IF HELM-RETRY-CNT NOT < 5
               MOVE 'Y' TO HELT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3320-COUNT-EXCEPT.
           IF HELM-EXCEPT-CNT < 9999999
               ADD 1 TO HELM-EXCEPT-CNT
           END-IF
           IF HELM-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO HELM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * MIN PAY AMT IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3330-LIMIT-MIN-PAY.
           IF HELM-MIN-PAY-AMT < 25.00
               MOVE 25.00 TO HELM-MIN-PAY-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HELM-MIN-PAY-AMT > 50000.00
               MOVE 50000.00 TO HELM-MIN-PAY-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 10 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3340-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF HELM-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO HELM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3350-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF HELM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO HELT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE DRAW AMT BY LIEN POS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3360-ACCUM-LIEN-POS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HELM-LIEN-POS-CD
                   ADD HELM-DRAW-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HELM-LIEN-POS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE HELM-DRAW-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT APPRAISE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3370-SPLIT-APPRAISE.
           COMPUTE WS-WORK-AMT = HELM-APPRAISE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = HELM-APPRAISE-AMT - (WS-WORK-AMT * 3)
           COMPUTE HELM-BALANCE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * LIMIT AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3380-FLAG-LIMIT.
           IF HELT-LIMIT-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE HELT-LIMIT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * KST -> NWS WHEN EFF DATE IS PAST
      * NWS -> ZJM WHEN EFF DATE IS PAST
      * ZJM -> KST WHEN EFF DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3390-SET-REASON.
           EVALUATE HELM-REASON-CD
               WHEN 'KST'
                   IF HELT-EFF-DATE > 0 AND HELT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NWS' TO HELM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NWS'
                   IF HELT-EFF-DATE > 0 AND HELT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZJM' TO HELM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZJM'
                   IF HELT-EFF-DATE > 0 AND HELT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KST' TO HELM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE DRAW SCHED TIER FOR THE MIN PAY YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3400-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-DRAW-SCHED-RATE(1) TO HELM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF HELM-MIN-PAY-YTD NOT < WS-DRAW-SCHED-LOW(WS-TX)
                   MOVE WS-DRAW-SCHED-RATE(WS-TX) TO HELM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE MIN PAY BAND TIER FOR THE APPRAISE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3410-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-MIN-PAY-BAND-RATE(1) TO HELM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF HELM-APPRAISE-AMT NOT < WS-MIN-PAY-BAND-LOW(WS-TX)
                   MOVE WS-MIN-PAY-BAND-RATE(WS-TX) TO HELM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE MIN PAY YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3420-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HELM-REASON-CD
                   ADD HELM-MIN-PAY-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HELM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE HELM-MIN-PAY-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE INTEREST AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3430-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HELM-REGION-CD
                   ADD HELM-INTEREST-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HELM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE HELM-INTEREST-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON MIN PAY YTD AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3440-ACCRUE-MIN-PAY.
           IF HELM-MIN-PAY-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HELM-MIN-PAY-YTD * HELM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO HELM-DRAW-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E899 CUST ID REQUIRED
      * E567 ANNUAL FEE AMT NEGATIVE
      * E573 ANNUAL FEE AMT OVER 5000
      * E351 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3450-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELM-CUST-ID = SPACES
                   MOVE 'E899' TO WS-REJECT-CODE
               WHEN HELM-ANNUAL-FEE-AMT < 0
                   MOVE 'E567' TO WS-REJECT-CODE
               WHEN HELM-ANNUAL-FEE-AMT > 5000.00
                   MOVE 'E573' TO WS-REJECT-CODE
               WHEN HELM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E351' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-45 AND OVER 45 DAYS.
      *----------------------------------------------------------------*
       3460-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF HELT-EFF-DATE > 0 AND HELT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD HELM-BALANCE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD HELM-BALANCE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD HELM-BALANCE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 45
                   ADD HELM-BALANCE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HELM-BALANCE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E076 NAME REQUIRED
      * E013 BALANCE YTD NEGATIVE
      * E012 BALANCE YTD OVER 10000
      * E391 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3470-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELM-NAME = SPACES
                   MOVE 'E076' TO WS-REJECT-CODE
               WHEN HELM-BALANCE-YTD < 0
                   MOVE 'E013' TO WS-REJECT-CODE
               WHEN HELM-BALANCE-YTD > 10000.00
                   MOVE 'E012' TO WS-REJECT-CODE
               WHEN HELM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E391' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE MIN PAY BAND TIER FOR THE APPRAISE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3480-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-MIN-PAY-BAND-RATE(1) TO HELM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF HELT-APPRAISE-AMT NOT < WS-MIN-PAY-BAND-LOW(WS-TX)
                   MOVE WS-MIN-PAY-BAND-RATE(WS-TX) TO HELM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR LIEN POS CD:
      * AXX -> PVA WHEN EFF DATE IS PAST
      * PVA -> SFC WHEN EFF DATE IS PAST
      * SFC -> AXX WHEN EFF DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3490-SET-LIEN-POS.
           EVALUATE HELM-LIEN-POS-CD
               WHEN 'AXX'
                   IF HELT-EFF-DATE > 0 AND HELT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PVA' TO HELM-LIEN-POS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PVA'
                   IF HELT-EFF-DATE > 0 AND HELT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SFC' TO HELM-LIEN-POS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SFC'
                   IF HELT-EFF-DATE > 0 AND HELT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AXX' TO HELM-LIEN-POS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 365 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3500-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF HELT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO HELM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE LIEN POS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3510-DESCRIBE-LIEN-POS.
           EVALUATE HELM-LIEN-POS-CD
               WHEN 'AXX'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'PVA'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'SFC'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3520-COUNT-PAYMENT.
           IF HELM-PAYMENT-CNT < 9999999
               ADD 1 TO HELM-PAYMENT-CNT
           END-IF
           IF HELM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO HELM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * BSD -> HKW WHEN NEXT REVIEW DATE IS PAST
      * HKW -> HPB WHEN NEXT REVIEW DATE IS PAST
      * HPB -> ULJ WHEN NEXT REVIEW DATE IS PAST
      * ULJ -> BSD WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3530-SET-REGION.
           EVALUATE HELM-REGION-CD
               WHEN 'BSD'
                   IF HELM-NEXT-REVIEW-DATE > 0 AND
                       HELM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'HKW' TO HELM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HKW'
                   IF HELM-NEXT-REVIEW-DATE > 0 AND
                       HELM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'HPB' TO HELM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HPB'
                   IF HELM-NEXT-REVIEW-DATE > 0 AND
                       HELM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'ULJ' TO HELM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ULJ'
                   IF HELM-NEXT-REVIEW-DATE > 0 AND
                       HELM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'BSD' TO HELM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE INTEREST YTD BY DRAW PERIOD CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3540-ACCUM-DRAW-PERIOD.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HELM-DRAW-PERIOD-CD
                   ADD HELM-INTEREST-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HELM-DRAW-PERIOD-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE HELM-INTEREST-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * CD -> JW WHEN EFF DATE IS PAST
      * JW -> KV WHEN EFF DATE IS PAST
      * KV -> WS WHEN EFF DATE IS PAST
      * WS -> XL WHEN EFF DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3550-SET-TYPE.
           EVALUATE HELM-TYPE-CD
               WHEN 'CD'
                   IF HELT-EFF-DATE > 0 AND HELT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JW' TO HELM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JW'
                   IF HELT-EFF-DATE > 0 AND HELT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KV' TO HELM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KV'
                   IF HELT-EFF-DATE > 0 AND HELT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WS' TO HELM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WS'
                   IF HELT-EFF-DATE > 0 AND HELT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XL' TO HELM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ANNUAL FEE AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3560-ACCRUE-ANNUAL-FEE.
           IF HELT-ANNUAL-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HELT-ANNUAL-FEE-AMT * HELM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO HELM-BALANCE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON BALANCE AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3570-ACCRUE-BALANCE.
           IF HELM-BALANCE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HELM-BALANCE-AMT * HELM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO HELM-BALANCE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DRAW SCHED TIER FOR THE LIMIT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3580-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DRAW-SCHED-RATE(1) TO HELM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF HELM-LIMIT-YTD NOT < WS-DRAW-SCHED-LOW(WS-TX)
                   MOVE WS-DRAW-SCHED-RATE(WS-TX) TO HELM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT BALANCE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3590-SPLIT-BALANCE.
           COMPUTE WS-WORK-AMT = HELM-BALANCE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = HELM-BALANCE-AMT - (WS-WORK-AMT * 4)
           COMPUTE HELM-BALANCE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DRAW FEE ON THE WORK AMT 2.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3600-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN HELM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO HELM-DRAW-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELM-HOLD-FLAG = 'Y'
                   COMPUTE HELM-DRAW-AMT ROUNDED = WS-WORK-AMT-2 *
                       HELM-PENALTY-PCT / 200
               WHEN HELM-STATUS-CD = 'GR'
                   MOVE 3.00 TO HELM-DRAW-AMT
               WHEN HELM-STATUS-CD = 'JC'
                   MOVE 5.00 TO HELM-DRAW-AMT
               WHEN OTHER
                   COMPUTE HELM-DRAW-AMT ROUNDED = WS-WORK-AMT-2 *
                       HELM-PENALTY-PCT / 100
           END-EVALUATE
           IF HELM-DRAW-AMT > 0 AND HELM-DRAW-AMT < 10.00
               MOVE 10.00 TO HELM-DRAW-AMT
           END-IF
           IF HELM-DRAW-AMT > 35.00
               MOVE 35.00 TO HELM-DRAW-AMT
           END-IF
           ADD HELM-DRAW-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3610-DESCRIBE-REASON.
           EVALUATE HELM-REASON-CD
               WHEN 'KST'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'NWS'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ZJM'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE MIN PAY YTD FEE ON THE LIMIT AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3620-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN HELM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO HELM-MIN-PAY-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELT-REVERSAL-FLAG = 'Y'
                   COMPUTE HELM-MIN-PAY-YTD ROUNDED = HELT-LIMIT-AMT *
                       HELM-DISCOUNT / 200
               WHEN HELM-DRAW-PERIOD-CD = 'CCV'
                   MOVE 3.00 TO HELM-MIN-PAY-YTD
               WHEN HELM-DRAW-PERIOD-CD = 'JSU'
                   MOVE 20.00 TO HELM-MIN-PAY-YTD
               WHEN OTHER
                   COMPUTE HELM-MIN-PAY-YTD ROUNDED = HELT-LIMIT-AMT *
                       HELM-DISCOUNT / 100
           END-EVALUATE
           IF HELM-MIN-PAY-YTD > 0 AND HELM-MIN-PAY-YTD < 2.00
               MOVE 2.00 TO HELM-MIN-PAY-YTD
           END-IF
           IF HELM-MIN-PAY-YTD > 75.00
               MOVE 75.00 TO HELM-MIN-PAY-YTD
           END-IF
           ADD HELM-MIN-PAY-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INTEREST AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3630-ACCRUE-INTEREST.
           IF HELM-INTEREST-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HELM-INTEREST-AMT * HELM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO HELM-DRAW-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * DR -> PY WHEN OPEN DATE IS PAST
      * PY -> FZ WHEN OPEN DATE IS PAST
      * FZ -> RC WHEN OPEN DATE IS PAST
      * RC -> AF WHEN OPEN DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3640-SET-TRAN.
           EVALUATE HELT-TRAN-CD
               WHEN 'DR'
                   IF HELM-OPEN-DATE > 0 AND HELM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PY' TO HELT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PY'
                   IF HELM-OPEN-DATE > 0 AND HELM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FZ' TO HELT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FZ'
                   IF HELM-OPEN-DATE > 0 AND HELM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RC' TO HELT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RC'
                   IF HELM-OPEN-DATE > 0 AND HELM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AF' TO HELT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE DRAW SCHED TIER FOR THE ANNUAL FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3650-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-DRAW-SCHED-RATE(1) TO HELM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF HELT-ANNUAL-FEE-AMT NOT < WS-DRAW-SCHED-LOW(WS-TX)
                   MOVE WS-DRAW-SCHED-RATE(WS-TX) TO HELM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * LIMIT YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3660-FLAG-LIMIT.
           IF HELM-LIMIT-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE HELM-LIMIT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON MIN PAY YTD AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3670-ACCRUE-MIN-PAY.
           IF HELM-MIN-PAY-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HELM-MIN-PAY-YTD * HELM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO HELT-LIMIT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE INTEREST TIER TIER FOR THE DRAW AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3680-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-INTEREST-TIER-RATE(1) TO HELM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF HELM-DRAW-AMT NOT < WS-INTEREST-TIER-LOW(WS-TX)
                   MOVE WS-INTEREST-TIER-RATE(WS-TX) TO HELM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * APPRAISE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3690-FLAG-APPRAISE.
           IF HELM-APPRAISE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE HELM-APPRAISE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO APPRAISE AMT.
      *----------------------------------------------------------------*
       3700-PRORATE-APPRAISE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HELM-APPRAISE-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO HELM-APPRAISE-AMT
           END-IF
           ADD HELM-APPRAISE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INTEREST AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3710-ACCRUE-INTEREST.
           IF HELM-INTEREST-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HELM-INTEREST-AMT * HELM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO HELM-DRAW-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR LIEN POS CD:
      * AXX -> PVA WHEN OPEN DATE IS PAST
      * PVA -> SFC WHEN OPEN DATE IS PAST
      * SFC -> AXX WHEN OPEN DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3720-SET-LIEN-POS.
           EVALUATE HELM-LIEN-POS-CD
               WHEN 'AXX'
                   IF HELM-OPEN-DATE > 0 AND HELM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PVA' TO HELM-LIEN-POS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PVA'
                   IF HELM-OPEN-DATE > 0 AND HELM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SFC' TO HELM-LIEN-POS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SFC'
                   IF HELM-OPEN-DATE > 0 AND HELM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AXX' TO HELM-LIEN-POS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE LIEN POS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3730-DESCRIBE-LIEN-POS.
           EVALUATE HELM-LIEN-POS-CD
               WHEN 'AXX'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'PVA'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'SFC'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ANNUAL FEE AMT IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3740-LIMIT-ANNUAL-FEE.
           IF HELM-ANNUAL-FEE-AMT < 100.00
               MOVE 100.00 TO HELM-ANNUAL-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HELM-ANNUAL-FEE-AMT > 1000.00
               MOVE 1000.00 TO HELM-ANNUAL-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E846 DESC REQUIRED
      * E044 INTEREST YTD NEGATIVE
      * E309 INTEREST YTD OVER 25000
      * E258 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3750-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELM-DESC = SPACES
                   MOVE 'E846' TO WS-REJECT-CODE
               WHEN HELM-INTEREST-YTD < 0
                   MOVE 'E044' TO WS-REJECT-CODE
               WHEN HELM-INTEREST-YTD > 25000.00
                   MOVE 'E309' TO WS-REJECT-CODE
               WHEN HELM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E258' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE APPRAISE TIER TIER FOR THE ANNUAL FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3760-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-APPRAISE-TIER-RATE(1) TO HELM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF HELT-ANNUAL-FEE-AMT NOT < WS-APPRAISE-TIER-LOW(WS-TX)
                   MOVE WS-APPRAISE-TIER-RATE(WS-TX) TO HELM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E569 CUST ID REQUIRED
      * E263 DRAW AMT NEGATIVE
      * E083 DRAW AMT OVER 25000
      * E129 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3770-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELM-CUST-ID = SPACES
                   MOVE 'E569' TO WS-REJECT-CODE
               WHEN HELT-DRAW-AMT < 0
                   MOVE 'E263' TO WS-REJECT-CODE
               WHEN HELT-DRAW-AMT > 25000.00
                   MOVE 'E083' TO WS-REJECT-CODE
               WHEN HELM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E129' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 30 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3780-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF HELT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO HELM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3790-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF HELM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO HELM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON MIN PAY AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3800-ACCRUE-MIN-PAY.
           IF HELM-MIN-PAY-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HELM-MIN-PAY-AMT * HELM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO HELT-DRAW-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE INTEREST YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3810-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN HELT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = HELM-INTEREST-YTD *
                       HELM-PENALTY-PCT / 200
               WHEN HELM-DRAW-PERIOD-CD = 'CCV'
                   MOVE 20.00 TO WS-WORK-AMT
               WHEN HELM-DRAW-PERIOD-CD = 'JSU'
                   MOVE 20.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = HELM-INTEREST-YTD *
                       HELM-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 50.00
               MOVE 50.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE MIN PAY BAND TIER FOR THE ANNUAL FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3820-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-MIN-PAY-BAND-RATE(1) TO HELM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF HELM-ANNUAL-FEE-YTD NOT < WS-MIN-PAY-BAND-LOW(WS-TX)
                   MOVE WS-MIN-PAY-BAND-RATE(WS-TX) TO HELM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3830-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF HELM-EXPIRY-DATE > 0 AND HELM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD HELM-LIMIT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD HELM-LIMIT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD HELM-LIMIT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD HELM-LIMIT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HELM-LIMIT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DRAW YTD AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3840-ACCRUE-DRAW.
           IF HELM-DRAW-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HELM-DRAW-YTD * HELM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO HELT-DRAW-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE LIMIT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ANNUAL FEE YTD.
      *----------------------------------------------------------------*
       3850-PRORATE-ANNUAL-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HELM-ANNUAL-FEE-YTD ROUNDED =
                   HELT-LIMIT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO HELM-ANNUAL-FEE-YTD
           END-IF
           ADD HELM-ANNUAL-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * APPRAISE YTD IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3860-LIMIT-APPRAISE.
           IF HELM-APPRAISE-YTD < 0.00
               MOVE 0.00 TO HELM-APPRAISE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HELM-APPRAISE-YTD > 500.00
               MOVE 500.00 TO HELM-APPRAISE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE INTEREST YTD FEE ON THE LIMIT AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3870-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN HELM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO HELM-INTEREST-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELM-PREFERRED-FLAG = 'Y'
                   COMPUTE HELM-INTEREST-YTD ROUNDED = HELT-LIMIT-AMT *
                       HELM-DISCOUNT / 200
               WHEN HELT-TRAN-CD = 'DR'
                   MOVE 5.00 TO HELM-INTEREST-YTD
               WHEN HELT-TRAN-CD = 'PY'
                   MOVE 7.00 TO HELM-INTEREST-YTD
               WHEN OTHER
                   COMPUTE HELM-INTEREST-YTD ROUNDED = HELT-LIMIT-AMT *
                       HELM-DISCOUNT / 100
           END-EVALUATE
           IF HELM-INTEREST-YTD > 0 AND HELM-INTEREST-YTD < 1.00
               MOVE 1.00 TO HELM-INTEREST-YTD
           END-IF
           IF HELM-INTEREST-YTD > 25.00
               MOVE 25.00 TO HELM-INTEREST-YTD
           END-IF
           ADD HELM-INTEREST-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3880-COUNT-PAYMENT.
           IF HELM-PAYMENT-CNT < 9999999
               ADD 1 TO HELM-PAYMENT-CNT
           END-IF
           IF HELM-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO HELM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3890-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 25.00
               MOVE 25.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 1000.00
               MOVE 1000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3900-COUNT-EXCEPT.
           IF HELM-EXCEPT-CNT < 9999999
               ADD 1 TO HELM-EXCEPT-CNT
           END-IF
           IF HELM-EXCEPT-CNT NOT < 3
               MOVE 'Y' TO HELM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3910-DESCRIBE-CHANNEL.
           EVALUATE HELT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * MIN PAY AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3920-FLAG-MIN-PAY.
           IF HELM-MIN-PAY-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE HELM-MIN-PAY-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * BSD -> HKW WHEN EFF DATE IS PAST
      * HKW -> HPB WHEN EFF DATE IS PAST
      * HPB -> ULJ WHEN EFF DATE IS PAST
      * ULJ -> BSD WHEN EFF DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3930-SET-REGION.
           EVALUATE HELM-REGION-CD
               WHEN 'BSD'
                   IF HELT-EFF-DATE > 0 AND HELT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HKW' TO HELM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HKW'
                   IF HELT-EFF-DATE > 0 AND HELT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HPB' TO HELM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HPB'
                   IF HELT-EFF-DATE > 0 AND HELT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ULJ' TO HELM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ULJ'
                   IF HELT-EFF-DATE > 0 AND HELT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BSD' TO HELM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE INTEREST TIER TIER FOR THE LIMIT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3940-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-INTEREST-TIER-RATE(1) TO HELM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF HELT-LIMIT-AMT NOT < WS-INTEREST-TIER-LOW(WS-TX)
                   MOVE WS-INTEREST-TIER-RATE(WS-TX) TO HELM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN POST DATE IS PAST
      * BAT -> MAL WHEN POST DATE IS PAST
      * MAL -> TEL WHEN POST DATE IS PAST
      * TEL -> ONL WHEN POST DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3950-SET-CHANNEL.
           EVALUATE HELT-CHANNEL-CD
               WHEN 'BRN'
                   IF HELT-POST-DATE > 0 AND HELT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO HELT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF HELT-POST-DATE > 0 AND HELT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO HELT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF HELT-POST-DATE > 0 AND HELT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO HELT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF HELT-POST-DATE > 0 AND HELT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO HELT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E645 NAME REQUIRED
      * E898 ANNUAL FEE AMT NEGATIVE
      * E744 ANNUAL FEE AMT OVER 10000
      * E910 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3960-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELM-NAME = SPACES
                   MOVE 'E645' TO WS-REJECT-CODE
               WHEN HELM-ANNUAL-FEE-AMT < 0
                   MOVE 'E898' TO WS-REJECT-CODE
               WHEN HELM-ANNUAL-FEE-AMT > 10000.00
                   MOVE 'E744' TO WS-REJECT-CODE
               WHEN HELM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E910' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E016 REF NO REQUIRED
      * E932 MIN PAY AMT NEGATIVE
      * E717 MIN PAY AMT OVER 5000
      * E957 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3970-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELM-REF-NO = SPACES
                   MOVE 'E016' TO WS-REJECT-CODE
               WHEN HELM-MIN-PAY-AMT < 0
                   MOVE 'E932' TO WS-REJECT-CODE
               WHEN HELM-MIN-PAY-AMT > 5000.00
                   MOVE 'E717' TO WS-REJECT-CODE
               WHEN HELM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E957' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR LIEN POS CD:
      * AXX -> PVA WHEN EXPIRY DATE IS PAST
      * PVA -> SFC WHEN EXPIRY DATE IS PAST
      * SFC -> AXX WHEN EXPIRY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3980-SET-LIEN-POS.
           EVALUATE HELM-LIEN-POS-CD
               WHEN 'AXX'
                   IF HELM-EXPIRY-DATE > 0 AND HELM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PVA' TO HELM-LIEN-POS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PVA'
                   IF HELM-EXPIRY-DATE > 0 AND HELM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SFC' TO HELM-LIEN-POS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SFC'
                   IF HELM-EXPIRY-DATE > 0 AND HELM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AXX' TO HELM-LIEN-POS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * GR -> JC WHEN NEXT REVIEW DATE IS PAST
      * JC -> KH WHEN NEXT REVIEW DATE IS PAST
      * KH -> KN WHEN NEXT REVIEW DATE IS PAST
      * KN -> PC WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3990-SET-STATUS.
           EVALUATE HELM-STATUS-CD
               WHEN 'GR'
                   IF HELM-NEXT-REVIEW-DATE > 0 AND
                       HELM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'JC' TO HELM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JC'
                   IF HELM-NEXT-REVIEW-DATE > 0 AND
                       HELM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'KH' TO HELM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KH'
                   IF HELM-NEXT-REVIEW-DATE > 0 AND
                       HELM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'KN' TO HELM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KN'
                   IF HELM-NEXT-REVIEW-DATE > 0 AND
                       HELM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'PC' TO HELM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * MIN PAY AMT IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4000-LIMIT-MIN-PAY.
           IF HELM-MIN-PAY-AMT < 0.00
               MOVE 0.00 TO HELM-MIN-PAY-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HELM-MIN-PAY-AMT > 2500.00
               MOVE 2500.00 TO HELM-MIN-PAY-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4010-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF HELM-CLOSE-DATE > 0 AND HELM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD HELT-LIMIT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD HELT-LIMIT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD HELT-LIMIT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD HELT-LIMIT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HELT-LIMIT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ANNUAL FEE YTD IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4020-LIMIT-ANNUAL-FEE.
           IF HELM-ANNUAL-FEE-YTD < 50.00
               MOVE 50.00 TO HELM-ANNUAL-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HELM-ANNUAL-FEE-YTD > 1000.00
               MOVE 1000.00 TO HELM-ANNUAL-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E032 CONTACT REQUIRED
      * E268 LIMIT AMT NEGATIVE
      * E853 LIMIT AMT OVER 25000
      * E286 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       4030-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELM-CONTACT = SPACES
                   MOVE 'E032' TO WS-REJECT-CODE
               WHEN HELM-LIMIT-AMT < 0
                   MOVE 'E268' TO WS-REJECT-CODE
               WHEN HELM-LIMIT-AMT > 25000.00
                   MOVE 'E853' TO WS-REJECT-CODE
               WHEN HELM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E286' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DRAW PERIOD CD:
      * CCV -> JSU WHEN EXPIRY DATE IS PAST
      * JSU -> KXA WHEN EXPIRY DATE IS PAST
      * KXA -> VMR WHEN EXPIRY DATE IS PAST
      * VMR -> WLJ WHEN EXPIRY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4040-SET-DRAW-PERIOD.
           EVALUATE HELM-DRAW-PERIOD-CD
               WHEN 'CCV'
                   IF HELM-EXPIRY-DATE > 0 AND HELM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JSU' TO HELM-DRAW-PERIOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JSU'
                   IF HELM-EXPIRY-DATE > 0 AND HELM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KXA' TO HELM-DRAW-PERIOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KXA'
                   IF HELM-EXPIRY-DATE > 0 AND HELM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VMR' TO HELM-DRAW-PERIOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VMR'
                   IF HELM-EXPIRY-DATE > 0 AND HELM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WLJ' TO HELM-DRAW-PERIOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E005 REF NO REQUIRED
      * E226 LIMIT AMT NEGATIVE
      * E230 LIMIT AMT OVER 99999
      * E995 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       4050-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELM-REF-NO = SPACES
                   MOVE 'E005' TO WS-REJECT-CODE
               WHEN HELT-LIMIT-AMT < 0
                   MOVE 'E226' TO WS-REJECT-CODE
               WHEN HELT-LIMIT-AMT > 99999.00
                   MOVE 'E230' TO WS-REJECT-CODE
               WHEN HELM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E995' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-60, 61-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4060-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF HELM-NEXT-REVIEW-DATE > 0 AND HELM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD HELM-BALANCE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD HELM-BALANCE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD HELM-BALANCE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD HELM-BALANCE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HELM-BALANCE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE APPRAISE AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4070-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HELM-STATUS-CD
                   ADD HELT-APPRAISE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HELM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE HELT-APPRAISE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * APPRAISE YTD IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4080-LIMIT-APPRAISE.
           IF HELM-APPRAISE-YTD < 0.00
               MOVE 0.00 TO HELM-APPRAISE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HELM-APPRAISE-YTD > 10000.00
               MOVE 10000.00 TO HELM-APPRAISE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON BALANCE AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4090-ACCRUE-BALANCE.
           IF HELM-BALANCE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HELM-BALANCE-AMT * HELM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO HELM-DRAW-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4100-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HELM-TYPE-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HELM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * MIN PAY YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4110-FLAG-MIN-PAY.
           IF HELM-MIN-PAY-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE HELM-MIN-PAY-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E562 CUST ID REQUIRED
      * E420 MIN PAY YTD NEGATIVE
      * E900 MIN PAY YTD OVER 99999
      * E094 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       4120-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELM-CUST-ID = SPACES
                   MOVE 'E562' TO WS-REJECT-CODE
               WHEN HELM-MIN-PAY-YTD < 0
                   MOVE 'E420' TO WS-REJECT-CODE
               WHEN HELM-MIN-PAY-YTD > 99999.00
                   MOVE 'E900' TO WS-REJECT-CODE
               WHEN HELM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E094' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DRAW AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4130-SPLIT-DRAW.
           COMPUTE WS-WORK-AMT = HELM-DRAW-AMT / 12
           COMPUTE WS-WORK-AMT-2 = HELM-DRAW-AMT - (WS-WORK-AMT * 12)
           COMPUTE HELM-BALANCE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       4140-COUNT-EXCEPT.
           IF HELM-EXCEPT-CNT < 9999999
               ADD 1 TO HELM-EXCEPT-CNT
           END-IF
           IF HELM-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO HELM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE HEL FIELDS TO HEFEE AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7030-CALL-HEFEE.
           INITIALIZE HEFEE-PARMS
           MOVE HELM-INTEREST-AMT
             TO HEFEE-INTEREST-AMT
           MOVE HELM-ANNUAL-FEE-YTD
             TO HEFEE-ANNUAL-FEE-YTD
           MOVE HELM-DRAW-AMT
             TO HEFEE-DRAW-AMT
           MOVE HELM-BALANCE-YTD
             TO HEFEE-BALANCE-YTD
           MOVE HELM-ANNUAL-FEE-AMT
             TO HEFEE-ANNUAL-FEE-AMT
           MOVE HELM-INTEREST-YTD
             TO HEFEE-INTEREST-YTD
           MOVE HELM-OPEN-DATE
             TO HEFEE-OPEN-DATE
           MOVE HELM-NEXT-DUE-DATE
             TO HEFEE-NEXT-DUE-DATE
           MOVE HELM-LAST-PAY-DATE
             TO HEFEE-LAST-PAY-DATE
           MOVE HELM-NEXT-REVIEW-DATE
             TO HEFEE-NEXT-REVIEW-DATE
           MOVE HELM-REASON-CD
             TO HEFEE-REASON-CD
           MOVE HELM-DRAW-PERIOD-CD
             TO HEFEE-DRAW-PERIOD-CD
           MOVE HELM-TYPE-CD
             TO HEFEE-TYPE-CD
           MOVE HELM-STATUS-CD
             TO HEFEE-STATUS-CD
           MOVE HELM-HOLD-FLAG
             TO HEFEE-HOLD-FLAG
           MOVE HELM-PREFERRED-FLAG
             TO HEFEE-PREFERRED-FLAG
           MOVE HELM-NOTICE-SENT-FLAG
             TO HEFEE-NOTICE-SENT-FLAG
           MOVE HELM-NOTICE-CNT
             TO HEFEE-NOTICE-CNT
           MOVE HELM-RETRY-CNT
             TO HEFEE-RETRY-CNT
           MOVE HELM-PENALTY-PCT
             TO HEFEE-PENALTY-PCT
           MOVE HELM-DISCOUNT
             TO HEFEE-DISCOUNT
           MOVE HELM-CONTACT
             TO HEFEE-CONTACT
           MOVE HELM-NAME
             TO HEFEE-NAME
           CALL 'HEFEE' USING HEFEE-PARMS
           EVALUATE HEFEE-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE HEFEE-INTEREST-AMT
                     TO HELM-INTEREST-AMT
                   MOVE HEFEE-DRAW-AMT
                     TO HELM-DRAW-AMT
                   MOVE HEFEE-BALANCE-YTD
                     TO HELM-BALANCE-YTD
                   MOVE HEFEE-ANNUAL-FEE-AMT
                     TO HELM-ANNUAL-FEE-AMT
                   MOVE HEFEE-INTEREST-YTD
                     TO HELM-INTEREST-YTD
                   CONTINUE
               WHEN 08
                   MOVE 'RFEE' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7030-CALL-HEFEE' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE HELT-KEY TO RD-KEY
           MOVE HELT-TRAN-CD TO RD-CODE
           MOVE HELT-DRAW-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE HELT-KEY TO RD-KEY
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
           DISPLAY 'HELSETL READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'HELSETL' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
