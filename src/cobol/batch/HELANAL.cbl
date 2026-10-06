      *================================================================*
      * PROGRAM  : HELANAL                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : HOME EQUITY LINES - ANALYSIS. READS THE HEL         *
      *            TRANSACTIONS, UPDATES THE HEL MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. HOME EQUITY LINE DRAWS, BILLING,*
      *            RATE CHANGE AND FREEZE                              *
      * INPUT    : HELMST (KSDS), HELTRN, BANKCTL                      *
      * OUTPUT   : HELOUT, HELANRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, HELIM                   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-11-13 PQR  ORIGINAL                                       *
      * 1995-12-03 EWS  LIMIT FEE SCHEDULE UPDATE CHG0842              *
      * 1996-12-22 PQR  INTEREST NOTICE TEXT PER LEGAL REQ5162         *
      * 1996-04-17 LMB  PERFORMANCE - INTEREST READ AHEAD PRB3809      *
      * 1998-11-28 DPS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2001-06-17 TWH  BALANCE CUTOFF MOVED REQ3512                   *
      * 2004-05-02 JLM  DRAW EXCEPTIONS TO OPS QUEUE INC1399           *
      * 2005-02-22 BHS  BALANCE NOTICE TEXT PER LEGAL REQ0235          *
      * 2008-11-23 MKL  ANNUAL-FEE CUTOFF MOVED PRB1406                *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELANAL.
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
           SELECT REPORT-FILE ASSIGN TO "HELANRPT"
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
                                           VALUE 8.000000.
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
      * WS-DRAW-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-DRAW-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000502000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000902500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000200000000522000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002700000000050000'.
       01  WS-DRAW-GRID-TBL REDEFINES WS-DRAW-GRID-VALUES.
           05  WS-DRAW-GRID-ROW OCCURS 4 TIMES.
               10  WS-DRAW-GRID-LOW        PIC 9(09)V99.
               10  WS-DRAW-GRID-RATE       PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-MIN-PAY-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-MIN-PAY-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000056000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000275000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010250000000469500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010500000000782000'.
       01  WS-MIN-PAY-GRID-TBL REDEFINES WS-MIN-PAY-GRID-VALUES.
           05  WS-MIN-PAY-GRID-ROW OCCURS 4 TIMES.
               10  WS-MIN-PAY-GRID-LOW     PIC 9(09)V99.
               10  WS-MIN-PAY-GRID-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-DRAW-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-DRAW-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000052500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000196500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003000000000561500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003100000000163000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005600000000320500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005700000000093500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015700000000366000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020700000000870000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00030700000000434000'.
       01  WS-DRAW-SCHED-TBL REDEFINES WS-DRAW-SCHED-VALUES.
           05  WS-DRAW-SCHED-ROW OCCURS 9 TIMES.
               10  WS-DRAW-SCHED-LOW       PIC 9(09)V99.
               10  WS-DRAW-SCHED-RATE      PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-LIMIT-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-LIMIT-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000890500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000473500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000350000000841500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010350000000272500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010400000000075000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010900000000435000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015900000000511500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015950000000327500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016200000000440500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016700000000691000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016750000000632500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00019250000000892000'.
       01  WS-LIMIT-BAND-TBL REDEFINES WS-LIMIT-BAND-VALUES.
           05  WS-LIMIT-BAND-ROW OCCURS 12 TIMES.
               10  WS-LIMIT-BAND-LOW       PIC 9(09)V99.
               10  WS-LIMIT-BAND-RATE      PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  HELANAL  HOME EQUITY LINES'.
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
       COPY HELIMP.

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
               PERFORM 3000-ACCRUE-MIN-PAY
               PERFORM 3010-LIMIT-INTEREST
               PERFORM 3020-AGE-EFF
               PERFORM 3030-AGE-OPEN
               PERFORM 3040-COUNT-PAYMENT
               PERFORM 3050-CHECK-LAST-PAY
               PERFORM 3060-AGE-OPEN
               PERFORM 3070-EDIT-ADDR
               PERFORM 3080-ASSESS-SUSPEND
               PERFORM 3090-ACCUM-REGION
               PERFORM 3100-SELECT-PENALTY-PCT
               PERFORM 3110-DESCRIBE-LIEN-POS
               PERFORM 3120-SELECT-DISCOUNT
               PERFORM 3130-SPLIT-ANNUAL-FEE
               PERFORM 3140-ASSESS-PREFERRED
               PERFORM 3150-ACCUM-TYPE
               PERFORM 3160-EDIT-MEMO
               PERFORM 3170-LIMIT-ANNUAL-FEE
               PERFORM 3180-DESCRIBE-STATUS
               PERFORM 3190-ACCRUE-ANNUAL-FEE
               PERFORM 3200-DESCRIBE-DRAW-PERIOD
               PERFORM 3210-PRORATE-BALANCE
               PERFORM 3220-AGE-OPEN
               PERFORM 3230-ACCRUE-LIMIT
               PERFORM 3240-EDIT-MEMO
               PERFORM 3250-ACCRUE-ANNUAL-FEE
               PERFORM 3260-DESCRIBE-TYPE
               PERFORM 3270-ACCRUE-INTEREST
               PERFORM 3280-ASSESS-SUSPEND
               PERFORM 3290-ACCUM-TRAN
               PERFORM 3300-ACCUM-LIEN-POS
               PERFORM 3310-DESCRIBE-CHANNEL
               PERFORM 3320-SELECT-PENALTY-PCT
               PERFORM 3330-LIMIT-ANNUAL-FEE
               PERFORM 3340-AGE-LAST-PAY
               PERFORM 3350-SELECT-MARGIN
               PERFORM 3360-COUNT-PAYMENT
               PERFORM 3370-COUNT-RETRY
               PERFORM 3380-COUNT-NOTICE
               PERFORM 3390-AGE-MATURITY
               PERFORM 3400-PRORATE-ANNUAL-FEE
               PERFORM 3410-EDIT-CONTACT
               PERFORM 3420-CHECK-MATURITY
               PERFORM 3430-ACCRUE-LIMIT
               PERFORM 3440-PRORATE-MIN-PAY
               PERFORM 3450-EDIT-DESC
               PERFORM 3460-ASSESS-PREFERRED
               PERFORM 3470-AGE-LAST-MAINT
               PERFORM 3480-ACCUM-LIEN-POS
               PERFORM 3490-EDIT-DESC
               PERFORM 3500-CHECK-NEXT-REVIEW
               PERFORM 3510-DESCRIBE-LIEN-POS
               PERFORM 3520-CHECK-LAST-PAY
               PERFORM 3530-CHECK-LAST-MAINT
               PERFORM 3540-DESCRIBE-TYPE
               PERFORM 3550-AGE-OPEN
               PERFORM 3560-LIMIT-ANNUAL-FEE
               PERFORM 3570-LIMIT-MIN-PAY
               PERFORM 3580-SPLIT-LIMIT
               PERFORM 3590-LIMIT-LIMIT
               PERFORM 3600-DESCRIBE-STATUS
               PERFORM 3610-LIMIT-ITEM
               PERFORM 3620-EDIT-MEMO
               PERFORM 3630-SET-CHANNEL
               PERFORM 3640-SPLIT-APPRAISE
               PERFORM 3650-COUNT-NOTICE
               PERFORM 3660-AGE-POST
               PERFORM 3670-ACCUM-LIEN-POS
               PERFORM 3680-SELECT-MARGIN
               PERFORM 3690-SET-REASON
               PERFORM 3700-SPLIT-MIN-PAY
               PERFORM 3710-AGE-EFF
               PERFORM 3720-SPLIT-BALANCE
               PERFORM 3730-FLAG-APPRAISE
               PERFORM 3740-CHECK-CLOSE
               PERFORM 3750-ASSESS-NOTICE-SENT
               PERFORM 3760-SELECT-MARGIN
               PERFORM 3770-DESCRIBE-REASON
               PERFORM 3780-ACCRUE-APPRAISE
               PERFORM 3790-AGE-EFF
               PERFORM 3800-COUNT-SEQ
               PERFORM 3810-CHECK-NEXT-REVIEW
               PERFORM 3820-COUNT-RETRY
               PERFORM 3830-FLAG-ANNUAL-FEE
               PERFORM 3840-PRORATE-ANNUAL-FEE
               PERFORM 3850-AGE-LAST-MAINT
               PERFORM 3860-EDIT-CUST-ID
               PERFORM 3870-ASSESS-PREFERRED
               PERFORM 3880-LIMIT-MIN-PAY
               PERFORM 3890-PRORATE-DRAW
               PERFORM 3900-EDIT-CONTACT
               PERFORM 3910-SPLIT-ANNUAL-FEE
               PERFORM 3920-SELECT-DISCOUNT
               PERFORM 3930-SPLIT-DRAW
               PERFORM 3940-ASSESS-NOTICE-SENT
               PERFORM 3950-SPLIT-DRAW
               PERFORM 3960-LIMIT-APPRAISE
               PERFORM 3970-SELECT-DISCOUNT
               PERFORM 3980-COUNT-EXCEPT
               PERFORM 3990-ACCUM-CHANNEL
               PERFORM 4000-COUNT-EXCEPT
               PERFORM 4010-ACCUM-STATUS
               PERFORM 4020-ASSESS-NOTICE-SENT
               PERFORM 4030-AGE-EXPIRY
               PERFORM 4040-DESCRIBE-CHANNEL
               PERFORM 4050-PRORATE-LIMIT
               PERFORM 4060-AGE-OPEN
               PERFORM 4070-DESCRIBE-TRAN
               PERFORM 4080-SET-CHANNEL
               PERFORM 4090-SELECT-DISCOUNT
               PERFORM 4100-SPLIT-ANNUAL-FEE
               PERFORM 4110-SPLIT-APPRAISE
               PERFORM 4120-COUNT-NOTICE
               PERFORM 7010-CALL-HELIM
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
      * DAILY INTEREST ON MIN PAY YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3000-ACCRUE-MIN-PAY.
           IF HELM-MIN-PAY-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HELM-MIN-PAY-YTD * HELM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO HELM-LIMIT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INTEREST AMT IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3010-LIMIT-INTEREST.
           IF HELM-INTEREST-AMT < 10.00
               MOVE 10.00 TO HELM-INTEREST-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HELM-INTEREST-AMT > 500.00
               MOVE 500.00 TO HELM-INTEREST-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3020-AGE-EFF.
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
                   ADD HELM-INTEREST-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD HELM-INTEREST-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD HELM-INTEREST-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD HELM-INTEREST-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HELM-INTEREST-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3030-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF HELM-OPEN-DATE > 0 AND HELM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD HELM-BALANCE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD HELM-BALANCE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD HELM-BALANCE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD HELM-BALANCE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HELM-BALANCE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3040-COUNT-PAYMENT.
           IF HELM-PAYMENT-CNT < 9999999
               ADD 1 TO HELM-PAYMENT-CNT
           END-IF
           IF HELM-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO HELM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 365 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3050-CHECK-LAST-PAY.
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
           IF WS-DAYS > 365
               MOVE 'Y' TO HELM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3060-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF HELM-OPEN-DATE > 0 AND HELM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD HELT-LIMIT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD HELT-LIMIT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD HELT-LIMIT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD HELT-LIMIT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HELT-LIMIT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E693 ADDR REQUIRED
      * E207 INTEREST AMT NEGATIVE
      * E545 INTEREST AMT OVER 5000
      * E500 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3070-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELM-ADDR = SPACES
                   MOVE 'E693' TO WS-REJECT-CODE
               WHEN HELM-INTEREST-AMT < 0
                   MOVE 'E207' TO WS-REJECT-CODE
               WHEN HELM-INTEREST-AMT > 5000.00
                   MOVE 'E545' TO WS-REJECT-CODE
               WHEN HELM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E500' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE INTEREST YTD FEE ON THE MIN PAY YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3080-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN HELM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO HELM-INTEREST-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELM-HOLD-FLAG = 'Y'
                   COMPUTE HELM-INTEREST-YTD ROUNDED = HELM-MIN-PAY-YTD
                       * HELM-MARGIN / 200
               WHEN HELM-REGION-CD = 'BSD'
                   MOVE 12.00 TO HELM-INTEREST-YTD
               WHEN HELM-REGION-CD = 'HKW'
                   MOVE 7.00 TO HELM-INTEREST-YTD
               WHEN OTHER
                   COMPUTE HELM-INTEREST-YTD ROUNDED = HELM-MIN-PAY-YTD
                       * HELM-MARGIN / 100
           END-EVALUATE
           IF HELM-INTEREST-YTD > 0 AND HELM-INTEREST-YTD < 1.00
               MOVE 1.00 TO HELM-INTEREST-YTD
           END-IF
           IF HELM-INTEREST-YTD > 35.00
               MOVE 35.00 TO HELM-INTEREST-YTD
           END-IF
           ADD HELM-INTEREST-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE BALANCE AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3090-ACCUM-REGION.
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
      * SELECT THE DRAW SCHED TIER FOR THE BALANCE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3100-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DRAW-SCHED-RATE(1) TO HELM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF HELM-BALANCE-YTD NOT < WS-DRAW-SCHED-LOW(WS-TX)
                   MOVE WS-DRAW-SCHED-RATE(WS-TX) TO HELM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE LIEN POS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3110-DESCRIBE-LIEN-POS.
           EVALUATE HELM-LIEN-POS-CD
               WHEN 'AXX'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'PVA'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'SFC'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE MIN PAY GRID TIER FOR THE DRAW AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3120-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-MIN-PAY-GRID-RATE(1) TO HELM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF HELM-DRAW-AMT NOT < WS-MIN-PAY-GRID-LOW(WS-TX)
                   MOVE WS-MIN-PAY-GRID-RATE(WS-TX) TO HELM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ANNUAL FEE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3130-SPLIT-ANNUAL-FEE.
           COMPUTE WS-WORK-AMT = HELT-ANNUAL-FEE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = HELT-ANNUAL-FEE-AMT - (WS-WORK-AMT *
               4)
           COMPUTE HELT-LIMIT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ANNUAL FEE YTD FEE ON THE BALANCE YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3140-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN HELM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO HELM-ANNUAL-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELM-HOLD-FLAG = 'Y'
                   COMPUTE HELM-ANNUAL-FEE-YTD ROUNDED =
                       HELM-BALANCE-YTD * HELM-DISCOUNT / 200
               WHEN HELT-CHANNEL-CD = 'BRN'
                   MOVE 3.00 TO HELM-ANNUAL-FEE-YTD
               WHEN HELT-CHANNEL-CD = 'BAT'
                   MOVE 3.00 TO HELM-ANNUAL-FEE-YTD
               WHEN OTHER
                   COMPUTE HELM-ANNUAL-FEE-YTD ROUNDED =
                       HELM-BALANCE-YTD * HELM-DISCOUNT / 100
           END-EVALUATE
           IF HELM-ANNUAL-FEE-YTD > 0 AND HELM-ANNUAL-FEE-YTD < 1.00
               MOVE 1.00 TO HELM-ANNUAL-FEE-YTD
           END-IF
           IF HELM-ANNUAL-FEE-YTD > 250.00
               MOVE 250.00 TO HELM-ANNUAL-FEE-YTD
           END-IF
           ADD HELM-ANNUAL-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE APPRAISE AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3150-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HELM-TYPE-CD
                   ADD HELT-APPRAISE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HELM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE HELT-APPRAISE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E241 MEMO REQUIRED
      * E371 APPRAISE YTD NEGATIVE
      * E492 APPRAISE YTD OVER 10000
      * E509 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3160-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELT-MEMO = SPACES
                   MOVE 'E241' TO WS-REJECT-CODE
               WHEN HELM-APPRAISE-YTD < 0
                   MOVE 'E371' TO WS-REJECT-CODE
               WHEN HELM-APPRAISE-YTD > 10000.00
                   MOVE 'E492' TO WS-REJECT-CODE
               WHEN HELM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E509' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ANNUAL FEE YTD IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3170-LIMIT-ANNUAL-FEE.
           IF HELM-ANNUAL-FEE-YTD < 50.00
               MOVE 50.00 TO HELM-ANNUAL-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HELM-ANNUAL-FEE-YTD > 1000.00
               MOVE 1000.00 TO HELM-ANNUAL-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3180-DESCRIBE-STATUS.
           EVALUATE HELM-STATUS-CD
               WHEN 'GR'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'JC'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'KH'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'KN'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'PC'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'UZ'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ANNUAL FEE AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3190-ACCRUE-ANNUAL-FEE.
           IF HELM-ANNUAL-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HELM-ANNUAL-FEE-AMT * HELM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO HELT-DRAW-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE DRAW PERIOD CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3200-DESCRIBE-DRAW-PERIOD.
           EVALUATE HELM-DRAW-PERIOD-CD
               WHEN 'CCV'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'JSU'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'KXA'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'VMR'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'WLJ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'XRA'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE INTEREST AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO BALANCE YTD.
      *----------------------------------------------------------------*
       3210-PRORATE-BALANCE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HELM-BALANCE-YTD ROUNDED =
                   HELM-INTEREST-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO HELM-BALANCE-YTD
           END-IF
           ADD HELM-BALANCE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3220-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF HELM-OPEN-DATE > 0 AND HELM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELM-OPEN-DATE TO DTU-DATE-1
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
      * DAILY INTEREST ON LIMIT AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3230-ACCRUE-LIMIT.
           IF HELM-LIMIT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HELM-LIMIT-AMT * HELM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO HELM-INTEREST-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E045 MEMO REQUIRED
      * E456 DRAW AMT NEGATIVE
      * E057 DRAW AMT OVER 10000
      * E428 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3240-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELT-MEMO = SPACES
                   MOVE 'E045' TO WS-REJECT-CODE
               WHEN HELM-DRAW-AMT < 0
                   MOVE 'E456' TO WS-REJECT-CODE
               WHEN HELM-DRAW-AMT > 10000.00
                   MOVE 'E057' TO WS-REJECT-CODE
               WHEN HELM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E428' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ANNUAL FEE AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3250-ACCRUE-ANNUAL-FEE.
           IF HELM-ANNUAL-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HELM-ANNUAL-FEE-AMT * HELM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO HELM-INTEREST-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3260-DESCRIBE-TYPE.
           EVALUATE HELM-TYPE-CD
               WHEN 'CD'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'JW'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'KV'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'WS'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'XL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'ZV'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INTEREST AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3270-ACCRUE-INTEREST.
           IF HELM-INTEREST-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HELM-INTEREST-AMT * HELM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO HELM-INTEREST-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE INTEREST FEE ON THE DRAW AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3280-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN HELM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO HELM-INTEREST-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE HELM-INTEREST-AMT ROUNDED = HELM-DRAW-AMT *
                       HELM-MARGIN / 200
               WHEN HELM-REASON-CD = 'KST'
                   MOVE 7.00 TO HELM-INTEREST-AMT
               WHEN HELM-REASON-CD = 'NWS'
                   MOVE 5.00 TO HELM-INTEREST-AMT
               WHEN OTHER
                   COMPUTE HELM-INTEREST-AMT ROUNDED = HELM-DRAW-AMT *
                       HELM-MARGIN / 100
           END-EVALUATE
           IF HELM-INTEREST-AMT > 0 AND HELM-INTEREST-AMT < 5.00
               MOVE 5.00 TO HELM-INTEREST-AMT
           END-IF
           IF HELM-INTEREST-AMT > 25.00
               MOVE 25.00 TO HELM-INTEREST-AMT
           END-IF
           ADD HELM-INTEREST-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE LIMIT AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3290-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HELT-TRAN-CD
                   ADD HELT-LIMIT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HELT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE HELT-LIMIT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE INTEREST AMT BY LIEN POS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3300-ACCUM-LIEN-POS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HELM-LIEN-POS-CD
                   ADD HELM-INTEREST-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HELM-LIEN-POS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE HELM-INTEREST-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3310-DESCRIBE-CHANNEL.
           EVALUATE HELT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE DRAW GRID TIER FOR THE APPRAISE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3320-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DRAW-GRID-RATE(1) TO HELM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF HELM-APPRAISE-YTD NOT < WS-DRAW-GRID-LOW(WS-TX)
                   MOVE WS-DRAW-GRID-RATE(WS-TX) TO HELM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ANNUAL FEE AMT IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3330-LIMIT-ANNUAL-FEE.
           IF HELM-ANNUAL-FEE-AMT < 0.00
               MOVE 0.00 TO HELM-ANNUAL-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HELM-ANNUAL-FEE-AMT > 10000.00
               MOVE 10000.00 TO HELM-ANNUAL-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3340-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF HELM-LAST-PAY-DATE > 0 AND HELM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD HELM-ANNUAL-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD HELM-ANNUAL-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD HELM-ANNUAL-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD HELM-ANNUAL-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HELM-ANNUAL-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE DRAW SCHED TIER FOR THE APPRAISE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3350-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-DRAW-SCHED-RATE(1) TO HELM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF HELM-APPRAISE-YTD NOT < WS-DRAW-SCHED-LOW(WS-TX)
                   MOVE WS-DRAW-SCHED-RATE(WS-TX) TO HELM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3360-COUNT-PAYMENT.
           IF HELM-PAYMENT-CNT < 9999999
               ADD 1 TO HELM-PAYMENT-CNT
           END-IF
           IF HELM-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO HELT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3370-COUNT-RETRY.
           IF HELM-RETRY-CNT < 9999999
               ADD 1 TO HELM-RETRY-CNT
           END-IF
           IF HELM-RETRY-CNT NOT < 12
               MOVE 'Y' TO HELM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3380-COUNT-NOTICE.
           IF HELM-NOTICE-CNT < 9999999
               ADD 1 TO HELM-NOTICE-CNT
           END-IF
           IF HELM-NOTICE-CNT NOT < 3
               MOVE 'Y' TO HELM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3390-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF HELM-MATURITY-DATE > 0 AND HELM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD HELT-APPRAISE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD HELT-APPRAISE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD HELT-APPRAISE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD HELT-APPRAISE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HELT-APPRAISE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE BALANCE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ANNUAL FEE AMT.
      *----------------------------------------------------------------*
       3400-PRORATE-ANNUAL-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HELT-ANNUAL-FEE-AMT ROUNDED =
                   HELM-BALANCE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO HELT-ANNUAL-FEE-AMT
           END-IF
           ADD HELT-ANNUAL-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E028 CONTACT REQUIRED
      * E713 ANNUAL FEE AMT NEGATIVE
      * E677 ANNUAL FEE AMT OVER 10000
      * E618 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3410-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELM-CONTACT = SPACES
                   MOVE 'E028' TO WS-REJECT-CODE
               WHEN HELM-ANNUAL-FEE-AMT < 0
                   MOVE 'E713' TO WS-REJECT-CODE
               WHEN HELM-ANNUAL-FEE-AMT > 10000.00
                   MOVE 'E677' TO WS-REJECT-CODE
               WHEN HELM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E618' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3420-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF HELM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO HELT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON LIMIT AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3430-ACCRUE-LIMIT.
           IF HELM-LIMIT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HELM-LIMIT-AMT * HELM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO HELM-DRAW-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE BALANCE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO MIN PAY AMT.
      *----------------------------------------------------------------*
       3440-PRORATE-MIN-PAY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HELM-MIN-PAY-AMT ROUNDED =
                   HELM-BALANCE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO HELM-MIN-PAY-AMT
           END-IF
           ADD HELM-MIN-PAY-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E503 DESC REQUIRED
      * E454 APPRAISE AMT NEGATIVE
      * E189 APPRAISE AMT OVER 25000
      * E949 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3450-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELM-DESC = SPACES
                   MOVE 'E503' TO WS-REJECT-CODE
               WHEN HELT-APPRAISE-AMT < 0
                   MOVE 'E454' TO WS-REJECT-CODE
               WHEN HELT-APPRAISE-AMT > 25000.00
                   MOVE 'E189' TO WS-REJECT-CODE
               WHEN HELM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E949' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ANNUAL FEE FEE ON THE MIN PAY AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3460-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN HELM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO HELM-ANNUAL-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELM-OVERRIDE-FLAG = 'Y'
                   COMPUTE HELM-ANNUAL-FEE-AMT ROUNDED =
                       HELM-MIN-PAY-AMT * HELM-PENALTY-PCT / 200
               WHEN HELM-REGION-CD = 'BSD'
                   MOVE 3.00 TO HELM-ANNUAL-FEE-AMT
               WHEN HELM-REGION-CD = 'HKW'
                   MOVE 5.00 TO HELM-ANNUAL-FEE-AMT
               WHEN OTHER
                   COMPUTE HELM-ANNUAL-FEE-AMT ROUNDED =
                       HELM-MIN-PAY-AMT * HELM-PENALTY-PCT / 100
           END-EVALUATE
           IF HELM-ANNUAL-FEE-AMT > 0 AND HELM-ANNUAL-FEE-AMT < 5.00
               MOVE 5.00 TO HELM-ANNUAL-FEE-AMT
           END-IF
           IF HELM-ANNUAL-FEE-AMT > 100.00
               MOVE 100.00 TO HELM-ANNUAL-FEE-AMT
           END-IF
           ADD HELM-ANNUAL-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3470-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF HELM-LAST-MAINT-DATE > 0 AND HELM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD HELT-ANNUAL-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD HELT-ANNUAL-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD HELT-ANNUAL-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD HELT-ANNUAL-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HELT-ANNUAL-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE APPRAISE YTD BY LIEN POS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3480-ACCUM-LIEN-POS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HELM-LIEN-POS-CD
                   ADD HELM-APPRAISE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HELM-LIEN-POS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE HELM-APPRAISE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E058 DESC REQUIRED
      * E658 WORK AMT NEGATIVE
      * E149 WORK AMT OVER 25000
      * E798 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3490-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELM-DESC = SPACES
                   MOVE 'E058' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E658' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 25000.00
                   MOVE 'E149' TO WS-REJECT-CODE
               WHEN HELM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E798' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 60 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3500-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF HELM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO HELM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE LIEN POS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3510-DESCRIBE-LIEN-POS.
           EVALUATE HELM-LIEN-POS-CD
               WHEN 'AXX'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'PVA'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'SFC'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 45 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3520-CHECK-LAST-PAY.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO HELM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 60 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3530-CHECK-LAST-MAINT.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO HELM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3540-DESCRIBE-TYPE.
           EVALUATE HELM-TYPE-CD
               WHEN 'CD'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'JW'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'KV'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'WS'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'XL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'ZV'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3550-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF HELM-OPEN-DATE > 0 AND HELM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD HELM-INTEREST-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD HELM-INTEREST-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD HELM-INTEREST-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD HELM-INTEREST-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HELM-INTEREST-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ANNUAL FEE AMT IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3560-LIMIT-ANNUAL-FEE.
           IF HELM-ANNUAL-FEE-AMT < 25.00
               MOVE 25.00 TO HELM-ANNUAL-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HELM-ANNUAL-FEE-AMT > 2500.00
               MOVE 2500.00 TO HELM-ANNUAL-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * MIN PAY AMT IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3570-LIMIT-MIN-PAY.
           IF HELM-MIN-PAY-AMT < 0.00
               MOVE 0.00 TO HELM-MIN-PAY-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HELM-MIN-PAY-AMT > 5000.00
               MOVE 5000.00 TO HELM-MIN-PAY-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT LIMIT AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3580-SPLIT-LIMIT.
           COMPUTE WS-WORK-AMT = HELT-LIMIT-AMT / 3
           COMPUTE WS-WORK-AMT-2 = HELT-LIMIT-AMT - (WS-WORK-AMT * 3)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * LIMIT AMT IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3590-LIMIT-LIMIT.
           IF HELT-LIMIT-AMT < 0.00
               MOVE 0.00 TO HELT-LIMIT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HELT-LIMIT-AMT > 50000.00
               MOVE 50000.00 TO HELT-LIMIT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3600-DESCRIBE-STATUS.
           EVALUATE HELM-STATUS-CD
               WHEN 'GR'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'JC'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'KH'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'KN'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'PC'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'UZ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3610-LIMIT-ITEM.
           IF WS-WORK-AMT < 100.00
               MOVE 100.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 50000.00
               MOVE 50000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E179 MEMO REQUIRED
      * E708 APPRAISE AMT NEGATIVE
      * E033 APPRAISE AMT OVER 5000
      * E600 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3620-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELT-MEMO = SPACES
                   MOVE 'E179' TO WS-REJECT-CODE
               WHEN HELT-APPRAISE-AMT < 0
                   MOVE 'E708' TO WS-REJECT-CODE
               WHEN HELT-APPRAISE-AMT > 5000.00
                   MOVE 'E033' TO WS-REJECT-CODE
               WHEN HELT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E600' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN MATURITY DATE IS PAST
      * BAT -> MAL WHEN MATURITY DATE IS PAST
      * MAL -> TEL WHEN MATURITY DATE IS PAST
      * TEL -> ONL WHEN MATURITY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3630-SET-CHANNEL.
           EVALUATE HELT-CHANNEL-CD
               WHEN 'BRN'
                   IF HELM-MATURITY-DATE > 0 AND HELM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO HELT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF HELM-MATURITY-DATE > 0 AND HELM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO HELT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF HELM-MATURITY-DATE > 0 AND HELM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO HELT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF HELM-MATURITY-DATE > 0 AND HELM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO HELT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT APPRAISE YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3640-SPLIT-APPRAISE.
           COMPUTE WS-WORK-AMT = HELM-APPRAISE-YTD / 4
           COMPUTE WS-WORK-AMT-2 = HELM-APPRAISE-YTD - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3650-COUNT-NOTICE.
           IF HELM-NOTICE-CNT < 9999999
               ADD 1 TO HELM-NOTICE-CNT
           END-IF
           IF HELM-NOTICE-CNT NOT < 10
               MOVE 'Y' TO HELM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3660-AGE-POST.
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
                   ADD HELM-BALANCE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD HELM-BALANCE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD HELM-BALANCE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD HELM-BALANCE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HELM-BALANCE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE APPRAISE AMT BY LIEN POS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3670-ACCUM-LIEN-POS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HELM-LIEN-POS-CD
                   ADD HELT-APPRAISE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HELM-LIEN-POS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE HELT-APPRAISE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE MIN PAY GRID TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3680-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-MIN-PAY-GRID-RATE(1) TO HELM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF WS-WORK-AMT-2 NOT < WS-MIN-PAY-GRID-LOW(WS-TX)
                   MOVE WS-MIN-PAY-GRID-RATE(WS-TX) TO HELM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * KST -> NWS WHEN NEXT DUE DATE IS PAST
      * NWS -> ZJM WHEN NEXT DUE DATE IS PAST
      * ZJM -> KST WHEN NEXT DUE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3690-SET-REASON.
           EVALUATE HELM-REASON-CD
               WHEN 'KST'
                   IF HELM-NEXT-DUE-DATE > 0 AND HELM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NWS' TO HELM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NWS'
                   IF HELM-NEXT-DUE-DATE > 0 AND HELM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZJM' TO HELM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZJM'
                   IF HELM-NEXT-DUE-DATE > 0 AND HELM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KST' TO HELM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT MIN PAY YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3700-SPLIT-MIN-PAY.
           COMPUTE WS-WORK-AMT = HELM-MIN-PAY-YTD / 3
           COMPUTE WS-WORK-AMT-2 = HELM-MIN-PAY-YTD - (WS-WORK-AMT * 3)
           COMPUTE HELM-INTEREST-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3710-AGE-EFF.
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
                   ADD HELM-BALANCE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD HELM-BALANCE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD HELM-BALANCE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD HELM-BALANCE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HELM-BALANCE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT BALANCE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3720-SPLIT-BALANCE.
           COMPUTE WS-WORK-AMT = HELM-BALANCE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = HELM-BALANCE-AMT - (WS-WORK-AMT * 4)
           COMPUTE HELM-APPRAISE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * APPRAISE YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3730-FLAG-APPRAISE.
           IF HELM-APPRAISE-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE HELM-APPRAISE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3740-CHECK-CLOSE.
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
           IF WS-DAYS > 90
               MOVE 'Y' TO HELM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE MIN PAY FEE ON THE BALANCE AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3750-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN HELM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO HELM-MIN-PAY-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELM-PREFERRED-FLAG = 'Y'
                   COMPUTE HELM-MIN-PAY-AMT ROUNDED = HELM-BALANCE-AMT *
                       HELM-PENALTY-PCT / 200
               WHEN HELT-CHANNEL-CD = 'BRN'
                   MOVE 15.00 TO HELM-MIN-PAY-AMT
               WHEN HELT-CHANNEL-CD = 'BAT'
                   MOVE 5.00 TO HELM-MIN-PAY-AMT
               WHEN OTHER
                   COMPUTE HELM-MIN-PAY-AMT ROUNDED = HELM-BALANCE-AMT *
                       HELM-PENALTY-PCT / 100
           END-EVALUATE
           IF HELM-MIN-PAY-AMT > 0 AND HELM-MIN-PAY-AMT < 10.00
               MOVE 10.00 TO HELM-MIN-PAY-AMT
           END-IF
           IF HELM-MIN-PAY-AMT > 250.00
               MOVE 250.00 TO HELM-MIN-PAY-AMT
           END-IF
           ADD HELM-MIN-PAY-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE DRAW GRID TIER FOR THE DRAW AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3760-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-DRAW-GRID-RATE(1) TO HELM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF HELT-DRAW-AMT NOT < WS-DRAW-GRID-LOW(WS-TX)
                   MOVE WS-DRAW-GRID-RATE(WS-TX) TO HELM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3770-DESCRIBE-REASON.
           EVALUATE HELM-REASON-CD
               WHEN 'KST'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'NWS'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ZJM'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON APPRAISE AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3780-ACCRUE-APPRAISE.
           IF HELM-APPRAISE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   HELM-APPRAISE-AMT * HELM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO HELT-DRAW-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-90, 91-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3790-AGE-EFF.
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
                   ADD HELT-LIMIT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD HELT-LIMIT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD HELT-LIMIT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD HELT-LIMIT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HELT-LIMIT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3800-COUNT-SEQ.
           IF HELT-SEQ < 9999999
               ADD 1 TO HELT-SEQ
           END-IF
           IF HELT-SEQ NOT < 6
               MOVE 'Y' TO HELM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 120 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3810-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF HELM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO HELM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3820-COUNT-RETRY.
           IF HELM-RETRY-CNT < 9999999
               ADD 1 TO HELM-RETRY-CNT
           END-IF
           IF HELM-RETRY-CNT NOT < 12
               MOVE 'Y' TO HELM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ANNUAL FEE AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3830-FLAG-ANNUAL-FEE.
           IF HELM-ANNUAL-FEE-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE HELM-ANNUAL-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ANNUAL FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ANNUAL FEE AMT.
      *----------------------------------------------------------------*
       3840-PRORATE-ANNUAL-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HELT-ANNUAL-FEE-AMT ROUNDED =
                   HELM-ANNUAL-FEE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO HELT-ANNUAL-FEE-AMT
           END-IF
           ADD HELT-ANNUAL-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3850-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF HELM-LAST-MAINT-DATE > 0 AND HELM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD HELM-LIMIT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD HELM-LIMIT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD HELM-LIMIT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD HELM-LIMIT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HELM-LIMIT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E094 CUST ID REQUIRED
      * E032 APPRAISE AMT NEGATIVE
      * E675 APPRAISE AMT OVER 25000
      * E279 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3860-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELM-CUST-ID = SPACES
                   MOVE 'E094' TO WS-REJECT-CODE
               WHEN HELM-APPRAISE-AMT < 0
                   MOVE 'E032' TO WS-REJECT-CODE
               WHEN HELM-APPRAISE-AMT > 25000.00
                   MOVE 'E675' TO WS-REJECT-CODE
               WHEN HELM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E279' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE INTEREST YTD FEE ON THE LIMIT AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3870-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN HELM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO HELM-INTEREST-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE HELM-INTEREST-YTD ROUNDED = HELT-LIMIT-AMT *
                       HELM-MARGIN / 200
               WHEN HELM-DRAW-PERIOD-CD = 'CCV'
                   MOVE 15.00 TO HELM-INTEREST-YTD
               WHEN HELM-DRAW-PERIOD-CD = 'JSU'
                   MOVE 7.00 TO HELM-INTEREST-YTD
               WHEN OTHER
                   COMPUTE HELM-INTEREST-YTD ROUNDED = HELT-LIMIT-AMT *
                       HELM-MARGIN / 100
           END-EVALUATE
           IF HELM-INTEREST-YTD > 0 AND HELM-INTEREST-YTD < 5.00
               MOVE 5.00 TO HELM-INTEREST-YTD
           END-IF
           IF HELM-INTEREST-YTD > 50.00
               MOVE 50.00 TO HELM-INTEREST-YTD
           END-IF
           ADD HELM-INTEREST-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * MIN PAY AMT IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3880-LIMIT-MIN-PAY.
           IF HELM-MIN-PAY-AMT < 100.00
               MOVE 100.00 TO HELM-MIN-PAY-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HELM-MIN-PAY-AMT > 50000.00
               MOVE 50000.00 TO HELM-MIN-PAY-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DRAW YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3890-PRORATE-DRAW.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   HELM-DRAW-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E216 CONTACT REQUIRED
      * E107 LIMIT AMT NEGATIVE
      * E242 LIMIT AMT OVER 250000
      * E783 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3900-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN HELM-CONTACT = SPACES
                   MOVE 'E216' TO WS-REJECT-CODE
               WHEN HELM-LIMIT-AMT < 0
                   MOVE 'E107' TO WS-REJECT-CODE
               WHEN HELM-LIMIT-AMT > 250000.00
                   MOVE 'E242' TO WS-REJECT-CODE
               WHEN HELM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E783' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ANNUAL FEE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3910-SPLIT-ANNUAL-FEE.
           COMPUTE WS-WORK-AMT = HELM-ANNUAL-FEE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = HELM-ANNUAL-FEE-AMT - (WS-WORK-AMT *
               4)
           COMPUTE HELM-APPRAISE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DRAW SCHED TIER FOR THE MIN PAY YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3920-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-DRAW-SCHED-RATE(1) TO HELM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF HELM-MIN-PAY-YTD NOT < WS-DRAW-SCHED-LOW(WS-TX)
                   MOVE WS-DRAW-SCHED-RATE(WS-TX) TO HELM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DRAW AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3930-SPLIT-DRAW.
           COMPUTE WS-WORK-AMT = HELM-DRAW-AMT / 4
           COMPUTE WS-WORK-AMT-2 = HELM-DRAW-AMT - (WS-WORK-AMT * 4)
           COMPUTE HELM-MIN-PAY-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE LIMIT YTD FEE ON THE BALANCE AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3940-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN HELM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO HELM-LIMIT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELM-PREFERRED-FLAG = 'Y'
                   COMPUTE HELM-LIMIT-YTD ROUNDED = HELM-BALANCE-AMT *
                       HELM-DISCOUNT / 200
               WHEN HELT-CHANNEL-CD = 'BRN'
                   MOVE 15.00 TO HELM-LIMIT-YTD
               WHEN HELT-CHANNEL-CD = 'BAT'
                   MOVE 12.00 TO HELM-LIMIT-YTD
               WHEN OTHER
                   COMPUTE HELM-LIMIT-YTD ROUNDED = HELM-BALANCE-AMT *
                       HELM-DISCOUNT / 100
           END-EVALUATE
           IF HELM-LIMIT-YTD > 0 AND HELM-LIMIT-YTD < 5.00
               MOVE 5.00 TO HELM-LIMIT-YTD
           END-IF
           IF HELM-LIMIT-YTD > 250.00
               MOVE 250.00 TO HELM-LIMIT-YTD
           END-IF
           ADD HELM-LIMIT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT DRAW YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3950-SPLIT-DRAW.
           COMPUTE WS-WORK-AMT = HELM-DRAW-YTD / 12
           COMPUTE WS-WORK-AMT-2 = HELM-DRAW-YTD - (WS-WORK-AMT * 12)
           COMPUTE HELM-MIN-PAY-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * APPRAISE AMT IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3960-LIMIT-APPRAISE.
           IF HELM-APPRAISE-AMT < 50.00
               MOVE 50.00 TO HELM-APPRAISE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF HELM-APPRAISE-AMT > 5000.00
               MOVE 5000.00 TO HELM-APPRAISE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE LIMIT BAND TIER FOR THE INTEREST AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3970-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-LIMIT-BAND-RATE(1) TO HELM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF HELM-INTEREST-AMT NOT < WS-LIMIT-BAND-LOW(WS-TX)
                   MOVE WS-LIMIT-BAND-RATE(WS-TX) TO HELM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3980-COUNT-EXCEPT.
           IF HELM-EXCEPT-CNT < 9999999
               ADD 1 TO HELM-EXCEPT-CNT
           END-IF
           IF HELM-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO HELM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE LIMIT AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3990-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HELT-CHANNEL-CD
                   ADD HELT-LIMIT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HELT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE HELT-LIMIT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       4000-COUNT-EXCEPT.
           IF HELM-EXCEPT-CNT < 9999999
               ADD 1 TO HELM-EXCEPT-CNT
           END-IF
           IF HELM-EXCEPT-CNT NOT < 12
               MOVE 'Y' TO HELM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE BALANCE YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4010-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = HELM-STATUS-CD
                   ADD HELM-BALANCE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE HELM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE HELM-BALANCE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE MIN PAY YTD FEE ON THE LIMIT YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       4020-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN HELM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO HELM-MIN-PAY-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN HELM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE HELM-MIN-PAY-YTD ROUNDED = HELM-LIMIT-YTD *
                       HELM-MARGIN / 200
               WHEN HELM-REASON-CD = 'KST'
                   MOVE 5.00 TO HELM-MIN-PAY-YTD
               WHEN HELM-REASON-CD = 'NWS'
                   MOVE 12.00 TO HELM-MIN-PAY-YTD
               WHEN OTHER
                   COMPUTE HELM-MIN-PAY-YTD ROUNDED = HELM-LIMIT-YTD *
                       HELM-MARGIN / 100
           END-EVALUATE
           IF HELM-MIN-PAY-YTD > 0 AND HELM-MIN-PAY-YTD < 1.00
               MOVE 1.00 TO HELM-MIN-PAY-YTD
           END-IF
           IF HELM-MIN-PAY-YTD > 250.00
               MOVE 250.00 TO HELM-MIN-PAY-YTD
           END-IF
           ADD HELM-MIN-PAY-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4030-AGE-EXPIRY.
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
                   ADD HELM-ANNUAL-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD HELM-ANNUAL-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD HELM-ANNUAL-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD HELM-ANNUAL-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HELM-ANNUAL-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4040-DESCRIBE-CHANNEL.
           EVALUATE HELT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE LIMIT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO LIMIT AMT.
      *----------------------------------------------------------------*
       4050-PRORATE-LIMIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE HELT-LIMIT-AMT ROUNDED =
                   HELM-LIMIT-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO HELT-LIMIT-AMT
           END-IF
           ADD HELT-LIMIT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4060-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF HELM-OPEN-DATE > 0 AND HELM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE HELM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD HELM-LIMIT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD HELM-LIMIT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD HELM-LIMIT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD HELM-LIMIT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD HELM-LIMIT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4070-DESCRIBE-TRAN.
           EVALUATE HELT-TRAN-CD
               WHEN 'DR'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'PY'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'FZ'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'RC'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'AF'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN EXPIRY DATE IS PAST
      * BAT -> MAL WHEN EXPIRY DATE IS PAST
      * MAL -> TEL WHEN EXPIRY DATE IS PAST
      * TEL -> ONL WHEN EXPIRY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4080-SET-CHANNEL.
           EVALUATE HELT-CHANNEL-CD
               WHEN 'BRN'
                   IF HELM-EXPIRY-DATE > 0 AND HELM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO HELT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF HELM-EXPIRY-DATE > 0 AND HELM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO HELT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF HELM-EXPIRY-DATE > 0 AND HELM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO HELT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF HELM-EXPIRY-DATE > 0 AND HELM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO HELT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE DRAW GRID TIER FOR THE APPRAISE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4090-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-DRAW-GRID-RATE(1) TO HELM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF HELM-APPRAISE-YTD NOT < WS-DRAW-GRID-LOW(WS-TX)
                   MOVE WS-DRAW-GRID-RATE(WS-TX) TO HELM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF HELM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO HELM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ANNUAL FEE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4100-SPLIT-ANNUAL-FEE.
           COMPUTE WS-WORK-AMT = HELT-ANNUAL-FEE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = HELT-ANNUAL-FEE-AMT - (WS-WORK-AMT *
               3)
           COMPUTE HELT-APPRAISE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT APPRAISE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4110-SPLIT-APPRAISE.
           COMPUTE WS-WORK-AMT = HELM-APPRAISE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = HELM-APPRAISE-AMT - (WS-WORK-AMT *
               12)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       4120-COUNT-NOTICE.
           IF HELM-NOTICE-CNT < 9999999
               ADD 1 TO HELM-NOTICE-CNT
           END-IF
           IF HELM-NOTICE-CNT NOT < 6
               MOVE 'Y' TO HELM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE HEL FIELDS TO HELIM AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7010-CALL-HELIM.
           INITIALIZE HELIM-PARMS
           MOVE HELM-BALANCE-YTD
             TO HELIM-BALANCE-YTD
           MOVE HELM-APPRAISE-YTD
             TO HELIM-APPRAISE-YTD
           MOVE HELM-ANNUAL-FEE-AMT
             TO HELIM-ANNUAL-FEE-AMT
           MOVE HELM-APPRAISE-AMT
             TO HELIM-APPRAISE-AMT
           MOVE HELM-INTEREST-YTD
             TO HELIM-INTEREST-YTD
           MOVE HELM-MIN-PAY-AMT
             TO HELIM-MIN-PAY-AMT
           MOVE HELM-EFFECTIVE-DATE
             TO HELIM-EFFECTIVE-DATE
           MOVE HELM-LAST-PAY-DATE
             TO HELIM-LAST-PAY-DATE
           MOVE HELM-NEXT-DUE-DATE
             TO HELIM-NEXT-DUE-DATE
           MOVE HELM-MATURITY-DATE
             TO HELIM-MATURITY-DATE
           MOVE HELM-DRAW-PERIOD-CD
             TO HELIM-DRAW-PERIOD-CD
           MOVE HELM-STATUS-CD
             TO HELIM-STATUS-CD
           MOVE HELM-TYPE-CD
             TO HELIM-TYPE-CD
           MOVE HELM-LIEN-POS-CD
             TO HELIM-LIEN-POS-CD
           MOVE HELM-AUTO-RENEW-FLAG
             TO HELIM-AUTO-RENEW-FLAG
           MOVE HELM-OVERRIDE-FLAG
             TO HELIM-OVERRIDE-FLAG
           MOVE HELM-HOLD-FLAG
             TO HELIM-HOLD-FLAG
           MOVE HELM-PAYMENT-CNT
             TO HELIM-PAYMENT-CNT
           MOVE HELM-EXCEPT-CNT
             TO HELIM-EXCEPT-CNT
           MOVE HELM-PENALTY-PCT
             TO HELIM-PENALTY-PCT
           MOVE HELM-DISCOUNT
             TO HELIM-DISCOUNT
           MOVE HELM-REF-NO
             TO HELIM-REF-NO
           MOVE HELM-DESC
             TO HELIM-DESC
           CALL 'HELIM' USING HELIM-PARMS
           EVALUATE HELIM-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE HELIM-APPRAISE-YTD
                     TO HELM-APPRAISE-YTD
                   MOVE HELIM-APPRAISE-AMT
                     TO HELM-APPRAISE-AMT
                   MOVE HELIM-INTEREST-YTD
                     TO HELM-INTEREST-YTD
                   MOVE HELIM-MIN-PAY-AMT
                     TO HELM-MIN-PAY-AMT
                   MOVE HELIM-OVERRIDE-FLAG
                     TO HELM-OVERRIDE-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RLIM' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-HELIM' TO ABND-PARAGRAPH
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
           DISPLAY 'HELANAL READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'HELANAL' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
