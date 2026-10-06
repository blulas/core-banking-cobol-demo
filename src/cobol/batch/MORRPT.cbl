      *================================================================*
      * PROGRAM  : MORRPT                                              *
      * TYPE     : BATCH                                               *
      * PURPOSE  : MONEY ORDERS - MANAGEMENT REPORTING. READS THE MOR  *
      *            TRANSACTIONS, UPDATES THE MOR MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. MONEY ORDER AND OFFICIAL CHECK  *
      *            ISSUANCE, PAID ITEMS AND ESCHEAT                    *
      * INPUT    : MORMST (KSDS), MORTRN, BANKCTL                      *
      * OUTPUT   : MOROUT, MORRPRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, MODTE, MOFEE, MORISK    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-08-22 LMB  ORIGINAL                                       *
      * 1994-07-26 DPS  PAID STATUS CODES ADDED PRB3407                *
      * 1995-12-07 BHS  ESCHEAT ROUNDING - HALF UP INC4849             *
      * 1997-11-22 PQR  FEE STATUS CODES ADDED INC4280                 *
      * 1998-11-08 JLM  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2001-03-17 SKN  FEE ROUNDING - HALF UP CHG0649                 *
      * 2001-01-18 JLM  FEE LIMIT REVISED PRB5028                      *
      * 2002-05-04 RJT  PERFORMANCE - ESCHEAT READ AHEAD INC4432       *
      * 2004-01-05 PQR  FACE EXCEPTIONS TO OPS QUEUE PRB5708           *
      * 2006-01-15 DPS  ESCHEAT FEE SCHEDULE UPDATE REQ3011            *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MORRPT.
       AUTHOR. D P SHAW.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "MORMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS MORM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "MORTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "MOROUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "MORRPRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY MORMST.
       FD  TRAN-FILE.
       COPY MORTRN.
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
                                           VALUE 10.000000.
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
      * WS-PAID-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PAID-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000450000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000822000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003000000000807000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003500000000304500'.
       01  WS-PAID-GRID-TBL REDEFINES WS-PAID-GRID-VALUES.
           05  WS-PAID-GRID-ROW OCCURS 4 TIMES.
               10  WS-PAID-GRID-LOW        PIC 9(09)V99.
               10  WS-PAID-GRID-RATE       PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-ESCHEAT-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-ESCHEAT-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000454000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000419500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000300000000259000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005300000000139500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005350000000297000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005400000000748000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005900000000513000'.
       01  WS-ESCHEAT-BAND-TBL REDEFINES WS-ESCHEAT-BAND-VALUES.
           05  WS-ESCHEAT-BAND-ROW OCCURS 7 TIMES.
               10  WS-ESCHEAT-BAND-LOW     PIC 9(09)V99.
               10  WS-ESCHEAT-BAND-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-OUTSTAND-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-OUTSTAND-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000596000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000676500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010100000000466000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010350000000459500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010850000000304000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011850000000355000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021850000000243500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022350000000225000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00024850000000672000'.
       01  WS-OUTSTAND-SCHED-TBL REDEFINES WS-OUTSTAND-SCHED-VALUES.
           05  WS-OUTSTAND-SCHED-ROW OCCURS 9 TIMES.
               10  WS-OUTSTAND-SCHED-LOW   PIC 9(09)V99.
               10  WS-OUTSTAND-SCHED-RATE  PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  MORRPT  MONEY ORDERS'.
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
       COPY MODTEP.
       COPY MOFEEP.
       COPY MORISKP.

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
               MOVE 'MORMST' TO ABND-FILE-NAME
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
           MOVE MORT-KEY TO MORM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-ASSESS-STOP
               PERFORM 3010-FLAG-OUTSTAND
               PERFORM 3020-ASSESS-EXEMPT
               PERFORM 3030-ACCUM-TYPE
               PERFORM 3040-ACCRUE-VOIDED
               PERFORM 3050-ACCRUE-OUTSTAND
               PERFORM 3060-ASSESS-AUTO-RENEW
               PERFORM 3070-FLAG-FACE
               PERFORM 3080-SET-TYPE
               PERFORM 3090-PRORATE-OUTSTAND
               PERFORM 3100-LIMIT-OUTSTAND
               PERFORM 3110-SET-CHANNEL
               PERFORM 3120-SPLIT-FEE
               PERFORM 3130-FLAG-ESCHEAT
               PERFORM 3140-CHECK-LAST-MAINT
               PERFORM 3150-FLAG-ITEM
               PERFORM 3160-AGE-OPEN
               PERFORM 3170-EDIT-CUST-ID
               PERFORM 3180-SELECT-FEE-PCT
               PERFORM 3190-DESCRIBE-CYCLE
               PERFORM 3200-ASSESS-EXEMPT
               PERFORM 3210-CHECK-LAST-MAINT
               PERFORM 3220-CHECK-LAST-STMT
               PERFORM 3230-ACCUM-CHANNEL
               PERFORM 3240-PRORATE-ESCHEAT
               PERFORM 3250-EDIT-REJECT
               PERFORM 3260-SPLIT-FEE
               PERFORM 3270-EDIT-REF-NO
               PERFORM 3280-ACCRUE-ESCHEAT
               PERFORM 3290-COUNT-PAYMENT
               PERFORM 3300-ACCUM-CHANNEL
               PERFORM 3310-LIMIT-VOIDED
               PERFORM 3320-AGE-CLOSE
               PERFORM 3330-CHECK-EFFECTIVE
               PERFORM 3340-PRORATE-FEE
               PERFORM 3350-CHECK-EFF
               PERFORM 3360-LIMIT-FACE
               PERFORM 3370-ASSESS-REVERSAL
               PERFORM 3380-LIMIT-FACE
               PERFORM 3390-ACCUM-ITEM-TYPE
               PERFORM 3400-SET-TYPE
               PERFORM 3410-ASSESS-OVERRIDE
               PERFORM 3420-AGE-LAST-MAINT
               PERFORM 3430-EDIT-REJECT
               PERFORM 3440-DESCRIBE-REASON
               PERFORM 3450-SET-PAYEE-TYPE
               PERFORM 3460-DESCRIBE-REASON
               PERFORM 3470-FLAG-VOIDED
               PERFORM 3480-COUNT-NOTICE
               PERFORM 3490-FLAG-ESCHEAT
               PERFORM 3500-COUNT-SEQ
               PERFORM 3510-SET-ITEM-TYPE
               PERFORM 3520-ASSESS-PREFERRED
               PERFORM 3530-PRORATE-OUTSTAND
               PERFORM 3540-FLAG-ESCHEAT
               PERFORM 3550-PRORATE-OUTSTAND
               PERFORM 3560-FLAG-FACE
               PERFORM 3570-DESCRIBE-TYPE
               PERFORM 3580-CHECK-LAST-PAY
               PERFORM 3590-DESCRIBE-CHANNEL
               PERFORM 3600-CHECK-POST
               PERFORM 3610-PRORATE-FACE
               PERFORM 3620-COUNT-RETRY
               PERFORM 3630-FLAG-ESCHEAT
               PERFORM 3640-EDIT-CUST-ID
               PERFORM 3650-CHECK-NOTICE
               PERFORM 3660-EDIT-MEMO
               PERFORM 3670-ASSESS-OVERRIDE
               PERFORM 3680-ACCUM-TRAN
               PERFORM 3690-COUNT-ITEM
               PERFORM 3700-DESCRIBE-PAYEE-TYPE
               PERFORM 3710-SET-REASON
               PERFORM 3720-LIMIT-ESCHEAT
               PERFORM 3730-CHECK-POST
               PERFORM 3740-ACCUM-CHANNEL
               PERFORM 3750-ASSESS-PREFERRED
               PERFORM 3760-LIMIT-ITEM
               PERFORM 3770-SPLIT-OUTSTAND
               PERFORM 3780-SET-PAYEE-TYPE
               PERFORM 3790-SET-CHANNEL
               PERFORM 3800-EDIT-DESC
               PERFORM 3810-AGE-NOTICE
               PERFORM 3820-DESCRIBE-CYCLE
               PERFORM 3830-SPLIT-PAID
               PERFORM 3840-AGE-NOTICE
               PERFORM 3850-SELECT-DISCOUNT
               PERFORM 3860-COUNT-ITEM
               PERFORM 3870-CHECK-LAST-PAY
               PERFORM 3880-SET-CYCLE
               PERFORM 3890-DESCRIBE-TRAN
               PERFORM 3900-FLAG-VOIDED
               PERFORM 3910-COUNT-NOTICE
               PERFORM 3920-ACCRUE-ESCHEAT
               PERFORM 3930-SPLIT-PAID
               PERFORM 3940-ASSESS-STOP
               PERFORM 3950-SELECT-FEE-PCT
               PERFORM 3960-ACCRUE-VOIDED
               PERFORM 3970-SET-ITEM-TYPE
               PERFORM 3980-CHECK-EFF
               PERFORM 3990-CHECK-EXPIRY
               PERFORM 4000-FLAG-PAID
               PERFORM 4010-FLAG-FACE
               PERFORM 4020-SET-ITEM-TYPE
               PERFORM 4030-SET-PAYEE-TYPE
               PERFORM 4040-EDIT-CONTACT
               PERFORM 4050-COUNT-NOTICE
               PERFORM 4060-SET-ITEM-TYPE
               PERFORM 4070-CHECK-EXPIRY
               PERFORM 4080-PRORATE-VOIDED
               PERFORM 4090-COUNT-RETRY
               PERFORM 4100-SPLIT-OUTSTAND
               PERFORM 4110-LIMIT-ESCHEAT
               PERFORM 7020-CALL-MODTE
               PERFORM 7000-CALL-MOFEE
               PERFORM 7010-CALL-MORISK
               REWRITE MOR-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'MORMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM MOR-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * ASSESS THE OUTSTAND YTD FEE ON THE PAID AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3000-ASSESS-STOP.
           EVALUATE TRUE
               WHEN MORM-STOP-FLAG = 'Y'
                   MOVE ZERO TO MORM-OUTSTAND-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MORT-REVERSAL-FLAG = 'Y'
                   COMPUTE MORM-OUTSTAND-YTD ROUNDED = MORM-PAID-AMT *
                       MORM-PENALTY-PCT / 200
               WHEN MORM-ITEM-TYPE-CD = 'B'
                   MOVE 3.00 TO MORM-OUTSTAND-YTD
               WHEN MORM-ITEM-TYPE-CD = 'H'
                   MOVE 20.00 TO MORM-OUTSTAND-YTD
               WHEN OTHER
                   COMPUTE MORM-OUTSTAND-YTD ROUNDED = MORM-PAID-AMT *
                       MORM-PENALTY-PCT / 100
           END-EVALUATE
           IF MORM-OUTSTAND-YTD > 0 AND MORM-OUTSTAND-YTD < 1.00
               MOVE 1.00 TO MORM-OUTSTAND-YTD
           END-IF
           IF MORM-OUTSTAND-YTD > 250.00
               MOVE 250.00 TO MORM-OUTSTAND-YTD
           END-IF
           ADD MORM-OUTSTAND-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * OUTSTAND AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3010-FLAG-OUTSTAND.
           IF MORT-OUTSTAND-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE MORT-OUTSTAND-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAID FEE ON THE FEE AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3020-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN MORM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO MORM-PAID-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MORT-REVERSAL-FLAG = 'Y'
                   COMPUTE MORM-PAID-AMT ROUNDED = MORM-FEE-AMT *
                       MORM-FEE-PCT / 200
               WHEN MORT-TRAN-CD = 'IS'
                   MOVE 15.00 TO MORM-PAID-AMT
               WHEN MORT-TRAN-CD = 'PD'
                   MOVE 5.00 TO MORM-PAID-AMT
               WHEN OTHER
                   COMPUTE MORM-PAID-AMT ROUNDED = MORM-FEE-AMT *
                       MORM-FEE-PCT / 100
           END-EVALUATE
           IF MORM-PAID-AMT > 0 AND MORM-PAID-AMT < 10.00
               MOVE 10.00 TO MORM-PAID-AMT
           END-IF
           IF MORM-PAID-AMT > 150.00
               MOVE 150.00 TO MORM-PAID-AMT
           END-IF
           ADD MORM-PAID-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE FACE AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3030-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MORM-TYPE-CD
                   ADD MORM-FACE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MORM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MORM-FACE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VOIDED YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3040-ACCRUE-VOIDED.
           IF MORM-VOIDED-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORM-VOIDED-YTD * MORM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO MORM-PAID-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON OUTSTAND AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3050-ACCRUE-OUTSTAND.
           IF MORM-OUTSTAND-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORM-OUTSTAND-AMT * MORM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO MORM-ESCHEAT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ESCHEAT FEE ON THE FEE YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3060-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN MORM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO MORT-ESCHEAT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MORM-STOP-FLAG = 'Y'
                   COMPUTE MORT-ESCHEAT-AMT ROUNDED = MORM-FEE-YTD *
                       MORM-PENALTY-PCT / 200
               WHEN MORM-TYPE-CD = 'AGE'
                   MOVE 3.00 TO MORT-ESCHEAT-AMT
               WHEN MORM-TYPE-CD = 'JDX'
                   MOVE 15.00 TO MORT-ESCHEAT-AMT
               WHEN OTHER
                   COMPUTE MORT-ESCHEAT-AMT ROUNDED = MORM-FEE-YTD *
                       MORM-PENALTY-PCT / 100
           END-EVALUATE
           IF MORT-ESCHEAT-AMT > 0 AND MORT-ESCHEAT-AMT < 2.00
               MOVE 2.00 TO MORT-ESCHEAT-AMT
           END-IF
           IF MORT-ESCHEAT-AMT > 150.00
               MOVE 150.00 TO MORT-ESCHEAT-AMT
           END-IF
           ADD MORT-ESCHEAT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FACE YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3070-FLAG-FACE.
           IF MORM-FACE-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE MORM-FACE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * AGE -> JDX WHEN LAST PAY DATE IS PAST
      * JDX -> LDX WHEN LAST PAY DATE IS PAST
      * LDX -> RHT WHEN LAST PAY DATE IS PAST
      * RHT -> AGE WHEN LAST PAY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3080-SET-TYPE.
           EVALUATE MORM-TYPE-CD
               WHEN 'AGE'
                   IF MORM-LAST-PAY-DATE > 0 AND MORM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JDX' TO MORM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JDX'
                   IF MORM-LAST-PAY-DATE > 0 AND MORM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LDX' TO MORM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LDX'
                   IF MORM-LAST-PAY-DATE > 0 AND MORM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RHT' TO MORM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RHT'
                   IF MORM-LAST-PAY-DATE > 0 AND MORM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AGE' TO MORM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO OUTSTAND YTD.
      *----------------------------------------------------------------*
       3090-PRORATE-OUTSTAND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MORM-OUTSTAND-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MORM-OUTSTAND-YTD
           END-IF
           ADD MORM-OUTSTAND-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * OUTSTAND YTD IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3100-LIMIT-OUTSTAND.
           IF MORM-OUTSTAND-YTD < 50.00
               MOVE 50.00 TO MORM-OUTSTAND-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MORM-OUTSTAND-YTD > 2500.00
               MOVE 2500.00 TO MORM-OUTSTAND-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * DR -> DX WHEN LAST PAY DATE IS PAST
      * DX -> PT WHEN LAST PAY DATE IS PAST
      * PT -> RN WHEN LAST PAY DATE IS PAST
      * RN -> YD WHEN LAST PAY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3110-SET-CHANNEL.
           EVALUATE MORM-CHANNEL-CD
               WHEN 'DR'
                   IF MORM-LAST-PAY-DATE > 0 AND MORM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DX' TO MORM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DX'
                   IF MORM-LAST-PAY-DATE > 0 AND MORM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PT' TO MORM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PT'
                   IF MORM-LAST-PAY-DATE > 0 AND MORM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RN' TO MORM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RN'
                   IF MORM-LAST-PAY-DATE > 0 AND MORM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YD' TO MORM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3120-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = MORM-FEE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = MORM-FEE-AMT - (WS-WORK-AMT * 4)
           COMPUTE MORT-OUTSTAND-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ESCHEAT YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3130-FLAG-ESCHEAT.
           IF MORM-ESCHEAT-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE MORM-ESCHEAT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 45 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3140-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF MORM-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO MORM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3150-FLAG-ITEM.
           IF WS-WORK-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3160-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF MORM-OPEN-DATE > 0 AND MORM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MORM-FACE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD MORM-FACE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD MORM-FACE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD MORM-FACE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MORM-FACE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E413 CUST ID REQUIRED
      * E623 WORK AMT NEGATIVE
      * E347 WORK AMT OVER 99999
      * E661 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3170-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MORM-CUST-ID = SPACES
                   MOVE 'E413' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E623' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 99999.00
                   MOVE 'E347' TO WS-REJECT-CODE
               WHEN MORM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E661' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ESCHEAT BAND TIER FOR THE ESCHEAT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3180-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ESCHEAT-BAND-RATE(1) TO MORM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF MORM-ESCHEAT-YTD NOT < WS-ESCHEAT-BAND-LOW(WS-TX)
                   MOVE WS-ESCHEAT-BAND-RATE(WS-TX) TO MORM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MORM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MORM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3190-DESCRIBE-CYCLE.
           EVALUATE MORM-CYCLE-CD
               WHEN 'DGU'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'GEJ'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'JBE'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE FEE FEE ON THE ESCHEAT AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3200-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN MORM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO MORM-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MORM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE MORM-FEE-AMT ROUNDED = MORM-ESCHEAT-AMT *
                       MORM-DISCOUNT / 200
               WHEN MORT-CHANNEL-CD = 'BRN'
                   MOVE 10.00 TO MORM-FEE-AMT
               WHEN MORT-CHANNEL-CD = 'BAT'
                   MOVE 3.00 TO MORM-FEE-AMT
               WHEN OTHER
                   COMPUTE MORM-FEE-AMT ROUNDED = MORM-ESCHEAT-AMT *
                       MORM-DISCOUNT / 100
           END-EVALUATE
           IF MORM-FEE-AMT > 0 AND MORM-FEE-AMT < 10.00
               MOVE 10.00 TO MORM-FEE-AMT
           END-IF
           IF MORM-FEE-AMT > 75.00
               MOVE 75.00 TO MORM-FEE-AMT
           END-IF
           ADD MORM-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 120 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3210-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF MORM-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO MORM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 365 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3220-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF MORM-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO MORM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ESCHEAT AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3230-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MORT-CHANNEL-CD
                   ADD MORM-ESCHEAT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MORT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MORM-ESCHEAT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE VOIDED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ESCHEAT YTD.
      *----------------------------------------------------------------*
       3240-PRORATE-ESCHEAT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MORM-ESCHEAT-YTD ROUNDED =
                   MORM-VOIDED-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MORM-ESCHEAT-YTD
           END-IF
           ADD MORM-ESCHEAT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E090 REJECT CD REQUIRED
      * E464 PAID YTD NEGATIVE
      * E912 PAID YTD OVER 25000
      * E912 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3250-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MORT-REJECT-CD = SPACES
                   MOVE 'E090' TO WS-REJECT-CODE
               WHEN MORM-PAID-YTD < 0
                   MOVE 'E464' TO WS-REJECT-CODE
               WHEN MORM-PAID-YTD > 25000.00
                   MOVE 'E912' TO WS-REJECT-CODE
               WHEN MORM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E912' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3260-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = MORM-FEE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = MORM-FEE-AMT - (WS-WORK-AMT * 3)
           COMPUTE MORM-ESCHEAT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E282 REF NO REQUIRED
      * E401 FACE AMT NEGATIVE
      * E958 FACE AMT OVER 250000
      * E136 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3270-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MORM-REF-NO = SPACES
                   MOVE 'E282' TO WS-REJECT-CODE
               WHEN MORM-FACE-AMT < 0
                   MOVE 'E401' TO WS-REJECT-CODE
               WHEN MORM-FACE-AMT > 250000.00
                   MOVE 'E958' TO WS-REJECT-CODE
               WHEN MORM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E136' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ESCHEAT AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3280-ACCRUE-ESCHEAT.
           IF MORM-ESCHEAT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORM-ESCHEAT-AMT * MORM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO MORT-OUTSTAND-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3290-COUNT-PAYMENT.
           IF MORM-PAYMENT-CNT < 9999999
               ADD 1 TO MORM-PAYMENT-CNT
           END-IF
           IF MORM-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO MORM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FACE AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3300-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MORT-CHANNEL-CD
                   ADD MORM-FACE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MORT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MORM-FACE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * VOIDED YTD IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3310-LIMIT-VOIDED.
           IF MORM-VOIDED-YTD < 50.00
               MOVE 50.00 TO MORM-VOIDED-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MORM-VOIDED-YTD > 5000.00
               MOVE 5000.00 TO MORM-VOIDED-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3320-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF MORM-CLOSE-DATE > 0 AND MORM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3330-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF MORM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO MORM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FEE AMT.
      *----------------------------------------------------------------*
       3340-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MORT-FEE-AMT ROUNDED =
                   MORM-FEE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MORT-FEE-AMT
           END-IF
           ADD MORT-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 60 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3350-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF MORT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO MORM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FACE YTD IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3360-LIMIT-FACE.
           IF MORM-FACE-YTD < 100.00
               MOVE 100.00 TO MORM-FACE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MORM-FACE-YTD > 10000.00
               MOVE 10000.00 TO MORM-FACE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAID FEE ON THE WORK AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3370-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN MORT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO MORM-PAID-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MORM-STOP-FLAG = 'Y'
                   COMPUTE MORM-PAID-AMT ROUNDED = WS-WORK-AMT *
                       MORM-PENALTY-PCT / 200
               WHEN MORM-PAYEE-TYPE-CD = 'CJ'
                   MOVE 7.00 TO MORM-PAID-AMT
               WHEN MORM-PAYEE-TYPE-CD = 'FY'
                   MOVE 7.00 TO MORM-PAID-AMT
               WHEN OTHER
                   COMPUTE MORM-PAID-AMT ROUNDED = WS-WORK-AMT *
                       MORM-PENALTY-PCT / 100
           END-EVALUATE
           IF MORM-PAID-AMT > 0 AND MORM-PAID-AMT < 1.00
               MOVE 1.00 TO MORM-PAID-AMT
           END-IF
           IF MORM-PAID-AMT > 50.00
               MOVE 50.00 TO MORM-PAID-AMT
           END-IF
           ADD MORM-PAID-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FACE AMT IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3380-LIMIT-FACE.
           IF MORM-FACE-AMT < 0.00
               MOVE 0.00 TO MORM-FACE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MORM-FACE-AMT > 500.00
               MOVE 500.00 TO MORM-FACE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE VOIDED YTD BY ITEM TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3390-ACCUM-ITEM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MORM-ITEM-TYPE-CD
                   ADD MORM-VOIDED-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MORM-ITEM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MORM-VOIDED-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * AGE -> JDX WHEN EXPIRY DATE IS PAST
      * JDX -> LDX WHEN EXPIRY DATE IS PAST
      * LDX -> RHT WHEN EXPIRY DATE IS PAST
      * RHT -> AGE WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3400-SET-TYPE.
           EVALUATE MORM-TYPE-CD
               WHEN 'AGE'
                   IF MORM-EXPIRY-DATE > 0 AND MORM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JDX' TO MORM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JDX'
                   IF MORM-EXPIRY-DATE > 0 AND MORM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LDX' TO MORM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LDX'
                   IF MORM-EXPIRY-DATE > 0 AND MORM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RHT' TO MORM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RHT'
                   IF MORM-EXPIRY-DATE > 0 AND MORM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AGE' TO MORM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE FEE FEE ON THE FACE YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3410-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN MORM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO MORM-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MORM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE MORM-FEE-AMT ROUNDED = MORM-FACE-YTD *
                       MORM-FEE-PCT / 200
               WHEN MORM-ITEM-TYPE-CD = 'B'
                   MOVE 15.00 TO MORM-FEE-AMT
               WHEN MORM-ITEM-TYPE-CD = 'H'
                   MOVE 7.00 TO MORM-FEE-AMT
               WHEN OTHER
                   COMPUTE MORM-FEE-AMT ROUNDED = MORM-FACE-YTD *
                       MORM-FEE-PCT / 100
           END-EVALUATE
           IF MORM-FEE-AMT > 0 AND MORM-FEE-AMT < 2.00
               MOVE 2.00 TO MORM-FEE-AMT
           END-IF
           IF MORM-FEE-AMT > 250.00
               MOVE 250.00 TO MORM-FEE-AMT
           END-IF
           ADD MORM-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3420-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF MORM-LAST-MAINT-DATE > 0 AND MORM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MORM-FACE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD MORM-FACE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD MORM-FACE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD MORM-FACE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MORM-FACE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E385 REJECT CD REQUIRED
      * E589 OUTSTAND YTD NEGATIVE
      * E625 OUTSTAND YTD OVER 250000
      * E216 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3430-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MORT-REJECT-CD = SPACES
                   MOVE 'E385' TO WS-REJECT-CODE
               WHEN MORM-OUTSTAND-YTD < 0
                   MOVE 'E589' TO WS-REJECT-CODE
               WHEN MORM-OUTSTAND-YTD > 250000.00
                   MOVE 'E625' TO WS-REJECT-CODE
               WHEN MORM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E216' TO WS-REJECT-CODE
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
       3440-DESCRIBE-REASON.
           EVALUATE MORM-REASON-CD
               WHEN 'B'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'L'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'P'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'S'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'T'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'Z'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PAYEE TYPE CD:
      * CJ -> FY WHEN NOTICE DATE IS PAST
      * FY -> HX WHEN NOTICE DATE IS PAST
      * HX -> UV WHEN NOTICE DATE IS PAST
      * UV -> VV WHEN NOTICE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3450-SET-PAYEE-TYPE.
           EVALUATE MORM-PAYEE-TYPE-CD
               WHEN 'CJ'
                   IF MORM-NOTICE-DATE > 0 AND MORM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FY' TO MORM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FY'
                   IF MORM-NOTICE-DATE > 0 AND MORM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HX' TO MORM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HX'
                   IF MORM-NOTICE-DATE > 0 AND MORM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UV' TO MORM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UV'
                   IF MORM-NOTICE-DATE > 0 AND MORM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VV' TO MORM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3460-DESCRIBE-REASON.
           EVALUATE MORM-REASON-CD
               WHEN 'B'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'L'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'P'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'S'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'T'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'Z'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * VOIDED YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3470-FLAG-VOIDED.
           IF MORM-VOIDED-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE MORM-VOIDED-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3480-COUNT-NOTICE.
           IF MORM-NOTICE-CNT < 9999999
               ADD 1 TO MORM-NOTICE-CNT
           END-IF
           IF MORM-NOTICE-CNT NOT < 5
               MOVE 'Y' TO MORM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ESCHEAT AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3490-FLAG-ESCHEAT.
           IF MORT-ESCHEAT-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE MORT-ESCHEAT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3500-COUNT-SEQ.
           IF MORT-SEQ < 9999999
               ADD 1 TO MORT-SEQ
           END-IF
           IF MORT-SEQ NOT < 10
               MOVE 'Y' TO MORT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ITEM TYPE CD:
      * B -> H WHEN EXPIRY DATE IS PAST
      * H -> S WHEN EXPIRY DATE IS PAST
      * S -> T WHEN EXPIRY DATE IS PAST
      * T -> B WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3510-SET-ITEM-TYPE.
           EVALUATE MORM-ITEM-TYPE-CD
               WHEN 'B'
                   IF MORM-EXPIRY-DATE > 0 AND MORM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO MORM-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF MORM-EXPIRY-DATE > 0 AND MORM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO MORM-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF MORM-EXPIRY-DATE > 0 AND MORM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO MORM-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF MORM-EXPIRY-DATE > 0 AND MORM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'B' TO MORM-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE PAID AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3520-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN MORM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MORM-STOP-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = MORM-PAID-AMT *
                       MORM-FEE-PCT / 200
               WHEN MORM-CYCLE-CD = 'DGU'
                   MOVE 20.00 TO WS-WORK-AMT
               WHEN MORM-CYCLE-CD = 'GEJ'
                   MOVE 12.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = MORM-PAID-AMT *
                       MORM-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 5.00
               MOVE 5.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 25.00
               MOVE 25.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE FACE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO OUTSTAND AMT.
      *----------------------------------------------------------------*
       3530-PRORATE-OUTSTAND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MORT-OUTSTAND-AMT ROUNDED =
                   MORM-FACE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MORT-OUTSTAND-AMT
           END-IF
           ADD MORT-OUTSTAND-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ESCHEAT AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3540-FLAG-ESCHEAT.
           IF MORT-ESCHEAT-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE MORT-ESCHEAT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO OUTSTAND AMT.
      *----------------------------------------------------------------*
       3550-PRORATE-OUTSTAND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MORT-OUTSTAND-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MORT-OUTSTAND-AMT
           END-IF
           ADD MORT-OUTSTAND-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FACE AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3560-FLAG-FACE.
           IF MORM-FACE-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE MORM-FACE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3570-DESCRIBE-TYPE.
           EVALUATE MORM-TYPE-CD
               WHEN 'AGE'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'JDX'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'LDX'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'RHT'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 15 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3580-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF MORM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO MORM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3590-DESCRIBE-CHANNEL.
           EVALUATE MORM-CHANNEL-CD
               WHEN 'DR'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'DX'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'PT'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'RN'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'YD'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'YU'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 60 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3600-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF MORT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO MORM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE OUTSTAND AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FACE AMT.
      *----------------------------------------------------------------*
       3610-PRORATE-FACE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MORT-FACE-AMT ROUNDED =
                   MORM-OUTSTAND-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MORT-FACE-AMT
           END-IF
           ADD MORT-FACE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3620-COUNT-RETRY.
           IF MORM-RETRY-CNT < 9999999
               ADD 1 TO MORM-RETRY-CNT
           END-IF
           IF MORM-RETRY-CNT NOT < 12
               MOVE 'Y' TO MORT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ESCHEAT YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3630-FLAG-ESCHEAT.
           IF MORM-ESCHEAT-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE MORM-ESCHEAT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E716 CUST ID REQUIRED
      * E258 WORK AMT NEGATIVE
      * E561 WORK AMT OVER 1000000
      * E433 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3640-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MORM-CUST-ID = SPACES
                   MOVE 'E716' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E258' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 1000000.00
                   MOVE 'E561' TO WS-REJECT-CODE
               WHEN MORM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E433' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3650-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF MORM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO MORM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E962 MEMO REQUIRED
      * E452 FEE AMT NEGATIVE
      * E907 FEE AMT OVER 10000
      * E298 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3660-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MORT-MEMO = SPACES
                   MOVE 'E962' TO WS-REJECT-CODE
               WHEN MORM-FEE-AMT < 0
                   MOVE 'E452' TO WS-REJECT-CODE
               WHEN MORM-FEE-AMT > 10000.00
                   MOVE 'E907' TO WS-REJECT-CODE
               WHEN MORM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E298' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAID FEE ON THE FEE AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3670-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN MORM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO MORM-PAID-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MORM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE MORM-PAID-AMT ROUNDED = MORT-FEE-AMT *
                       MORM-FEE-PCT / 200
               WHEN MORM-REASON-CD = 'B'
                   MOVE 5.00 TO MORM-PAID-AMT
               WHEN MORM-REASON-CD = 'L'
                   MOVE 7.00 TO MORM-PAID-AMT
               WHEN OTHER
                   COMPUTE MORM-PAID-AMT ROUNDED = MORT-FEE-AMT *
                       MORM-FEE-PCT / 100
           END-EVALUATE
           IF MORM-PAID-AMT > 0 AND MORM-PAID-AMT < 5.00
               MOVE 5.00 TO MORM-PAID-AMT
           END-IF
           IF MORM-PAID-AMT > 250.00
               MOVE 250.00 TO MORM-PAID-AMT
           END-IF
           ADD MORM-PAID-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE YTD BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3680-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MORT-TRAN-CD
                   ADD MORM-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MORT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MORM-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3690-COUNT-ITEM.
           IF MORM-ITEM-CNT < 9999999
               ADD 1 TO MORM-ITEM-CNT
           END-IF
           IF MORM-ITEM-CNT NOT < 3
               MOVE 'Y' TO MORM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PAYEE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3700-DESCRIBE-PAYEE-TYPE.
           EVALUATE MORM-PAYEE-TYPE-CD
               WHEN 'CJ'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'FY'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'HX'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'UV'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'VV'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * B -> L WHEN CLOSE DATE IS PAST
      * L -> P WHEN CLOSE DATE IS PAST
      * P -> S WHEN CLOSE DATE IS PAST
      * S -> T WHEN CLOSE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3710-SET-REASON.
           EVALUATE MORM-REASON-CD
               WHEN 'B'
                   IF MORM-CLOSE-DATE > 0 AND MORM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO MORM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF MORM-CLOSE-DATE > 0 AND MORM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'P' TO MORM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'P'
                   IF MORM-CLOSE-DATE > 0 AND MORM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO MORM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF MORM-CLOSE-DATE > 0 AND MORM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO MORM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ESCHEAT AMT IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3720-LIMIT-ESCHEAT.
           IF MORM-ESCHEAT-AMT < 10.00
               MOVE 10.00 TO MORM-ESCHEAT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MORM-ESCHEAT-AMT > 5000.00
               MOVE 5000.00 TO MORM-ESCHEAT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 30 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3730-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF MORT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO MORT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE VOIDED YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3740-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MORM-CHANNEL-CD
                   ADD MORM-VOIDED-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MORM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MORM-VOIDED-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FEE YTD FEE ON THE VOIDED AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3750-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN MORM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO MORM-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MORM-REVIEW-FLAG = 'Y'
                   COMPUTE MORM-FEE-YTD ROUNDED = MORM-VOIDED-AMT *
                       MORM-PENALTY-PCT / 200
               WHEN MORT-TRAN-CD = 'IS'
                   MOVE 5.00 TO MORM-FEE-YTD
               WHEN MORT-TRAN-CD = 'PD'
                   MOVE 15.00 TO MORM-FEE-YTD
               WHEN OTHER
                   COMPUTE MORM-FEE-YTD ROUNDED = MORM-VOIDED-AMT *
                       MORM-PENALTY-PCT / 100
           END-EVALUATE
           IF MORM-FEE-YTD > 0 AND MORM-FEE-YTD < 2.00
               MOVE 2.00 TO MORM-FEE-YTD
           END-IF
           IF MORM-FEE-YTD > 150.00
               MOVE 150.00 TO MORM-FEE-YTD
           END-IF
           ADD MORM-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3760-LIMIT-ITEM.
           IF WS-WORK-AMT < 25.00
               MOVE 25.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 1000.00
               MOVE 1000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT OUTSTAND AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3770-SPLIT-OUTSTAND.
           COMPUTE WS-WORK-AMT = MORM-OUTSTAND-AMT / 4
           COMPUTE WS-WORK-AMT-2 = MORM-OUTSTAND-AMT - (WS-WORK-AMT * 4)
           COMPUTE MORM-VOIDED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PAYEE TYPE CD:
      * CJ -> FY WHEN LAST STMT DATE IS PAST
      * FY -> HX WHEN LAST STMT DATE IS PAST
      * HX -> UV WHEN LAST STMT DATE IS PAST
      * UV -> VV WHEN LAST STMT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3780-SET-PAYEE-TYPE.
           EVALUATE MORM-PAYEE-TYPE-CD
               WHEN 'CJ'
                   IF MORM-LAST-STMT-DATE > 0 AND MORM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FY' TO MORM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FY'
                   IF MORM-LAST-STMT-DATE > 0 AND MORM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HX' TO MORM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HX'
                   IF MORM-LAST-STMT-DATE > 0 AND MORM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UV' TO MORM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UV'
                   IF MORM-LAST-STMT-DATE > 0 AND MORM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VV' TO MORM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * DR -> DX WHEN LAST ACT DATE IS PAST
      * DX -> PT WHEN LAST ACT DATE IS PAST
      * PT -> RN WHEN LAST ACT DATE IS PAST
      * RN -> YD WHEN LAST ACT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3790-SET-CHANNEL.
           EVALUATE MORM-CHANNEL-CD
               WHEN 'DR'
                   IF MORM-LAST-ACT-DATE > 0 AND MORM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DX' TO MORM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DX'
                   IF MORM-LAST-ACT-DATE > 0 AND MORM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PT' TO MORM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PT'
                   IF MORM-LAST-ACT-DATE > 0 AND MORM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RN' TO MORM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RN'
                   IF MORM-LAST-ACT-DATE > 0 AND MORM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YD' TO MORM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E240 DESC REQUIRED
      * E761 OUTSTAND AMT NEGATIVE
      * E356 OUTSTAND AMT OVER 250000
      * E082 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3800-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MORM-DESC = SPACES
                   MOVE 'E240' TO WS-REJECT-CODE
               WHEN MORM-OUTSTAND-AMT < 0
                   MOVE 'E761' TO WS-REJECT-CODE
               WHEN MORM-OUTSTAND-AMT > 250000.00
                   MOVE 'E356' TO WS-REJECT-CODE
               WHEN MORM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E082' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3810-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF MORM-NOTICE-DATE > 0 AND MORM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MORM-VOIDED-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD MORM-VOIDED-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD MORM-VOIDED-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD MORM-VOIDED-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MORM-VOIDED-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3820-DESCRIBE-CYCLE.
           EVALUATE MORM-CYCLE-CD
               WHEN 'DGU'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'GEJ'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'JBE'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT PAID YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3830-SPLIT-PAID.
           COMPUTE WS-WORK-AMT = MORM-PAID-YTD / 3
           COMPUTE WS-WORK-AMT-2 = MORM-PAID-YTD - (WS-WORK-AMT * 3)
           COMPUTE MORM-OUTSTAND-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3840-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF MORM-NOTICE-DATE > 0 AND MORM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MORT-OUTSTAND-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD MORT-OUTSTAND-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD MORT-OUTSTAND-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD MORT-OUTSTAND-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MORT-OUTSTAND-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE OUTSTAND SCHED TIER FOR THE OUTSTAND AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3850-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-OUTSTAND-SCHED-RATE(1) TO MORM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF MORT-OUTSTAND-AMT NOT < WS-OUTSTAND-SCHED-LOW(WS-TX)
                   MOVE WS-OUTSTAND-SCHED-RATE(WS-TX) TO MORM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MORM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MORM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3860-COUNT-ITEM.
           IF MORM-ITEM-CNT < 9999999
               ADD 1 TO MORM-ITEM-CNT
           END-IF
           IF MORM-ITEM-CNT NOT < 12
               MOVE 'Y' TO MORM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 90 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3870-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF MORM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO MORT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * DGU -> GEJ WHEN EXPIRY DATE IS PAST
      * GEJ -> JBE WHEN EXPIRY DATE IS PAST
      * JBE -> DGU WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3880-SET-CYCLE.
           EVALUATE MORM-CYCLE-CD
               WHEN 'DGU'
                   IF MORM-EXPIRY-DATE > 0 AND MORM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GEJ' TO MORM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GEJ'
                   IF MORM-EXPIRY-DATE > 0 AND MORM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JBE' TO MORM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JBE'
                   IF MORM-EXPIRY-DATE > 0 AND MORM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DGU' TO MORM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3890-DESCRIBE-TRAN.
           EVALUATE MORT-TRAN-CD
               WHEN 'IS'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'PD'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'VD'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'SP'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'ES'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * VOIDED AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3900-FLAG-VOIDED.
           IF MORM-VOIDED-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE MORM-VOIDED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3910-COUNT-NOTICE.
           IF MORM-NOTICE-CNT < 9999999
               ADD 1 TO MORM-NOTICE-CNT
           END-IF
           IF MORM-NOTICE-CNT NOT < 12
               MOVE 'Y' TO MORM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ESCHEAT AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3920-ACCRUE-ESCHEAT.
           IF MORT-ESCHEAT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORT-ESCHEAT-AMT * MORM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO MORM-FACE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PAID AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3930-SPLIT-PAID.
           COMPUTE WS-WORK-AMT = MORM-PAID-AMT / 12
           COMPUTE WS-WORK-AMT-2 = MORM-PAID-AMT - (WS-WORK-AMT * 12)
           COMPUTE MORM-PAID-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE VOIDED FEE ON THE FEE AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3940-ASSESS-STOP.
           EVALUATE TRUE
               WHEN MORM-STOP-FLAG = 'Y'
                   MOVE ZERO TO MORM-VOIDED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MORM-REVIEW-FLAG = 'Y'
                   COMPUTE MORM-VOIDED-AMT ROUNDED = MORM-FEE-AMT *
                       MORM-PENALTY-PCT / 200
               WHEN MORM-REASON-CD = 'B'
                   MOVE 12.00 TO MORM-VOIDED-AMT
               WHEN MORM-REASON-CD = 'L'
                   MOVE 3.00 TO MORM-VOIDED-AMT
               WHEN OTHER
                   COMPUTE MORM-VOIDED-AMT ROUNDED = MORM-FEE-AMT *
                       MORM-PENALTY-PCT / 100
           END-EVALUATE
           IF MORM-VOIDED-AMT > 0 AND MORM-VOIDED-AMT < 1.00
               MOVE 1.00 TO MORM-VOIDED-AMT
           END-IF
           IF MORM-VOIDED-AMT > 100.00
               MOVE 100.00 TO MORM-VOIDED-AMT
           END-IF
           ADD MORM-VOIDED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE PAID GRID TIER FOR THE OUTSTAND AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3950-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAID-GRID-RATE(1) TO MORM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF MORT-OUTSTAND-AMT NOT < WS-PAID-GRID-LOW(WS-TX)
                   MOVE WS-PAID-GRID-RATE(WS-TX) TO MORM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MORM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MORM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VOIDED AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3960-ACCRUE-VOIDED.
           IF MORM-VOIDED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORM-VOIDED-AMT * MORM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO MORM-ESCHEAT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ITEM TYPE CD:
      * B -> H WHEN EFFECTIVE DATE IS PAST
      * H -> S WHEN EFFECTIVE DATE IS PAST
      * S -> T WHEN EFFECTIVE DATE IS PAST
      * T -> B WHEN EFFECTIVE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3970-SET-ITEM-TYPE.
           EVALUATE MORM-ITEM-TYPE-CD
               WHEN 'B'
                   IF MORM-EFFECTIVE-DATE > 0 AND MORM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO MORM-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF MORM-EFFECTIVE-DATE > 0 AND MORM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO MORM-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF MORM-EFFECTIVE-DATE > 0 AND MORM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO MORM-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF MORM-EFFECTIVE-DATE > 0 AND MORM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'B' TO MORM-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 30 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3980-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF MORT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO MORM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3990-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF MORM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO MORM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAID AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4000-FLAG-PAID.
           IF MORM-PAID-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE MORM-PAID-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FACE AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4010-FLAG-FACE.
           IF MORT-FACE-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE MORT-FACE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ITEM TYPE CD:
      * B -> H WHEN CLOSE DATE IS PAST
      * H -> S WHEN CLOSE DATE IS PAST
      * S -> T WHEN CLOSE DATE IS PAST
      * T -> B WHEN CLOSE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4020-SET-ITEM-TYPE.
           EVALUATE MORM-ITEM-TYPE-CD
               WHEN 'B'
                   IF MORM-CLOSE-DATE > 0 AND MORM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO MORM-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF MORM-CLOSE-DATE > 0 AND MORM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO MORM-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF MORM-CLOSE-DATE > 0 AND MORM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO MORM-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF MORM-CLOSE-DATE > 0 AND MORM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'B' TO MORM-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PAYEE TYPE CD:
      * CJ -> FY WHEN LAST ACT DATE IS PAST
      * FY -> HX WHEN LAST ACT DATE IS PAST
      * HX -> UV WHEN LAST ACT DATE IS PAST
      * UV -> VV WHEN LAST ACT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4030-SET-PAYEE-TYPE.
           EVALUATE MORM-PAYEE-TYPE-CD
               WHEN 'CJ'
                   IF MORM-LAST-ACT-DATE > 0 AND MORM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FY' TO MORM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FY'
                   IF MORM-LAST-ACT-DATE > 0 AND MORM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HX' TO MORM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HX'
                   IF MORM-LAST-ACT-DATE > 0 AND MORM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UV' TO MORM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UV'
                   IF MORM-LAST-ACT-DATE > 0 AND MORM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VV' TO MORM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E079 CONTACT REQUIRED
      * E130 WORK AMT 2 NEGATIVE
      * E290 WORK AMT 2 OVER 5000
      * E336 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       4040-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MORM-CONTACT = SPACES
                   MOVE 'E079' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E130' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 5000.00
                   MOVE 'E290' TO WS-REJECT-CODE
               WHEN MORM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E336' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       4050-COUNT-NOTICE.
           IF MORM-NOTICE-CNT < 9999999
               ADD 1 TO MORM-NOTICE-CNT
           END-IF
           IF MORM-NOTICE-CNT NOT < 12
               MOVE 'Y' TO MORM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ITEM TYPE CD:
      * B -> H WHEN EFF DATE IS PAST
      * H -> S WHEN EFF DATE IS PAST
      * S -> T WHEN EFF DATE IS PAST
      * T -> B WHEN EFF DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4060-SET-ITEM-TYPE.
           EVALUATE MORM-ITEM-TYPE-CD
               WHEN 'B'
                   IF MORT-EFF-DATE > 0 AND MORT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO MORM-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF MORT-EFF-DATE > 0 AND MORT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO MORM-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF MORT-EFF-DATE > 0 AND MORT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO MORM-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF MORT-EFF-DATE > 0 AND MORT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'B' TO MORM-ITEM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 10 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4070-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF MORM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO MORM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE OUTSTAND AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO VOIDED YTD.
      *----------------------------------------------------------------*
       4080-PRORATE-VOIDED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MORM-VOIDED-YTD ROUNDED =
                   MORM-OUTSTAND-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MORM-VOIDED-YTD
           END-IF
           ADD MORM-VOIDED-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       4090-COUNT-RETRY.
           IF MORM-RETRY-CNT < 9999999
               ADD 1 TO MORM-RETRY-CNT
           END-IF
           IF MORM-RETRY-CNT NOT < 12
               MOVE 'Y' TO MORM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT OUTSTAND YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4100-SPLIT-OUTSTAND.
           COMPUTE WS-WORK-AMT = MORM-OUTSTAND-YTD / 3
           COMPUTE WS-WORK-AMT-2 = MORM-OUTSTAND-YTD - (WS-WORK-AMT * 3)
           COMPUTE MORM-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ESCHEAT YTD IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4110-LIMIT-ESCHEAT.
           IF MORM-ESCHEAT-YTD < 10.00
               MOVE 10.00 TO MORM-ESCHEAT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MORM-ESCHEAT-YTD > 1000.00
               MOVE 1000.00 TO MORM-ESCHEAT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE MOR FIELDS TO MODTE AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7020-CALL-MODTE.
           INITIALIZE MODTE-PARMS
           MOVE MORM-FEE-YTD
             TO MODTE-FEE-YTD
           MOVE MORM-FEE-AMT
             TO MODTE-FEE-AMT
           MOVE MORM-PAID-AMT
             TO MODTE-PAID-AMT
           MOVE MORM-VOIDED-AMT
             TO MODTE-VOIDED-AMT
           MOVE MORM-ESCHEAT-AMT
             TO MODTE-ESCHEAT-AMT
           MOVE MORM-FACE-AMT
             TO MODTE-FACE-AMT
           MOVE MORM-LAST-MAINT-DATE
             TO MODTE-LAST-MAINT-DATE
           MOVE MORM-EXPIRY-DATE
             TO MODTE-EXPIRY-DATE
           MOVE MORM-CLOSE-DATE
             TO MODTE-CLOSE-DATE
           MOVE MORM-OPEN-DATE
             TO MODTE-OPEN-DATE
           MOVE MORM-ITEM-TYPE-CD
             TO MODTE-ITEM-TYPE-CD
           MOVE MORM-REASON-CD
             TO MODTE-REASON-CD
           MOVE MORM-TYPE-CD
             TO MODTE-TYPE-CD
           MOVE MORM-CYCLE-CD
             TO MODTE-CYCLE-CD
           MOVE MORM-STOP-FLAG
             TO MODTE-STOP-FLAG
           MOVE MORM-AUTO-RENEW-FLAG
             TO MODTE-AUTO-RENEW-FLAG
           MOVE MORM-EXEMPT-FLAG
             TO MODTE-EXEMPT-FLAG
           MOVE MORM-PAYMENT-CNT
             TO MODTE-PAYMENT-CNT
           MOVE MORM-ITEM-CNT
             TO MODTE-ITEM-CNT
           MOVE MORM-FEE-PCT
             TO MODTE-FEE-PCT
           MOVE MORM-DISCOUNT
             TO MODTE-DISCOUNT
           MOVE MORM-NAME
             TO MODTE-NAME
           MOVE MORM-REF-NO
             TO MODTE-REF-NO
           CALL 'MODTE' USING MODTE-PARMS
           EVALUATE MODTE-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE MODTE-FEE-AMT
                     TO MORM-FEE-AMT
                   MOVE MODTE-VOIDED-AMT
                     TO MORM-VOIDED-AMT
                   MOVE MODTE-ESCHEAT-AMT
                     TO MORM-ESCHEAT-AMT
                   MOVE MODTE-STOP-FLAG
                     TO MORM-STOP-FLAG
                   MOVE MODTE-EXEMPT-FLAG
                     TO MORM-EXEMPT-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RDTE' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-MODTE' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE MOR FIELDS TO MOFEE AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7000-CALL-MOFEE.
           INITIALIZE MOFEE-PARMS
           MOVE MORM-OUTSTAND-AMT
             TO MOFEE-OUTSTAND-AMT
           MOVE MORM-ESCHEAT-AMT
             TO MOFEE-ESCHEAT-AMT
           MOVE MORM-PAID-YTD
             TO MOFEE-PAID-YTD
           MOVE MORM-PAID-AMT
             TO MOFEE-PAID-AMT
           MOVE MORM-FACE-AMT
             TO MOFEE-FACE-AMT
           MOVE MORM-FEE-AMT
             TO MOFEE-FEE-AMT
           MOVE MORM-LAST-STMT-DATE
             TO MOFEE-LAST-STMT-DATE
           MOVE MORM-LAST-PAY-DATE
             TO MOFEE-LAST-PAY-DATE
           MOVE MORM-LAST-ACT-DATE
             TO MOFEE-LAST-ACT-DATE
           MOVE MORM-EFFECTIVE-DATE
             TO MOFEE-EFFECTIVE-DATE
           MOVE MORM-PAYEE-TYPE-CD
             TO MOFEE-PAYEE-TYPE-CD
           MOVE MORM-CHANNEL-CD
             TO MOFEE-CHANNEL-CD
           MOVE MORM-REASON-CD
             TO MOFEE-REASON-CD
           MOVE MORM-ITEM-TYPE-CD
             TO MOFEE-ITEM-TYPE-CD
           MOVE MORM-OVERRIDE-FLAG
             TO MOFEE-OVERRIDE-FLAG
           MOVE MORM-STOP-FLAG
             TO MOFEE-STOP-FLAG
           MOVE MORM-PREFERRED-FLAG
             TO MOFEE-PREFERRED-FLAG
           MOVE MORM-ITEM-CNT
             TO MOFEE-ITEM-CNT
           MOVE MORM-RETRY-CNT
             TO MOFEE-RETRY-CNT
           MOVE MORM-DISCOUNT
             TO MOFEE-DISCOUNT
           MOVE MORM-FEE-PCT
             TO MOFEE-FEE-PCT
           MOVE MORM-REF-NO
             TO MOFEE-REF-NO
           MOVE MORM-NAME
             TO MOFEE-NAME
           CALL 'MOFEE' USING MOFEE-PARMS
           EVALUATE MOFEE-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE MOFEE-PAID-YTD
                     TO MORM-PAID-YTD
                   MOVE MOFEE-PAID-AMT
                     TO MORM-PAID-AMT
                   MOVE MOFEE-FACE-AMT
                     TO MORM-FACE-AMT
                   MOVE MOFEE-OVERRIDE-FLAG
                     TO MORM-OVERRIDE-FLAG
                   MOVE MOFEE-STOP-FLAG
                     TO MORM-STOP-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RFEE' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-MOFEE' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE MOR FIELDS TO MORISK AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7010-CALL-MORISK.
           INITIALIZE MORISK-PARMS
           MOVE MORM-ESCHEAT-YTD
             TO MORISK-ESCHEAT-YTD
           MOVE MORM-OUTSTAND-YTD
             TO MORISK-OUTSTAND-YTD
           MOVE MORM-PAID-AMT
             TO MORISK-PAID-AMT
           MOVE MORM-FEE-AMT
             TO MORISK-FEE-AMT
           MOVE MORM-FEE-YTD
             TO MORISK-FEE-YTD
           MOVE MORM-OUTSTAND-AMT
             TO MORISK-OUTSTAND-AMT
           MOVE MORM-NOTICE-DATE
             TO MORISK-NOTICE-DATE
           MOVE MORM-OPEN-DATE
             TO MORISK-OPEN-DATE
           MOVE MORM-LAST-ACT-DATE
             TO MORISK-LAST-ACT-DATE
           MOVE MORM-EXPIRY-DATE
             TO MORISK-EXPIRY-DATE
           MOVE MORM-REASON-CD
             TO MORISK-REASON-CD
           MOVE MORM-CYCLE-CD
             TO MORISK-CYCLE-CD
           MOVE MORM-PAYEE-TYPE-CD
             TO MORISK-PAYEE-TYPE-CD
           MOVE MORM-CHANNEL-CD
             TO MORISK-CHANNEL-CD
           MOVE MORM-PREFERRED-FLAG
             TO MORISK-PREFERRED-FLAG
           MOVE MORM-STOP-FLAG
             TO MORISK-STOP-FLAG
           MOVE MORM-AUTO-RENEW-FLAG
             TO MORISK-AUTO-RENEW-FLAG
           MOVE MORM-ITEM-CNT
             TO MORISK-ITEM-CNT
           MOVE MORM-RETRY-CNT
             TO MORISK-RETRY-CNT
           MOVE MORM-PENALTY-PCT
             TO MORISK-PENALTY-PCT
           MOVE MORM-FEE-PCT
             TO MORISK-FEE-PCT
           MOVE MORM-ADDR
             TO MORISK-ADDR
           MOVE MORM-DESC
             TO MORISK-DESC
           CALL 'MORISK' USING MORISK-PARMS
           EVALUATE MORISK-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE MORISK-ESCHEAT-YTD
                     TO MORM-ESCHEAT-YTD
                   MOVE MORISK-OUTSTAND-YTD
                     TO MORM-OUTSTAND-YTD
                   MOVE MORISK-FEE-AMT
                     TO MORM-FEE-AMT
                   MOVE MORISK-OUTSTAND-AMT
                     TO MORM-OUTSTAND-AMT
                   MOVE MORISK-PREFERRED-FLAG
                     TO MORM-PREFERRED-FLAG
                   MOVE MORISK-AUTO-RENEW-FLAG
                     TO MORM-AUTO-RENEW-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RISK' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-MORISK' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE MORT-KEY TO RD-KEY
           MOVE MORT-TRAN-CD TO RD-CODE
           MOVE MORT-FACE-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE MORT-KEY TO RD-KEY
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
           DISPLAY 'MORRPT READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'MORRPT' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
