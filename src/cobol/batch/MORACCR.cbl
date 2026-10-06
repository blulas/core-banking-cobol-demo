      *================================================================*
      * PROGRAM  : MORACCR                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : MONEY ORDERS - DAILY ACCRUAL. READS THE MOR         *
      *            TRANSACTIONS, UPDATES THE MOR MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. MONEY ORDER AND OFFICIAL CHECK  *
      *            ISSUANCE, PAID ITEMS AND ESCHEAT                    *
      * INPUT    : MORMST (KSDS), MORTRN, BANKCTL                      *
      * OUTPUT   : MOROUT, MORACRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, MODTE                   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-12-13 EWS  ORIGINAL                                       *
      * 1992-05-09 MKL  VOIDED RERUN PROTECTION PRB1926                *
      * 1994-04-06 EWS  PERFORMANCE - PAID READ AHEAD CHG7551          *
      * 1998-07-21 EWS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-05-09 BHS  AUDIT FINDING - FACE CONTROL TOTALS PRB3866    *
      * 2000-07-18 SKN  AUDIT FINDING - VOIDED CONTROL TOTALS PRB4723  *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MORACCR.
       AUTHOR. M K LOWE.

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
           SELECT REPORT-FILE ASSIGN TO "MORACRPT"
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
                                           VALUE 18.000000.
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
      * WS-FEE-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FEE-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000598500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000648000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000150000000388500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001150000000114000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001250000000801000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001750000000638000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001800000000769500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006800000000051000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007050000000159500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007150000000665000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012150000000842500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022150000000419500'.
       01  WS-FEE-TIER-TBL REDEFINES WS-FEE-TIER-VALUES.
           05  WS-FEE-TIER-ROW OCCURS 12 TIMES.
               10  WS-FEE-TIER-LOW         PIC 9(09)V99.
               10  WS-FEE-TIER-RATE        PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-VOIDED-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-VOIDED-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000871000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000061000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001050000000712500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002050000000569000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002550000000666000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002650000000473500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002700000000700000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005200000000687000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010200000000638500'.
       01  WS-VOIDED-SCHED-TBL REDEFINES WS-VOIDED-SCHED-VALUES.
           05  WS-VOIDED-SCHED-ROW OCCURS 9 TIMES.
               10  WS-VOIDED-SCHED-LOW     PIC 9(09)V99.
               10  WS-VOIDED-SCHED-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-FEE-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FEE-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000449000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000776500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000184000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010050000000693000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010550000000485500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010600000000105000'.
       01  WS-FEE-GRID-TBL REDEFINES WS-FEE-GRID-VALUES.
           05  WS-FEE-GRID-ROW OCCURS 6 TIMES.
               10  WS-FEE-GRID-LOW         PIC 9(09)V99.
               10  WS-FEE-GRID-RATE        PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  MORACCR  MONEY ORDERS'.
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
               PERFORM 3000-CHECK-NOTICE
               PERFORM 3010-SET-TRAN
               PERFORM 3020-SPLIT-PAID
               PERFORM 3030-DESCRIBE-ITEM-TYPE
               PERFORM 3040-ACCRUE-PAID
               PERFORM 3050-SELECT-DISCOUNT
               PERFORM 3060-AGE-LAST-STMT
               PERFORM 3070-COUNT-RETRY
               PERFORM 3080-DESCRIBE-TRAN
               PERFORM 3090-SELECT-PENALTY-PCT
               PERFORM 3100-ACCUM-PAYEE-TYPE
               PERFORM 3110-EDIT-MEMO
               PERFORM 3120-ASSESS-STOP
               PERFORM 3130-PRORATE-PAID
               PERFORM 3140-ASSESS-REVERSAL
               PERFORM 3150-ACCRUE-ESCHEAT
               PERFORM 3160-AGE-NOTICE
               PERFORM 3170-PRORATE-FEE
               PERFORM 3180-AGE-EXPIRY
               PERFORM 3190-ACCRUE-FACE
               PERFORM 3200-SPLIT-FACE
               PERFORM 3210-DESCRIBE-ITEM-TYPE
               PERFORM 3220-CHECK-EFF
               PERFORM 3230-CHECK-NOTICE
               PERFORM 3240-ASSESS-OVERRIDE
               PERFORM 3250-DESCRIBE-PAYEE-TYPE
               PERFORM 3260-SELECT-PENALTY-PCT
               PERFORM 3270-ASSESS-EXEMPT
               PERFORM 3280-ACCRUE-PAID
               PERFORM 3290-ACCRUE-FACE
               PERFORM 3300-SELECT-FEE-PCT
               PERFORM 3310-SELECT-PENALTY-PCT
               PERFORM 3320-ACCUM-TYPE
               PERFORM 3330-ASSESS-STOP
               PERFORM 3340-COUNT-NOTICE
               PERFORM 3350-FLAG-ITEM
               PERFORM 3360-EDIT-DESC
               PERFORM 3370-COUNT-RETRY
               PERFORM 3380-LIMIT-ESCHEAT
               PERFORM 3390-SET-PAYEE-TYPE
               PERFORM 3400-DESCRIBE-PAYEE-TYPE
               PERFORM 3410-SET-TRAN
               PERFORM 3420-CHECK-LAST-PAY
               PERFORM 3430-PRORATE-ESCHEAT
               PERFORM 3440-FLAG-FACE
               PERFORM 3450-LIMIT-FEE
               PERFORM 3460-LIMIT-OUTSTAND
               PERFORM 3470-COUNT-PAYMENT
               PERFORM 3480-ASSESS-REVIEW
               PERFORM 3490-SELECT-DISCOUNT
               PERFORM 3500-DESCRIBE-CHANNEL
               PERFORM 3510-SELECT-PENALTY-PCT
               PERFORM 3520-ASSESS-AUTO-RENEW
               PERFORM 3530-SPLIT-VOIDED
               PERFORM 3540-COUNT-PAYMENT
               PERFORM 3550-ACCRUE-OUTSTAND
               PERFORM 3560-SET-TYPE
               PERFORM 3570-ACCUM-TYPE
               PERFORM 3580-DESCRIBE-ITEM-TYPE
               PERFORM 3590-AGE-LAST-PAY
               PERFORM 3600-SET-TRAN
               PERFORM 3610-ASSESS-EXEMPT
               PERFORM 3620-SELECT-DISCOUNT
               PERFORM 3630-DESCRIBE-CYCLE
               PERFORM 3640-EDIT-NAME
               PERFORM 3650-PRORATE-VOIDED
               PERFORM 3660-ASSESS-OVERRIDE
               PERFORM 3670-ACCRUE-FACE
               PERFORM 3680-SET-CYCLE
               PERFORM 3690-SET-TYPE
               PERFORM 3700-SET-CYCLE
               PERFORM 3710-SELECT-PENALTY-PCT
               PERFORM 3720-AGE-LAST-ACT
               PERFORM 3730-ACCRUE-OUTSTAND
               PERFORM 3740-EDIT-DESC
               PERFORM 3750-PRORATE-ESCHEAT
               PERFORM 3760-SPLIT-OUTSTAND
               PERFORM 3770-ASSESS-OVERRIDE
               PERFORM 3780-CHECK-LAST-STMT
               PERFORM 3790-ACCUM-REASON
               PERFORM 3800-CHECK-EFFECTIVE
               PERFORM 3810-ACCUM-TYPE
               PERFORM 3820-EDIT-CUST-ID
               PERFORM 3830-CHECK-LAST-PAY
               PERFORM 3840-ACCRUE-FEE
               PERFORM 3850-CHECK-LAST-STMT
               PERFORM 3860-LIMIT-FACE
               PERFORM 3870-AGE-EFF
               PERFORM 3880-ACCRUE-FACE
               PERFORM 3890-DESCRIBE-PAYEE-TYPE
               PERFORM 3900-ASSESS-PREFERRED
               PERFORM 3910-SET-REASON
               PERFORM 3920-CHECK-EFFECTIVE
               PERFORM 3930-ASSESS-STOP
               PERFORM 3940-LIMIT-FEE
               PERFORM 3950-ASSESS-EXEMPT
               PERFORM 3960-SPLIT-ESCHEAT
               PERFORM 3970-ASSESS-STOP
               PERFORM 3980-COUNT-RETRY
               PERFORM 3990-DESCRIBE-ITEM-TYPE
               PERFORM 4000-SET-TYPE
               PERFORM 4010-FLAG-FEE
               PERFORM 4020-SELECT-PENALTY-PCT
               PERFORM 4030-DESCRIBE-CYCLE
               PERFORM 4040-AGE-EFFECTIVE
               PERFORM 4050-SET-TYPE
               PERFORM 4060-CHECK-LAST-MAINT
               PERFORM 4070-ACCUM-CHANNEL
               PERFORM 4080-ACCUM-REASON
               PERFORM 4090-COUNT-NOTICE
               PERFORM 4100-ASSESS-AUTO-RENEW
               PERFORM 4110-SELECT-PENALTY-PCT
               PERFORM 4120-DESCRIBE-ITEM-TYPE
               PERFORM 4130-COUNT-NOTICE
               PERFORM 4140-SET-PAYEE-TYPE
               PERFORM 4150-SET-TRAN
               PERFORM 4160-SELECT-PENALTY-PCT
               PERFORM 4170-COUNT-PAYMENT
               PERFORM 4180-LIMIT-FEE
               PERFORM 4190-AGE-EXPIRY
               PERFORM 4200-COUNT-SEQ
               PERFORM 4210-EDIT-REJECT
               PERFORM 4220-ASSESS-AUTO-RENEW
               PERFORM 4230-AGE-LAST-STMT
               PERFORM 4240-SELECT-FEE-PCT
               PERFORM 4250-DESCRIBE-REASON
               PERFORM 4260-FLAG-FEE
               PERFORM 4270-CHECK-LAST-MAINT
               PERFORM 4280-SPLIT-ESCHEAT
               PERFORM 4290-COUNT-PAYMENT
               PERFORM 4300-SET-TRAN
               PERFORM 4310-ACCRUE-PENALTY-PCT
               PERFORM 4320-FLAG-FEE
               PERFORM 4330-DESCRIBE-CYCLE
               PERFORM 4340-FLAG-OUTSTAND
               PERFORM 4350-EDIT-REF-NO
               PERFORM 4360-DESCRIBE-REASON
               PERFORM 4370-ASSESS-REVERSAL
               PERFORM 4380-SPLIT-OUTSTAND
               PERFORM 4390-SELECT-FEE-PCT
               PERFORM 4400-EDIT-DESC
               PERFORM 4410-FLAG-ESCHEAT
               PERFORM 4420-ACCRUE-ESCHEAT
               PERFORM 4430-DESCRIBE-TRAN
               PERFORM 4440-PRORATE-FEE
               PERFORM 4450-CHECK-POST
               PERFORM 4460-LIMIT-ITEM
               PERFORM 4470-FLAG-FACE
               PERFORM 4480-SELECT-PENALTY-PCT
               PERFORM 4490-COUNT-ITEM
               PERFORM 4500-ACCUM-ITEM-TYPE
               PERFORM 4510-ASSESS-OVERRIDE
               PERFORM 4520-AGE-LAST-MAINT
               PERFORM 7020-CALL-MODTE
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
      * DAYS SINCE NOTICE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3000-CHECK-NOTICE.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO MORM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * IS -> PD WHEN EFFECTIVE DATE IS PAST
      * PD -> VD WHEN EFFECTIVE DATE IS PAST
      * VD -> SP WHEN EFFECTIVE DATE IS PAST
      * SP -> ES WHEN EFFECTIVE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3010-SET-TRAN.
           EVALUATE MORT-TRAN-CD
               WHEN 'IS'
                   IF MORM-EFFECTIVE-DATE > 0 AND MORM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PD' TO MORT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PD'
                   IF MORM-EFFECTIVE-DATE > 0 AND MORM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VD' TO MORT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VD'
                   IF MORM-EFFECTIVE-DATE > 0 AND MORM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SP' TO MORT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SP'
                   IF MORM-EFFECTIVE-DATE > 0 AND MORM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ES' TO MORT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT PAID AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3020-SPLIT-PAID.
           COMPUTE WS-WORK-AMT = MORM-PAID-AMT / 12
           COMPUTE WS-WORK-AMT-2 = MORM-PAID-AMT - (WS-WORK-AMT * 12)
           COMPUTE MORM-VOIDED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE ITEM TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3030-DESCRIBE-ITEM-TYPE.
           EVALUATE MORM-ITEM-TYPE-CD
               WHEN 'B'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'H'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'S'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'T'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAID YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3040-ACCRUE-PAID.
           IF MORM-PAID-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORM-PAID-YTD * MORM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO MORM-ESCHEAT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE GRID TIER FOR THE ESCHEAT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3050-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-GRID-RATE(1) TO MORM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF MORM-ESCHEAT-AMT NOT < WS-FEE-GRID-LOW(WS-TX)
                   MOVE WS-FEE-GRID-RATE(WS-TX) TO MORM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MORM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MORM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3060-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF MORM-LAST-STMT-DATE > 0 AND MORM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MORM-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD MORM-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD MORM-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD MORM-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MORM-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3070-COUNT-RETRY.
           IF MORM-RETRY-CNT < 9999999
               ADD 1 TO MORM-RETRY-CNT
           END-IF
           IF MORM-RETRY-CNT NOT < 12
               MOVE 'Y' TO MORM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3080-DESCRIBE-TRAN.
           EVALUATE MORT-TRAN-CD
               WHEN 'IS'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'PD'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'VD'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'SP'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'ES'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE FEE TIER TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3090-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-TIER-RATE(1) TO MORM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF WS-WORK-AMT NOT < WS-FEE-TIER-LOW(WS-TX)
                   MOVE WS-FEE-TIER-RATE(WS-TX) TO MORM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MORM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MORM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FACE AMT BY PAYEE TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3100-ACCUM-PAYEE-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MORM-PAYEE-TYPE-CD
                   ADD MORM-FACE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MORM-PAYEE-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MORM-FACE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E087 MEMO REQUIRED
      * E093 PAID AMT NEGATIVE
      * E396 PAID AMT OVER 25000
      * E716 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3110-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MORT-MEMO = SPACES
                   MOVE 'E087' TO WS-REJECT-CODE
               WHEN MORM-PAID-AMT < 0
                   MOVE 'E093' TO WS-REJECT-CODE
               WHEN MORM-PAID-AMT > 25000.00
                   MOVE 'E396' TO WS-REJECT-CODE
               WHEN MORM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E716' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE VOIDED FEE ON THE OUTSTAND AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3120-ASSESS-STOP.
           EVALUATE TRUE
               WHEN MORM-STOP-FLAG = 'Y'
                   MOVE ZERO TO MORM-VOIDED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MORM-REVIEW-FLAG = 'Y'
                   COMPUTE MORM-VOIDED-AMT ROUNDED = MORM-OUTSTAND-AMT *
                       MORM-DISCOUNT / 200
               WHEN MORM-REASON-CD = 'B'
                   MOVE 3.00 TO MORM-VOIDED-AMT
               WHEN MORM-REASON-CD = 'L'
                   MOVE 5.00 TO MORM-VOIDED-AMT
               WHEN OTHER
                   COMPUTE MORM-VOIDED-AMT ROUNDED = MORM-OUTSTAND-AMT *
                       MORM-DISCOUNT / 100
           END-EVALUATE
           IF MORM-VOIDED-AMT > 0 AND MORM-VOIDED-AMT < 2.00
               MOVE 2.00 TO MORM-VOIDED-AMT
           END-IF
           IF MORM-VOIDED-AMT > 250.00
               MOVE 250.00 TO MORM-VOIDED-AMT
           END-IF
           ADD MORM-VOIDED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE PAID AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3130-PRORATE-PAID.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   MORM-PAID-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE FEE FEE ON THE ESCHEAT YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3140-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN MORT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO MORT-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MORM-OVERRIDE-FLAG = 'Y'
                   COMPUTE MORT-FEE-AMT ROUNDED = MORM-ESCHEAT-YTD *
                       MORM-DISCOUNT / 200
               WHEN MORM-ITEM-TYPE-CD = 'B'
                   MOVE 10.00 TO MORT-FEE-AMT
               WHEN MORM-ITEM-TYPE-CD = 'H'
                   MOVE 5.00 TO MORT-FEE-AMT
               WHEN OTHER
                   COMPUTE MORT-FEE-AMT ROUNDED = MORM-ESCHEAT-YTD *
                       MORM-DISCOUNT / 100
           END-EVALUATE
           IF MORT-FEE-AMT > 0 AND MORT-FEE-AMT < 1.00
               MOVE 1.00 TO MORT-FEE-AMT
           END-IF
           IF MORT-FEE-AMT > 150.00
               MOVE 150.00 TO MORT-FEE-AMT
           END-IF
           ADD MORT-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ESCHEAT AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3150-ACCRUE-ESCHEAT.
           IF MORM-ESCHEAT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORM-ESCHEAT-AMT * MORM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO MORT-ESCHEAT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3160-AGE-NOTICE.
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
                   ADD MORT-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD MORT-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD MORT-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD MORT-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MORT-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE VOIDED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FEE AMT.
      *----------------------------------------------------------------*
       3170-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MORT-FEE-AMT ROUNDED =
                   MORM-VOIDED-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MORT-FEE-AMT
           END-IF
           ADD MORT-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3180-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF MORM-EXPIRY-DATE > 0 AND MORM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MORM-OUTSTAND-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD MORM-OUTSTAND-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD MORM-OUTSTAND-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD MORM-OUTSTAND-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MORM-OUTSTAND-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FACE YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3190-ACCRUE-FACE.
           IF MORM-FACE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORM-FACE-YTD * MORM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO MORM-PAID-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FACE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3200-SPLIT-FACE.
           COMPUTE WS-WORK-AMT = MORM-FACE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = MORM-FACE-AMT - (WS-WORK-AMT * 4)
           COMPUTE MORM-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE ITEM TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3210-DESCRIBE-ITEM-TYPE.
           EVALUATE MORM-ITEM-TYPE-CD
               WHEN 'B'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'H'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'S'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'T'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 45 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3220-CHECK-EFF.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO MORM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3230-CHECK-NOTICE.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO MORM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE OUTSTAND YTD FEE ON THE PAID AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3240-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN MORM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO MORM-OUTSTAND-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MORT-REVERSAL-FLAG = 'Y'
                   COMPUTE MORM-OUTSTAND-YTD ROUNDED = MORM-PAID-AMT *
                       MORM-FEE-PCT / 200
               WHEN MORM-CYCLE-CD = 'DGU'
                   MOVE 20.00 TO MORM-OUTSTAND-YTD
               WHEN MORM-CYCLE-CD = 'GEJ'
                   MOVE 5.00 TO MORM-OUTSTAND-YTD
               WHEN OTHER
                   COMPUTE MORM-OUTSTAND-YTD ROUNDED = MORM-PAID-AMT *
                       MORM-FEE-PCT / 100
           END-EVALUATE
           IF MORM-OUTSTAND-YTD > 0 AND MORM-OUTSTAND-YTD < 1.00
               MOVE 1.00 TO MORM-OUTSTAND-YTD
           END-IF
           IF MORM-OUTSTAND-YTD > 35.00
               MOVE 35.00 TO MORM-OUTSTAND-YTD
           END-IF
           ADD MORM-OUTSTAND-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE PAYEE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3250-DESCRIBE-PAYEE-TYPE.
           EVALUATE MORM-PAYEE-TYPE-CD
               WHEN 'CJ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'FY'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'HX'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'UV'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'VV'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE FEE GRID TIER FOR THE VOIDED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3260-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-GRID-RATE(1) TO MORM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF MORM-VOIDED-AMT NOT < WS-FEE-GRID-LOW(WS-TX)
                   MOVE WS-FEE-GRID-RATE(WS-TX) TO MORM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MORM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MORM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ESCHEAT YTD FEE ON THE WORK AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3270-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN MORM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO MORM-ESCHEAT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MORM-PREFERRED-FLAG = 'Y'
                   COMPUTE MORM-ESCHEAT-YTD ROUNDED = WS-WORK-AMT *
                       MORM-PENALTY-PCT / 200
               WHEN MORM-TYPE-CD = 'AGE'
                   MOVE 3.00 TO MORM-ESCHEAT-YTD
               WHEN MORM-TYPE-CD = 'JDX'
                   MOVE 12.00 TO MORM-ESCHEAT-YTD
               WHEN OTHER
                   COMPUTE MORM-ESCHEAT-YTD ROUNDED = WS-WORK-AMT *
                       MORM-PENALTY-PCT / 100
           END-EVALUATE
           IF MORM-ESCHEAT-YTD > 0 AND MORM-ESCHEAT-YTD < 1.00
               MOVE 1.00 TO MORM-ESCHEAT-YTD
           END-IF
           IF MORM-ESCHEAT-YTD > 100.00
               MOVE 100.00 TO MORM-ESCHEAT-YTD
           END-IF
           ADD MORM-ESCHEAT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAID AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3280-ACCRUE-PAID.
           IF MORM-PAID-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORM-PAID-AMT * MORM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO MORM-ESCHEAT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FACE AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3290-ACCRUE-FACE.
           IF MORM-FACE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORM-FACE-AMT * MORM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO MORT-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE TIER TIER FOR THE VOIDED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3300-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-TIER-RATE(1) TO MORM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF MORM-VOIDED-YTD NOT < WS-FEE-TIER-LOW(WS-TX)
                   MOVE WS-FEE-TIER-RATE(WS-TX) TO MORM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MORM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MORM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE VOIDED SCHED TIER FOR THE OUTSTAND AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3310-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-VOIDED-SCHED-RATE(1) TO MORM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF MORM-OUTSTAND-AMT NOT < WS-VOIDED-SCHED-LOW(WS-TX)
                   MOVE WS-VOIDED-SCHED-RATE(WS-TX) TO MORM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MORM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MORM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FACE AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3320-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MORM-TYPE-CD
                   ADD MORT-FACE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MORM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MORT-FACE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAID FEE ON THE OUTSTAND YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3330-ASSESS-STOP.
           EVALUATE TRUE
               WHEN MORM-STOP-FLAG = 'Y'
                   MOVE ZERO TO MORM-PAID-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MORT-REVERSAL-FLAG = 'Y'
                   COMPUTE MORM-PAID-AMT ROUNDED = MORM-OUTSTAND-YTD *
                       MORM-FEE-PCT / 200
               WHEN MORM-ITEM-TYPE-CD = 'B'
                   MOVE 3.00 TO MORM-PAID-AMT
               WHEN MORM-ITEM-TYPE-CD = 'H'
                   MOVE 3.00 TO MORM-PAID-AMT
               WHEN OTHER
                   COMPUTE MORM-PAID-AMT ROUNDED = MORM-OUTSTAND-YTD *
                       MORM-FEE-PCT / 100
           END-EVALUATE
           IF MORM-PAID-AMT > 0 AND MORM-PAID-AMT < 1.00
               MOVE 1.00 TO MORM-PAID-AMT
           END-IF
           IF MORM-PAID-AMT > 25.00
               MOVE 25.00 TO MORM-PAID-AMT
           END-IF
           ADD MORM-PAID-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3340-COUNT-NOTICE.
           IF MORM-NOTICE-CNT < 9999999
               ADD 1 TO MORM-NOTICE-CNT
           END-IF
           IF MORM-NOTICE-CNT NOT < 3
               MOVE 'Y' TO MORT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3350-FLAG-ITEM.
           IF WS-WORK-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E608 DESC REQUIRED
      * E339 PAID AMT NEGATIVE
      * E892 PAID AMT OVER 50000
      * E510 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3360-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MORM-DESC = SPACES
                   MOVE 'E608' TO WS-REJECT-CODE
               WHEN MORM-PAID-AMT < 0
                   MOVE 'E339' TO WS-REJECT-CODE
               WHEN MORM-PAID-AMT > 50000.00
                   MOVE 'E892' TO WS-REJECT-CODE
               WHEN MORT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E510' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3370-COUNT-RETRY.
           IF MORM-RETRY-CNT < 9999999
               ADD 1 TO MORM-RETRY-CNT
           END-IF
           IF MORM-RETRY-CNT NOT < 10
               MOVE 'Y' TO MORM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ESCHEAT AMT IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3380-LIMIT-ESCHEAT.
           IF MORM-ESCHEAT-AMT < 25.00
               MOVE 25.00 TO MORM-ESCHEAT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MORM-ESCHEAT-AMT > 2500.00
               MOVE 2500.00 TO MORM-ESCHEAT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PAYEE TYPE CD:
      * CJ -> FY WHEN LAST STMT DATE IS PAST
      * FY -> HX WHEN LAST STMT DATE IS PAST
      * HX -> UV WHEN LAST STMT DATE IS PAST
      * UV -> VV WHEN LAST STMT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3390-SET-PAYEE-TYPE.
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
      * TRANSLATE PAYEE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3400-DESCRIBE-PAYEE-TYPE.
           EVALUATE MORM-PAYEE-TYPE-CD
               WHEN 'CJ'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'FY'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'HX'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'UV'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'VV'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * IS -> PD WHEN LAST STMT DATE IS PAST
      * PD -> VD WHEN LAST STMT DATE IS PAST
      * VD -> SP WHEN LAST STMT DATE IS PAST
      * SP -> ES WHEN LAST STMT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3410-SET-TRAN.
           EVALUATE MORT-TRAN-CD
               WHEN 'IS'
                   IF MORM-LAST-STMT-DATE > 0 AND MORM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PD' TO MORT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PD'
                   IF MORM-LAST-STMT-DATE > 0 AND MORM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VD' TO MORT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VD'
                   IF MORM-LAST-STMT-DATE > 0 AND MORM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SP' TO MORT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SP'
                   IF MORM-LAST-STMT-DATE > 0 AND MORM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ES' TO MORT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3420-CHECK-LAST-PAY.
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
           IF WS-DAYS > 180
               MOVE 'Y' TO MORM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FACE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ESCHEAT AMT.
      *----------------------------------------------------------------*
       3430-PRORATE-ESCHEAT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MORM-ESCHEAT-AMT ROUNDED =
                   MORM-FACE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MORM-ESCHEAT-AMT
           END-IF
           ADD MORM-ESCHEAT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FACE AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3440-FLAG-FACE.
           IF MORT-FACE-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE MORT-FACE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3450-LIMIT-FEE.
           IF MORM-FEE-AMT < 100.00
               MOVE 100.00 TO MORM-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MORM-FEE-AMT > 2500.00
               MOVE 2500.00 TO MORM-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * OUTSTAND AMT IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3460-LIMIT-OUTSTAND.
           IF MORT-OUTSTAND-AMT < 50.00
               MOVE 50.00 TO MORT-OUTSTAND-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MORT-OUTSTAND-AMT > 2500.00
               MOVE 2500.00 TO MORT-OUTSTAND-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3470-COUNT-PAYMENT.
           IF MORM-PAYMENT-CNT < 9999999
               ADD 1 TO MORM-PAYMENT-CNT
           END-IF
           IF MORM-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO MORM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAID YTD FEE ON THE FACE YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3480-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN MORM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO MORM-PAID-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MORM-OVERRIDE-FLAG = 'Y'
                   COMPUTE MORM-PAID-YTD ROUNDED = MORM-FACE-YTD *
                       MORM-PENALTY-PCT / 200
               WHEN MORM-PAYEE-TYPE-CD = 'CJ'
                   MOVE 20.00 TO MORM-PAID-YTD
               WHEN MORM-PAYEE-TYPE-CD = 'FY'
                   MOVE 7.00 TO MORM-PAID-YTD
               WHEN OTHER
                   COMPUTE MORM-PAID-YTD ROUNDED = MORM-FACE-YTD *
                       MORM-PENALTY-PCT / 100
           END-EVALUATE
           IF MORM-PAID-YTD > 0 AND MORM-PAID-YTD < 10.00
               MOVE 10.00 TO MORM-PAID-YTD
           END-IF
           IF MORM-PAID-YTD > 75.00
               MOVE 75.00 TO MORM-PAID-YTD
           END-IF
           ADD MORM-PAID-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE VOIDED SCHED TIER FOR THE FACE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3490-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-VOIDED-SCHED-RATE(1) TO MORM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF MORM-FACE-YTD NOT < WS-VOIDED-SCHED-LOW(WS-TX)
                   MOVE WS-VOIDED-SCHED-RATE(WS-TX) TO MORM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MORM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MORM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3500-DESCRIBE-CHANNEL.
           EVALUATE MORM-CHANNEL-CD
               WHEN 'DR'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'DX'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'PT'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'RN'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'YD'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'YU'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE FEE TIER TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3510-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-TIER-RATE(1) TO MORM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF MORT-FEE-AMT NOT < WS-FEE-TIER-LOW(WS-TX)
                   MOVE WS-FEE-TIER-RATE(WS-TX) TO MORM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MORM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MORM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FEE FEE ON THE OUTSTAND AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3520-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN MORM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO MORM-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MORM-STOP-FLAG = 'Y'
                   COMPUTE MORM-FEE-AMT ROUNDED = MORT-OUTSTAND-AMT *
                       MORM-FEE-PCT / 200
               WHEN MORM-ITEM-TYPE-CD = 'B'
                   MOVE 15.00 TO MORM-FEE-AMT
               WHEN MORM-ITEM-TYPE-CD = 'H'
                   MOVE 20.00 TO MORM-FEE-AMT
               WHEN OTHER
                   COMPUTE MORM-FEE-AMT ROUNDED = MORT-OUTSTAND-AMT *
                       MORM-FEE-PCT / 100
           END-EVALUATE
           IF MORM-FEE-AMT > 0 AND MORM-FEE-AMT < 5.00
               MOVE 5.00 TO MORM-FEE-AMT
           END-IF
           IF MORM-FEE-AMT > 50.00
               MOVE 50.00 TO MORM-FEE-AMT
           END-IF
           ADD MORM-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT VOIDED AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3530-SPLIT-VOIDED.
           COMPUTE WS-WORK-AMT = MORM-VOIDED-AMT / 4
           COMPUTE WS-WORK-AMT-2 = MORM-VOIDED-AMT - (WS-WORK-AMT * 4)
           COMPUTE MORM-VOIDED-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3540-COUNT-PAYMENT.
           IF MORM-PAYMENT-CNT < 9999999
               ADD 1 TO MORM-PAYMENT-CNT
           END-IF
           IF MORM-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO MORT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON OUTSTAND AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3550-ACCRUE-OUTSTAND.
           IF MORM-OUTSTAND-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORM-OUTSTAND-AMT * MORM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO MORM-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * AGE -> JDX WHEN LAST ACT DATE IS PAST
      * JDX -> LDX WHEN LAST ACT DATE IS PAST
      * LDX -> RHT WHEN LAST ACT DATE IS PAST
      * RHT -> AGE WHEN LAST ACT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3560-SET-TYPE.
           EVALUATE MORM-TYPE-CD
               WHEN 'AGE'
                   IF MORM-LAST-ACT-DATE > 0 AND MORM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JDX' TO MORM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JDX'
                   IF MORM-LAST-ACT-DATE > 0 AND MORM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LDX' TO MORM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LDX'
                   IF MORM-LAST-ACT-DATE > 0 AND MORM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RHT' TO MORM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RHT'
                   IF MORM-LAST-ACT-DATE > 0 AND MORM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AGE' TO MORM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3570-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MORM-TYPE-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MORM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE ITEM TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3580-DESCRIBE-ITEM-TYPE.
           EVALUATE MORM-ITEM-TYPE-CD
               WHEN 'B'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'H'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'S'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'T'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3590-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF MORM-LAST-PAY-DATE > 0 AND MORM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORM-LAST-PAY-DATE TO DTU-DATE-1
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
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * IS -> PD WHEN POST DATE IS PAST
      * PD -> VD WHEN POST DATE IS PAST
      * VD -> SP WHEN POST DATE IS PAST
      * SP -> ES WHEN POST DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3600-SET-TRAN.
           EVALUATE MORT-TRAN-CD
               WHEN 'IS'
                   IF MORT-POST-DATE > 0 AND MORT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PD' TO MORT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PD'
                   IF MORT-POST-DATE > 0 AND MORT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VD' TO MORT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VD'
                   IF MORT-POST-DATE > 0 AND MORT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SP' TO MORT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SP'
                   IF MORT-POST-DATE > 0 AND MORT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ES' TO MORT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE FACE FEE ON THE ESCHEAT AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3610-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN MORM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO MORT-FACE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MORM-PREFERRED-FLAG = 'Y'
                   COMPUTE MORT-FACE-AMT ROUNDED = MORT-ESCHEAT-AMT *
                       MORM-FEE-PCT / 200
               WHEN MORM-CHANNEL-CD = 'DR'
                   MOVE 5.00 TO MORT-FACE-AMT
               WHEN MORM-CHANNEL-CD = 'DX'
                   MOVE 15.00 TO MORT-FACE-AMT
               WHEN OTHER
                   COMPUTE MORT-FACE-AMT ROUNDED = MORT-ESCHEAT-AMT *
                       MORM-FEE-PCT / 100
           END-EVALUATE
           IF MORT-FACE-AMT > 0 AND MORT-FACE-AMT < 2.00
               MOVE 2.00 TO MORT-FACE-AMT
           END-IF
           IF MORT-FACE-AMT > 50.00
               MOVE 50.00 TO MORT-FACE-AMT
           END-IF
           ADD MORT-FACE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE FEE TIER TIER FOR THE PAID YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3620-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-TIER-RATE(1) TO MORM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF MORM-PAID-YTD NOT < WS-FEE-TIER-LOW(WS-TX)
                   MOVE WS-FEE-TIER-RATE(WS-TX) TO MORM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MORM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MORM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3630-DESCRIBE-CYCLE.
           EVALUATE MORM-CYCLE-CD
               WHEN 'DGU'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'GEJ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'JBE'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E783 NAME REQUIRED
      * E531 FEE YTD NEGATIVE
      * E429 FEE YTD OVER 5000
      * E371 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3640-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MORM-NAME = SPACES
                   MOVE 'E783' TO WS-REJECT-CODE
               WHEN MORM-FEE-YTD < 0
                   MOVE 'E531' TO WS-REJECT-CODE
               WHEN MORM-FEE-YTD > 5000.00
                   MOVE 'E429' TO WS-REJECT-CODE
               WHEN MORM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E371' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE VOIDED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3650-PRORATE-VOIDED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORM-VOIDED-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE OUTSTAND FEE ON THE FEE YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3660-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN MORM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO MORT-OUTSTAND-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MORM-PREFERRED-FLAG = 'Y'
                   COMPUTE MORT-OUTSTAND-AMT ROUNDED = MORM-FEE-YTD *
                       MORM-DISCOUNT / 200
               WHEN MORT-CHANNEL-CD = 'BRN'
                   MOVE 10.00 TO MORT-OUTSTAND-AMT
               WHEN MORT-CHANNEL-CD = 'BAT'
                   MOVE 7.00 TO MORT-OUTSTAND-AMT
               WHEN OTHER
                   COMPUTE MORT-OUTSTAND-AMT ROUNDED = MORM-FEE-YTD *
                       MORM-DISCOUNT / 100
           END-EVALUATE
           IF MORT-OUTSTAND-AMT > 0 AND MORT-OUTSTAND-AMT < 2.00
               MOVE 2.00 TO MORT-OUTSTAND-AMT
           END-IF
           IF MORT-OUTSTAND-AMT > 50.00
               MOVE 50.00 TO MORT-OUTSTAND-AMT
           END-IF
           ADD MORT-OUTSTAND-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FACE YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3670-ACCRUE-FACE.
           IF MORM-FACE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORM-FACE-YTD * MORM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO MORT-ESCHEAT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * DGU -> GEJ WHEN CLOSE DATE IS PAST
      * GEJ -> JBE WHEN CLOSE DATE IS PAST
      * JBE -> DGU WHEN CLOSE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3680-SET-CYCLE.
           EVALUATE MORM-CYCLE-CD
               WHEN 'DGU'
                   IF MORM-CLOSE-DATE > 0 AND MORM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GEJ' TO MORM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GEJ'
                   IF MORM-CLOSE-DATE > 0 AND MORM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JBE' TO MORM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JBE'
                   IF MORM-CLOSE-DATE > 0 AND MORM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DGU' TO MORM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * AGE -> JDX WHEN EXPIRY DATE IS PAST
      * JDX -> LDX WHEN EXPIRY DATE IS PAST
      * LDX -> RHT WHEN EXPIRY DATE IS PAST
      * RHT -> AGE WHEN EXPIRY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3690-SET-TYPE.
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
      * STATUS TRANSITIONS FOR CYCLE CD:
      * DGU -> GEJ WHEN OPEN DATE IS PAST
      * GEJ -> JBE WHEN OPEN DATE IS PAST
      * JBE -> DGU WHEN OPEN DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3700-SET-CYCLE.
           EVALUATE MORM-CYCLE-CD
               WHEN 'DGU'
                   IF MORM-OPEN-DATE > 0 AND MORM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GEJ' TO MORM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GEJ'
                   IF MORM-OPEN-DATE > 0 AND MORM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JBE' TO MORM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JBE'
                   IF MORM-OPEN-DATE > 0 AND MORM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DGU' TO MORM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE FEE TIER TIER FOR THE VOIDED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3710-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-TIER-RATE(1) TO MORM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF MORM-VOIDED-AMT NOT < WS-FEE-TIER-LOW(WS-TX)
                   MOVE WS-FEE-TIER-RATE(WS-TX) TO MORM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MORM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MORM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3720-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF MORM-LAST-ACT-DATE > 0 AND MORM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MORT-ESCHEAT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD MORT-ESCHEAT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD MORT-ESCHEAT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD MORT-ESCHEAT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MORT-ESCHEAT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON OUTSTAND YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3730-ACCRUE-OUTSTAND.
           IF MORM-OUTSTAND-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORM-OUTSTAND-YTD * MORM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E595 DESC REQUIRED
      * E826 FACE AMT NEGATIVE
      * E764 FACE AMT OVER 50000
      * E437 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3740-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MORM-DESC = SPACES
                   MOVE 'E595' TO WS-REJECT-CODE
               WHEN MORT-FACE-AMT < 0
                   MOVE 'E826' TO WS-REJECT-CODE
               WHEN MORT-FACE-AMT > 50000.00
                   MOVE 'E764' TO WS-REJECT-CODE
               WHEN MORM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E437' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ESCHEAT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3750-PRORATE-ESCHEAT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORT-ESCHEAT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT OUTSTAND AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3760-SPLIT-OUTSTAND.
           COMPUTE WS-WORK-AMT = MORT-OUTSTAND-AMT / 2
           COMPUTE WS-WORK-AMT-2 = MORT-OUTSTAND-AMT - (WS-WORK-AMT * 2)
           COMPUTE MORM-OUTSTAND-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE VOIDED YTD FEE ON THE PAID YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3770-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN MORM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO MORM-VOIDED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MORM-STOP-FLAG = 'Y'
                   COMPUTE MORM-VOIDED-YTD ROUNDED = MORM-PAID-YTD *
                       MORM-DISCOUNT / 200
               WHEN MORT-TRAN-CD = 'IS'
                   MOVE 7.00 TO MORM-VOIDED-YTD
               WHEN MORT-TRAN-CD = 'PD'
                   MOVE 7.00 TO MORM-VOIDED-YTD
               WHEN OTHER
                   COMPUTE MORM-VOIDED-YTD ROUNDED = MORM-PAID-YTD *
                       MORM-DISCOUNT / 100
           END-EVALUATE
           IF MORM-VOIDED-YTD > 0 AND MORM-VOIDED-YTD < 2.00
               MOVE 2.00 TO MORM-VOIDED-YTD
           END-IF
           IF MORM-VOIDED-YTD > 75.00
               MOVE 75.00 TO MORM-VOIDED-YTD
           END-IF
           ADD MORM-VOIDED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 90 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3780-CHECK-LAST-STMT.
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
           IF WS-DAYS > 90
               MOVE 'Y' TO MORT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE VOIDED YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3790-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MORM-REASON-CD
                   ADD MORM-VOIDED-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MORM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MORM-VOIDED-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3800-CHECK-EFFECTIVE.
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
           IF WS-DAYS > 120
               MOVE 'Y' TO MORM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FACE AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3810-ACCUM-TYPE.
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
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E972 CUST ID REQUIRED
      * E277 VOIDED AMT NEGATIVE
      * E372 VOIDED AMT OVER 250000
      * E293 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3820-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MORM-CUST-ID = SPACES
                   MOVE 'E972' TO WS-REJECT-CODE
               WHEN MORM-VOIDED-AMT < 0
                   MOVE 'E277' TO WS-REJECT-CODE
               WHEN MORM-VOIDED-AMT > 250000.00
                   MOVE 'E372' TO WS-REJECT-CODE
               WHEN MORM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E293' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3830-CHECK-LAST-PAY.
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
           IF WS-DAYS > 120
               MOVE 'Y' TO MORM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3840-ACCRUE-FEE.
           IF MORM-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORM-FEE-YTD * MORM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO MORM-FACE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 45 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3850-CHECK-LAST-STMT.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO MORM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FACE AMT IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3860-LIMIT-FACE.
           IF MORT-FACE-AMT < 0.00
               MOVE 0.00 TO MORT-FACE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MORT-FACE-AMT > 2500.00
               MOVE 2500.00 TO MORT-FACE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3870-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF MORT-EFF-DATE > 0 AND MORT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MORT-ESCHEAT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD MORT-ESCHEAT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD MORT-ESCHEAT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD MORT-ESCHEAT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MORT-ESCHEAT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FACE YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3880-ACCRUE-FACE.
           IF MORM-FACE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORM-FACE-YTD * MORM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PAYEE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3890-DESCRIBE-PAYEE-TYPE.
           EVALUATE MORM-PAYEE-TYPE-CD
               WHEN 'CJ'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'FY'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'HX'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'UV'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'VV'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE OUTSTAND FEE ON THE VOIDED YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3900-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN MORM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO MORM-OUTSTAND-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MORM-OVERRIDE-FLAG = 'Y'
                   COMPUTE MORM-OUTSTAND-AMT ROUNDED = MORM-VOIDED-YTD *
                       MORM-DISCOUNT / 200
               WHEN MORT-TRAN-CD = 'IS'
                   MOVE 3.00 TO MORM-OUTSTAND-AMT
               WHEN MORT-TRAN-CD = 'PD'
                   MOVE 3.00 TO MORM-OUTSTAND-AMT
               WHEN OTHER
                   COMPUTE MORM-OUTSTAND-AMT ROUNDED = MORM-VOIDED-YTD *
                       MORM-DISCOUNT / 100
           END-EVALUATE
           IF MORM-OUTSTAND-AMT > 0 AND MORM-OUTSTAND-AMT < 5.00
               MOVE 5.00 TO MORM-OUTSTAND-AMT
           END-IF
           IF MORM-OUTSTAND-AMT > 250.00
               MOVE 250.00 TO MORM-OUTSTAND-AMT
           END-IF
           ADD MORM-OUTSTAND-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * B -> L WHEN EFFECTIVE DATE IS PAST
      * L -> P WHEN EFFECTIVE DATE IS PAST
      * P -> S WHEN EFFECTIVE DATE IS PAST
      * S -> T WHEN EFFECTIVE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3910-SET-REASON.
           EVALUATE MORM-REASON-CD
               WHEN 'B'
                   IF MORM-EFFECTIVE-DATE > 0 AND MORM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO MORM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF MORM-EFFECTIVE-DATE > 0 AND MORM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'P' TO MORM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'P'
                   IF MORM-EFFECTIVE-DATE > 0 AND MORM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO MORM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF MORM-EFFECTIVE-DATE > 0 AND MORM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO MORM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3920-CHECK-EFFECTIVE.
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
           IF WS-DAYS > 180
               MOVE 'Y' TO MORT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FACE YTD FEE ON THE OUTSTAND YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3930-ASSESS-STOP.
           EVALUATE TRUE
               WHEN MORM-STOP-FLAG = 'Y'
                   MOVE ZERO TO MORM-FACE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MORT-REVERSAL-FLAG = 'Y'
                   COMPUTE MORM-FACE-YTD ROUNDED = MORM-OUTSTAND-YTD *
                       MORM-PENALTY-PCT / 200
               WHEN MORM-CHANNEL-CD = 'DR'
                   MOVE 3.00 TO MORM-FACE-YTD
               WHEN MORM-CHANNEL-CD = 'DX'
                   MOVE 3.00 TO MORM-FACE-YTD
               WHEN OTHER
                   COMPUTE MORM-FACE-YTD ROUNDED = MORM-OUTSTAND-YTD *
                       MORM-PENALTY-PCT / 100
           END-EVALUATE
           IF MORM-FACE-YTD > 0 AND MORM-FACE-YTD < 10.00
               MOVE 10.00 TO MORM-FACE-YTD
           END-IF
           IF MORM-FACE-YTD > 50.00
               MOVE 50.00 TO MORM-FACE-YTD
           END-IF
           ADD MORM-FACE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3940-LIMIT-FEE.
           IF MORM-FEE-AMT < 100.00
               MOVE 100.00 TO MORM-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MORM-FEE-AMT > 500.00
               MOVE 500.00 TO MORM-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FACE YTD FEE ON THE OUTSTAND AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3950-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN MORM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO MORM-FACE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MORM-STOP-FLAG = 'Y'
                   COMPUTE MORM-FACE-YTD ROUNDED = MORM-OUTSTAND-AMT *
                       MORM-PENALTY-PCT / 200
               WHEN MORT-TRAN-CD = 'IS'
                   MOVE 5.00 TO MORM-FACE-YTD
               WHEN MORT-TRAN-CD = 'PD'
                   MOVE 7.00 TO MORM-FACE-YTD
               WHEN OTHER
                   COMPUTE MORM-FACE-YTD ROUNDED = MORM-OUTSTAND-AMT *
                       MORM-PENALTY-PCT / 100
           END-EVALUATE
           IF MORM-FACE-YTD > 0 AND MORM-FACE-YTD < 10.00
               MOVE 10.00 TO MORM-FACE-YTD
           END-IF
           IF MORM-FACE-YTD > 50.00
               MOVE 50.00 TO MORM-FACE-YTD
           END-IF
           ADD MORM-FACE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT ESCHEAT YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3960-SPLIT-ESCHEAT.
           COMPUTE WS-WORK-AMT = MORM-ESCHEAT-YTD / 2
           COMPUTE WS-WORK-AMT-2 = MORM-ESCHEAT-YTD - (WS-WORK-AMT * 2)
           COMPUTE MORT-FACE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FEE FEE ON THE VOIDED YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3970-ASSESS-STOP.
           EVALUATE TRUE
               WHEN MORM-STOP-FLAG = 'Y'
                   MOVE ZERO TO MORM-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MORM-PREFERRED-FLAG = 'Y'
                   COMPUTE MORM-FEE-AMT ROUNDED = MORM-VOIDED-YTD *
                       MORM-PENALTY-PCT / 200
               WHEN MORT-CHANNEL-CD = 'BRN'
                   MOVE 12.00 TO MORM-FEE-AMT
               WHEN MORT-CHANNEL-CD = 'BAT'
                   MOVE 7.00 TO MORM-FEE-AMT
               WHEN OTHER
                   COMPUTE MORM-FEE-AMT ROUNDED = MORM-VOIDED-YTD *
                       MORM-PENALTY-PCT / 100
           END-EVALUATE
           IF MORM-FEE-AMT > 0 AND MORM-FEE-AMT < 1.00
               MOVE 1.00 TO MORM-FEE-AMT
           END-IF
           IF MORM-FEE-AMT > 250.00
               MOVE 250.00 TO MORM-FEE-AMT
           END-IF
           ADD MORM-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3980-COUNT-RETRY.
           IF MORM-RETRY-CNT < 9999999
               ADD 1 TO MORM-RETRY-CNT
           END-IF
           IF MORM-RETRY-CNT NOT < 5
               MOVE 'Y' TO MORM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE ITEM TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3990-DESCRIBE-ITEM-TYPE.
           EVALUATE MORM-ITEM-TYPE-CD
               WHEN 'B'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'H'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'S'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'T'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * AGE -> JDX WHEN LAST STMT DATE IS PAST
      * JDX -> LDX WHEN LAST STMT DATE IS PAST
      * LDX -> RHT WHEN LAST STMT DATE IS PAST
      * RHT -> AGE WHEN LAST STMT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4000-SET-TYPE.
           EVALUATE MORM-TYPE-CD
               WHEN 'AGE'
                   IF MORM-LAST-STMT-DATE > 0 AND MORM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JDX' TO MORM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JDX'
                   IF MORM-LAST-STMT-DATE > 0 AND MORM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LDX' TO MORM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LDX'
                   IF MORM-LAST-STMT-DATE > 0 AND MORM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RHT' TO MORM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RHT'
                   IF MORM-LAST-STMT-DATE > 0 AND MORM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AGE' TO MORM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FEE YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4010-FLAG-FEE.
           IF MORM-FEE-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE MORM-FEE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE VOIDED SCHED TIER FOR THE FACE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4020-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-VOIDED-SCHED-RATE(1) TO MORM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF MORM-FACE-AMT NOT < WS-VOIDED-SCHED-LOW(WS-TX)
                   MOVE WS-VOIDED-SCHED-RATE(WS-TX) TO MORM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MORM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MORM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4030-DESCRIBE-CYCLE.
           EVALUATE MORM-CYCLE-CD
               WHEN 'DGU'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'GEJ'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'JBE'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4040-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF MORM-EFFECTIVE-DATE > 0 AND MORM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MORM-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD MORM-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD MORM-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD MORM-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MORM-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * AGE -> JDX WHEN OPEN DATE IS PAST
      * JDX -> LDX WHEN OPEN DATE IS PAST
      * LDX -> RHT WHEN OPEN DATE IS PAST
      * RHT -> AGE WHEN OPEN DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4050-SET-TYPE.
           EVALUATE MORM-TYPE-CD
               WHEN 'AGE'
                   IF MORM-OPEN-DATE > 0 AND MORM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JDX' TO MORM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JDX'
                   IF MORM-OPEN-DATE > 0 AND MORM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LDX' TO MORM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LDX'
                   IF MORM-OPEN-DATE > 0 AND MORM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RHT' TO MORM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RHT'
                   IF MORM-OPEN-DATE > 0 AND MORM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AGE' TO MORM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 120 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4060-CHECK-LAST-MAINT.
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
               MOVE 'Y' TO MORM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ESCHEAT YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4070-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MORM-CHANNEL-CD
                   ADD MORM-ESCHEAT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MORM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MORM-ESCHEAT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4080-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MORM-REASON-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MORM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       4090-COUNT-NOTICE.
           IF MORM-NOTICE-CNT < 9999999
               ADD 1 TO MORM-NOTICE-CNT
           END-IF
           IF MORM-NOTICE-CNT NOT < 5
               MOVE 'Y' TO MORM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE OUTSTAND FEE ON THE ESCHEAT AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4100-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN MORM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO MORM-OUTSTAND-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MORM-EXEMPT-FLAG = 'Y'
                   COMPUTE MORM-OUTSTAND-AMT ROUNDED = MORT-ESCHEAT-AMT
                       * MORM-FEE-PCT / 200
               WHEN MORT-TRAN-CD = 'IS'
                   MOVE 10.00 TO MORM-OUTSTAND-AMT
               WHEN MORT-TRAN-CD = 'PD'
                   MOVE 20.00 TO MORM-OUTSTAND-AMT
               WHEN OTHER
                   COMPUTE MORM-OUTSTAND-AMT ROUNDED = MORT-ESCHEAT-AMT
                       * MORM-FEE-PCT / 100
           END-EVALUATE
           IF MORM-OUTSTAND-AMT > 0 AND MORM-OUTSTAND-AMT < 5.00
               MOVE 5.00 TO MORM-OUTSTAND-AMT
           END-IF
           IF MORM-OUTSTAND-AMT > 100.00
               MOVE 100.00 TO MORM-OUTSTAND-AMT
           END-IF
           ADD MORM-OUTSTAND-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE VOIDED SCHED TIER FOR THE ESCHEAT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4110-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-VOIDED-SCHED-RATE(1) TO MORM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF MORM-ESCHEAT-AMT NOT < WS-VOIDED-SCHED-LOW(WS-TX)
                   MOVE WS-VOIDED-SCHED-RATE(WS-TX) TO MORM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MORM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MORM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE ITEM TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4120-DESCRIBE-ITEM-TYPE.
           EVALUATE MORM-ITEM-TYPE-CD
               WHEN 'B'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'H'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'S'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'T'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       4130-COUNT-NOTICE.
           IF MORM-NOTICE-CNT < 9999999
               ADD 1 TO MORM-NOTICE-CNT
           END-IF
           IF MORM-NOTICE-CNT NOT < 5
               MOVE 'Y' TO MORM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PAYEE TYPE CD:
      * CJ -> FY WHEN LAST STMT DATE IS PAST
      * FY -> HX WHEN LAST STMT DATE IS PAST
      * HX -> UV WHEN LAST STMT DATE IS PAST
      * UV -> VV WHEN LAST STMT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4140-SET-PAYEE-TYPE.
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
      * STATUS TRANSITIONS FOR TRAN CD:
      * IS -> PD WHEN NOTICE DATE IS PAST
      * PD -> VD WHEN NOTICE DATE IS PAST
      * VD -> SP WHEN NOTICE DATE IS PAST
      * SP -> ES WHEN NOTICE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4150-SET-TRAN.
           EVALUATE MORT-TRAN-CD
               WHEN 'IS'
                   IF MORM-NOTICE-DATE > 0 AND MORM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PD' TO MORT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PD'
                   IF MORM-NOTICE-DATE > 0 AND MORM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VD' TO MORT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VD'
                   IF MORM-NOTICE-DATE > 0 AND MORM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SP' TO MORT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SP'
                   IF MORM-NOTICE-DATE > 0 AND MORM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ES' TO MORT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE FEE TIER TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4160-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-TIER-RATE(1) TO MORM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF MORT-FEE-AMT NOT < WS-FEE-TIER-LOW(WS-TX)
                   MOVE WS-FEE-TIER-RATE(WS-TX) TO MORM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MORM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MORM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4170-COUNT-PAYMENT.
           IF MORM-PAYMENT-CNT < 9999999
               ADD 1 TO MORM-PAYMENT-CNT
           END-IF
           IF MORM-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO MORM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4180-LIMIT-FEE.
           IF MORT-FEE-AMT < 10.00
               MOVE 10.00 TO MORT-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MORT-FEE-AMT > 2500.00
               MOVE 2500.00 TO MORT-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       4190-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF MORM-EXPIRY-DATE > 0 AND MORM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MORM-ESCHEAT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD MORM-ESCHEAT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD MORM-ESCHEAT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD MORM-ESCHEAT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MORM-ESCHEAT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       4200-COUNT-SEQ.
           IF MORT-SEQ < 9999999
               ADD 1 TO MORT-SEQ
           END-IF
           IF MORT-SEQ NOT < 3
               MOVE 'Y' TO MORM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E216 REJECT CD REQUIRED
      * E988 OUTSTAND YTD NEGATIVE
      * E557 OUTSTAND YTD OVER 1000000
      * E555 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       4210-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MORT-REJECT-CD = SPACES
                   MOVE 'E216' TO WS-REJECT-CODE
               WHEN MORM-OUTSTAND-YTD < 0
                   MOVE 'E988' TO WS-REJECT-CODE
               WHEN MORM-OUTSTAND-YTD > 1000000.00
                   MOVE 'E557' TO WS-REJECT-CODE
               WHEN MORT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E555' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ESCHEAT YTD FEE ON THE OUTSTAND YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4220-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN MORM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO MORM-ESCHEAT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MORM-EXEMPT-FLAG = 'Y'
                   COMPUTE MORM-ESCHEAT-YTD ROUNDED = MORM-OUTSTAND-YTD
                       * MORM-FEE-PCT / 200
               WHEN MORT-CHANNEL-CD = 'BRN'
                   MOVE 7.00 TO MORM-ESCHEAT-YTD
               WHEN MORT-CHANNEL-CD = 'BAT'
                   MOVE 7.00 TO MORM-ESCHEAT-YTD
               WHEN OTHER
                   COMPUTE MORM-ESCHEAT-YTD ROUNDED = MORM-OUTSTAND-YTD
                       * MORM-FEE-PCT / 100
           END-EVALUATE
           IF MORM-ESCHEAT-YTD > 0 AND MORM-ESCHEAT-YTD < 5.00
               MOVE 5.00 TO MORM-ESCHEAT-YTD
           END-IF
           IF MORM-ESCHEAT-YTD > 50.00
               MOVE 50.00 TO MORM-ESCHEAT-YTD
           END-IF
           ADD MORM-ESCHEAT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       4230-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF MORM-LAST-STMT-DATE > 0 AND MORM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MORM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MORM-VOIDED-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD MORM-VOIDED-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD MORM-VOIDED-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD MORM-VOIDED-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MORM-VOIDED-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE VOIDED SCHED TIER FOR THE PAID YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4240-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-VOIDED-SCHED-RATE(1) TO MORM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF MORM-PAID-YTD NOT < WS-VOIDED-SCHED-LOW(WS-TX)
                   MOVE WS-VOIDED-SCHED-RATE(WS-TX) TO MORM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MORM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MORM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4250-DESCRIBE-REASON.
           EVALUATE MORM-REASON-CD
               WHEN 'B'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'L'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'P'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'S'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'T'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'Z'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FEE AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4260-FLAG-FEE.
           IF MORT-FEE-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE MORT-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 120 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4270-CHECK-LAST-MAINT.
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
               MOVE 'Y' TO MORM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ESCHEAT YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4280-SPLIT-ESCHEAT.
           COMPUTE WS-WORK-AMT = MORM-ESCHEAT-YTD / 3
           COMPUTE WS-WORK-AMT-2 = MORM-ESCHEAT-YTD - (WS-WORK-AMT * 3)
           COMPUTE MORM-FACE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       4290-COUNT-PAYMENT.
           IF MORM-PAYMENT-CNT < 9999999
               ADD 1 TO MORM-PAYMENT-CNT
           END-IF
           IF MORM-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO MORM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * IS -> PD WHEN LAST STMT DATE IS PAST
      * PD -> VD WHEN LAST STMT DATE IS PAST
      * VD -> SP WHEN LAST STMT DATE IS PAST
      * SP -> ES WHEN LAST STMT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4300-SET-TRAN.
           EVALUATE MORT-TRAN-CD
               WHEN 'IS'
                   IF MORM-LAST-STMT-DATE > 0 AND MORM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PD' TO MORT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PD'
                   IF MORM-LAST-STMT-DATE > 0 AND MORM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VD' TO MORT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VD'
                   IF MORM-LAST-STMT-DATE > 0 AND MORM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SP' TO MORT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SP'
                   IF MORM-LAST-STMT-DATE > 0 AND MORM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ES' TO MORT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4310-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * MORM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO MORT-OUTSTAND-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4320-FLAG-FEE.
           IF MORM-FEE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE MORM-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4330-DESCRIBE-CYCLE.
           EVALUATE MORM-CYCLE-CD
               WHEN 'DGU'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'GEJ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'JBE'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * OUTSTAND YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4340-FLAG-OUTSTAND.
           IF MORM-OUTSTAND-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE MORM-OUTSTAND-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E388 REF NO REQUIRED
      * E527 WORK AMT 2 NEGATIVE
      * E839 WORK AMT 2 OVER 99999
      * E013 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       4350-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MORM-REF-NO = SPACES
                   MOVE 'E388' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E527' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 99999.00
                   MOVE 'E839' TO WS-REJECT-CODE
               WHEN MORT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E013' TO WS-REJECT-CODE
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
       4360-DESCRIBE-REASON.
           EVALUATE MORM-REASON-CD
               WHEN 'B'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'L'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'P'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'S'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'T'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'Z'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE FACE YTD FEE ON THE FACE AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4370-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN MORT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO MORM-FACE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MORM-OVERRIDE-FLAG = 'Y'
                   COMPUTE MORM-FACE-YTD ROUNDED = MORT-FACE-AMT *
                       MORM-PENALTY-PCT / 200
               WHEN MORM-PAYEE-TYPE-CD = 'CJ'
                   MOVE 12.00 TO MORM-FACE-YTD
               WHEN MORM-PAYEE-TYPE-CD = 'FY'
                   MOVE 12.00 TO MORM-FACE-YTD
               WHEN OTHER
                   COMPUTE MORM-FACE-YTD ROUNDED = MORT-FACE-AMT *
                       MORM-PENALTY-PCT / 100
           END-EVALUATE
           IF MORM-FACE-YTD > 0 AND MORM-FACE-YTD < 5.00
               MOVE 5.00 TO MORM-FACE-YTD
           END-IF
           IF MORM-FACE-YTD > 25.00
               MOVE 25.00 TO MORM-FACE-YTD
           END-IF
           ADD MORM-FACE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT OUTSTAND YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4380-SPLIT-OUTSTAND.
           COMPUTE WS-WORK-AMT = MORM-OUTSTAND-YTD / 4
           COMPUTE WS-WORK-AMT-2 = MORM-OUTSTAND-YTD - (WS-WORK-AMT * 4)
           COMPUTE MORM-VOIDED-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE VOIDED SCHED TIER FOR THE ESCHEAT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4390-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-VOIDED-SCHED-RATE(1) TO MORM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF MORM-ESCHEAT-AMT NOT < WS-VOIDED-SCHED-LOW(WS-TX)
                   MOVE WS-VOIDED-SCHED-RATE(WS-TX) TO MORM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MORM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MORM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E040 DESC REQUIRED
      * E081 FACE AMT NEGATIVE
      * E035 FACE AMT OVER 5000
      * E538 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       4400-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MORM-DESC = SPACES
                   MOVE 'E040' TO WS-REJECT-CODE
               WHEN MORM-FACE-AMT < 0
                   MOVE 'E081' TO WS-REJECT-CODE
               WHEN MORM-FACE-AMT > 5000.00
                   MOVE 'E035' TO WS-REJECT-CODE
               WHEN MORM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E538' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ESCHEAT AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4410-FLAG-ESCHEAT.
           IF MORT-ESCHEAT-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE MORT-ESCHEAT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ESCHEAT AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4420-ACCRUE-ESCHEAT.
           IF MORT-ESCHEAT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MORT-ESCHEAT-AMT * MORM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO MORM-VOIDED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4430-DESCRIBE-TRAN.
           EVALUATE MORT-TRAN-CD
               WHEN 'IS'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'PD'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'VD'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'SP'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'ES'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FEE AMT.
      *----------------------------------------------------------------*
       4440-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MORM-FEE-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MORM-FEE-AMT
           END-IF
           ADD MORM-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 120 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4450-CHECK-POST.
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
           IF WS-DAYS > 120
               MOVE 'Y' TO MORM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4460-LIMIT-ITEM.
           IF WS-WORK-AMT < 0.00
               MOVE 0.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 50000.00
               MOVE 50000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FACE AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4470-FLAG-FACE.
           IF MORM-FACE-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE MORM-FACE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE VOIDED SCHED TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4480-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-VOIDED-SCHED-RATE(1) TO MORM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF WS-WORK-AMT-2 NOT < WS-VOIDED-SCHED-LOW(WS-TX)
                   MOVE WS-VOIDED-SCHED-RATE(WS-TX) TO MORM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MORM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MORM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4490-COUNT-ITEM.
           IF MORM-ITEM-CNT < 9999999
               ADD 1 TO MORM-ITEM-CNT
           END-IF
           IF MORM-ITEM-CNT NOT < 10
               MOVE 'Y' TO MORT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FACE AMT BY ITEM TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4500-ACCUM-ITEM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MORM-ITEM-TYPE-CD
                   ADD MORM-FACE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MORM-ITEM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MORM-FACE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE ESCHEAT AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4510-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN MORM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MORM-REVIEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = MORT-ESCHEAT-AMT *
                       MORM-DISCOUNT / 200
               WHEN MORM-CHANNEL-CD = 'DR'
                   MOVE 5.00 TO WS-WORK-AMT-2
               WHEN MORM-CHANNEL-CD = 'DX'
                   MOVE 10.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = MORT-ESCHEAT-AMT *
                       MORM-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 10.00
               MOVE 10.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 25.00
               MOVE 25.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       4520-AGE-LAST-MAINT.
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
                   ADD MORM-OUTSTAND-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD MORM-OUTSTAND-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD MORM-OUTSTAND-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD MORM-OUTSTAND-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MORM-OUTSTAND-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

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
                   MOVE MODTE-VOIDED-AMT
                     TO MORM-VOIDED-AMT
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
           DISPLAY 'MORACCR READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'MORACCR' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
