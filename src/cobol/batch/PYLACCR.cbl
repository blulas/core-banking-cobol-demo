      *================================================================*
      * PROGRAM  : PYLACCR                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : PAYROLL SERVICES - DAILY ACCRUAL. READS THE PRL     *
      *            TRANSACTIONS, UPDATES THE PRL MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. CLIENT PAYROLL CALCULATION, TAX *
      *            DEPOSIT AND DIRECT DEPOSIT                          *
      * INPUT    : PRLMST (KSDS), PRLTRN, BANKCTL                      *
      * OUTPUT   : PRLOUT, PYLACRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, PYFEE, PYPRC, PYPEN     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1989-02-15 LMB  ORIGINAL                                       *
      * 1994-07-12 TWH  STATE-TAX RERUN PROTECTION REQ5673             *
      * 1994-08-13 RJT  GROSS ROUNDING - HALF UP REQ8359               *
      * 1994-09-03 LMB  PERFORMANCE - FED-TAX READ AHEAD REQ4223       *
      * 1998-07-06 LMB  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-09-05 SKN  GROSS EXCEPTIONS TO OPS QUEUE PRB1935          *
      * 2003-10-24 BHS  DEDUCT TABLE EXPANDED PRB1136                  *
      * 2008-10-07 BHS  DEDUCT STATUS CODES ADDED INC2508              *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PYLACCR.
       AUTHOR. T W HOLT.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "PRLMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS PRLM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "PRLTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "PRLOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "PYLACRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY PRLMST.
       FD  TRAN-FILE.
       COPY PRLTRN.
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
                                           VALUE 15.000000.
           05  WS-DAYS                     PIC S9(07) VALUE 0.
           05  WS-PERIOD-DAYS              PIC S9(05) VALUE 91.
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
      * WS-NET-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-NET-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000516500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000461000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005100000000407000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006100000000805000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007100000000236000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007600000000242500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007650000000744500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008150000000680500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008250000000761000'.
       01  WS-NET-GRID-TBL REDEFINES WS-NET-GRID-VALUES.
           05  WS-NET-GRID-ROW OCCURS 9 TIMES.
               10  WS-NET-GRID-LOW         PIC 9(09)V99.
               10  WS-NET-GRID-RATE        PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-FED-TAX-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FED-TAX-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000678500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000077000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020000000000229500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021000000000780000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023500000000127000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023600000000380000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023650000000542500'.
       01  WS-FED-TAX-GRID-TBL REDEFINES WS-FED-TAX-GRID-VALUES.
           05  WS-FED-TAX-GRID-ROW OCCURS 7 TIMES.
               10  WS-FED-TAX-GRID-LOW     PIC 9(09)V99.
               10  WS-FED-TAX-GRID-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-GROSS-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-GROSS-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000008000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000150000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005050000000125000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007550000000418000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017550000000869000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00027550000000552000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00027800000000571000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00028050000000008500'.
       01  WS-GROSS-BAND-TBL REDEFINES WS-GROSS-BAND-VALUES.
           05  WS-GROSS-BAND-ROW OCCURS 8 TIMES.
               10  WS-GROSS-BAND-LOW       PIC 9(09)V99.
               10  WS-GROSS-BAND-RATE      PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-STATE-TA-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-STATE-TA-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000089000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000367000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010500000000294500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010600000000215000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010650000000412500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020650000000868000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00030650000000319000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00030700000000500000'.
       01  WS-STATE-TA-SCHED-TBL REDEFINES WS-STATE-TA-SCHED-VALUES.
           05  WS-STATE-TA-SCHED-ROW OCCURS 8 TIMES.
               10  WS-STATE-TA-SCHED-LOW   PIC 9(09)V99.
               10  WS-STATE-TA-SCHED-RATE  PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  PYLACCR  PAYROLL SERVICES'.
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
       COPY PYFEEP.
       COPY PYPRCP.
       COPY PYPENP.

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
               MOVE 'PRLMST' TO ABND-FILE-NAME
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
           MOVE PRLT-KEY TO PRLM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-EDIT-OPERATOR
               PERFORM 3010-CHECK-NOTICE
               PERFORM 3020-SET-TRAN
               PERFORM 3030-FLAG-FICA
               PERFORM 3040-ACCRUE-GROSS
               PERFORM 3050-ACCUM-REASON
               PERFORM 3060-PRORATE-STATE-TAX
               PERFORM 3070-ACCRUE-DEDUCT
               PERFORM 3080-AGE-NEXT-DUE
               PERFORM 3090-SELECT-FEE-PCT
               PERFORM 3100-DESCRIBE-REGION
               PERFORM 3110-SELECT-FEE-PCT
               PERFORM 3120-AGE-NEXT-REVIEW
               PERFORM 3130-EDIT-CONTACT
               PERFORM 3140-COUNT-ITEM
               PERFORM 3150-SELECT-RATE
               PERFORM 3160-SET-PAY-FREQ
               PERFORM 3170-FLAG-FED-TAX
               PERFORM 3180-DESCRIBE-PAY-FREQ
               PERFORM 3190-SET-SOURCE
               PERFORM 3200-PRORATE-DEDUCT
               PERFORM 3210-SELECT-RATE
               PERFORM 3220-ACCRUE-FED-TAX
               PERFORM 3230-DESCRIBE-PAY-FREQ
               PERFORM 3240-LIMIT-DEDUCT
               PERFORM 3250-LIMIT-NET
               PERFORM 3260-LIMIT-FICA
               PERFORM 3270-DESCRIBE-REASON
               PERFORM 3280-COUNT-SEQ
               PERFORM 3290-SET-TRAN
               PERFORM 3300-SPLIT-DEDUCT
               PERFORM 3310-CHECK-NEXT-DUE
               PERFORM 3320-CHECK-NEXT-REVIEW
               PERFORM 3330-PRORATE-STATE-TAX
               PERFORM 3340-ACCRUE-FICA
               PERFORM 3350-EDIT-MEMO
               PERFORM 3360-SET-CHANNEL
               PERFORM 3370-EDIT-CUST-ID
               PERFORM 3380-ACCRUE-DEDUCT
               PERFORM 3390-COUNT-NOTICE
               PERFORM 3400-ASSESS-SUSPEND
               PERFORM 3410-ACCUM-TRAN
               PERFORM 3420-COUNT-CYCLE
               PERFORM 3430-ACCRUE-STATE-TAX
               PERFORM 3440-ACCRUE-FICA
               PERFORM 3450-PRORATE-FICA
               PERFORM 3460-SPLIT-DEDUCT
               PERFORM 3470-AGE-EXPIRY
               PERFORM 3480-DESCRIBE-STATUS
               PERFORM 3490-DESCRIBE-REASON
               PERFORM 3500-PRORATE-GROSS
               PERFORM 3510-SPLIT-STATE-TAX
               PERFORM 3520-ACCUM-REASON
               PERFORM 3530-AGE-POST
               PERFORM 3540-COUNT-NOTICE
               PERFORM 3550-FLAG-FICA
               PERFORM 3560-SELECT-MARGIN
               PERFORM 3570-SPLIT-FICA
               PERFORM 3580-ACCUM-STATUS
               PERFORM 3590-SELECT-MARGIN
               PERFORM 3600-SET-TRAN
               PERFORM 3610-PRORATE-DEDUCT
               PERFORM 3620-SELECT-FEE-PCT
               PERFORM 3630-CHECK-LAST-PAY
               PERFORM 3640-SET-PAY-FREQ
               PERFORM 3650-LIMIT-STATE-TAX
               PERFORM 3660-ACCUM-PAY-FREQ
               PERFORM 3670-AGE-LAST-ACT
               PERFORM 3680-SET-TRAN
               PERFORM 3690-ACCUM-SOURCE
               PERFORM 3700-ASSESS-REVERSAL
               PERFORM 3710-ASSESS-HOLD
               PERFORM 3720-CHECK-OPEN
               PERFORM 3730-CHECK-LAST-PAY
               PERFORM 3740-ACCUM-CHANNEL
               PERFORM 3750-AGE-LAST-ACT
               PERFORM 3760-ACCUM-PAY-FREQ
               PERFORM 3770-LIMIT-STATE-TAX
               PERFORM 3780-SELECT-FEE-PCT
               PERFORM 3790-ACCUM-CHANNEL
               PERFORM 3800-COUNT-ITEM
               PERFORM 3810-DESCRIBE-REASON
               PERFORM 3820-ACCUM-REASON
               PERFORM 3830-CHECK-LAST-PAY
               PERFORM 3840-DESCRIBE-TAX-STATE
               PERFORM 3850-FLAG-FICA
               PERFORM 3860-ACCUM-STATUS
               PERFORM 3870-ACCRUE-STATE-TAX
               PERFORM 3880-EDIT-OPERATOR
               PERFORM 3890-EDIT-REJECT
               PERFORM 3900-FLAG-FICA
               PERFORM 3910-CHECK-LAST-ACT
               PERFORM 3920-ACCUM-PAY-FREQ
               PERFORM 3930-LIMIT-NET
               PERFORM 3940-AGE-NEXT-DUE
               PERFORM 3950-FLAG-ITEM
               PERFORM 3960-SPLIT-STATE-TAX
               PERFORM 3970-AGE-EFFECTIVE
               PERFORM 3980-SPLIT-FED-TAX
               PERFORM 3990-ACCRUE-NET
               PERFORM 4000-SPLIT-DEDUCT
               PERFORM 4010-COUNT-CYCLE
               PERFORM 4020-DESCRIBE-TRAN
               PERFORM 4030-ACCUM-CHANNEL
               PERFORM 4040-SELECT-FEE-PCT
               PERFORM 4050-LIMIT-NET
               PERFORM 4060-AGE-EFFECTIVE
               PERFORM 4070-COUNT-PAYMENT
               PERFORM 4080-FLAG-GROSS
               PERFORM 4090-LIMIT-STATE-TAX
               PERFORM 4100-SELECT-MARGIN
               PERFORM 4110-AGE-NEXT-REVIEW
               PERFORM 4120-EDIT-ADDR
               PERFORM 4130-FLAG-NET
               PERFORM 4140-CHECK-EFF
               PERFORM 4150-CHECK-LAST-ACT
               PERFORM 4160-PRORATE-STATE-TAX
               PERFORM 4170-COUNT-PAYMENT
               PERFORM 4180-AGE-OPEN
               PERFORM 4190-AGE-LAST-PAY
               PERFORM 4200-CHECK-EXPIRY
               PERFORM 4210-ACCUM-PAY-FREQ
               PERFORM 4220-COUNT-SEQ
               PERFORM 4230-PRORATE-FED-TAX
               PERFORM 4240-ACCRUE-MARGIN
               PERFORM 4250-ACCUM-REASON
               PERFORM 4260-PRORATE-NET
               PERFORM 4270-ASSESS-SUSPEND
               PERFORM 4280-DESCRIBE-PAY-FREQ
               PERFORM 4290-SET-STATUS
               PERFORM 4300-COUNT-SEQ
               PERFORM 4310-ACCUM-REGION
               PERFORM 4320-PRORATE-GROSS
               PERFORM 4330-EDIT-REF-NO
               PERFORM 4340-PRORATE-GROSS
               PERFORM 4350-CHECK-EXPIRY
               PERFORM 4360-ASSESS-PREFERRED
               PERFORM 4370-AGE-LAST-ACT
               PERFORM 4380-SET-TAX-STATE
               PERFORM 4390-CHECK-NOTICE
               PERFORM 4400-CHECK-NEXT-DUE
               PERFORM 4410-AGE-NOTICE
               PERFORM 4420-SET-STATUS
               PERFORM 4430-FLAG-GROSS
               PERFORM 4440-SET-STATUS
               PERFORM 4450-SPLIT-FICA
               PERFORM 4460-PRORATE-STATE-TAX
               PERFORM 4470-LIMIT-FICA
               PERFORM 4480-ASSESS-HOLD
               PERFORM 4490-ASSESS-AUTO-RENEW
               PERFORM 4500-EDIT-MEMO
               PERFORM 4510-SPLIT-STATE-TAX
               PERFORM 4520-ASSESS-SUSPEND
               PERFORM 4530-FLAG-FICA
               PERFORM 4540-ASSESS-STOP
               PERFORM 4550-LIMIT-ITEM
               PERFORM 4560-ASSESS-REVERSAL
               PERFORM 4570-DESCRIBE-REGION
               PERFORM 4580-ACCRUE-FED-TAX
               PERFORM 4590-EDIT-NAME
               PERFORM 7010-CALL-PYFEE
               PERFORM 7000-CALL-PYPRC
               PERFORM 7040-CALL-PYPEN
               REWRITE PRL-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'PRLMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM PRL-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E362 OPERATOR REQUIRED
      * E149 FICA AMT NEGATIVE
      * E369 FICA AMT OVER 1000000
      * E178 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3000-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PRLT-OPERATOR = SPACES
                   MOVE 'E362' TO WS-REJECT-CODE
               WHEN PRLM-FICA-AMT < 0
                   MOVE 'E149' TO WS-REJECT-CODE
               WHEN PRLM-FICA-AMT > 1000000.00
                   MOVE 'E369' TO WS-REJECT-CODE
               WHEN PRLM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E178' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3010-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF PRLM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PRLM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO PRLM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * PR -> AD WHEN POST DATE IS PAST
      * AD -> VD WHEN POST DATE IS PAST
      * VD -> TX WHEN POST DATE IS PAST
      * TX -> DP WHEN POST DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3020-SET-TRAN.
           EVALUATE PRLT-TRAN-CD
               WHEN 'PR'
                   IF PRLT-POST-DATE > 0 AND PRLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AD' TO PRLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AD'
                   IF PRLT-POST-DATE > 0 AND PRLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VD' TO PRLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VD'
                   IF PRLT-POST-DATE > 0 AND PRLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TX' TO PRLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TX'
                   IF PRLT-POST-DATE > 0 AND PRLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DP' TO PRLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FICA AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3030-FLAG-FICA.
           IF PRLT-FICA-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PRLT-FICA-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON GROSS AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3040-ACCRUE-GROSS.
           IF PRLM-GROSS-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PRLM-GROSS-AMT * PRLM-RATE / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FED TAX YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3050-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PRLM-REASON-CD
                   ADD PRLM-FED-TAX-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PRLM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PRLM-FED-TAX-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FICA AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO STATE TAX AMT.
      *----------------------------------------------------------------*
       3060-PRORATE-STATE-TAX.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PRLM-STATE-TAX-AMT ROUNDED =
                   PRLM-FICA-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PRLM-STATE-TAX-AMT
           END-IF
           ADD PRLM-STATE-TAX-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DEDUCT YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3070-ACCRUE-DEDUCT.
           IF PRLM-DEDUCT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PRLM-DEDUCT-YTD * PRLM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO PRLM-FICA-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3080-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF PRLM-NEXT-DUE-DATE > 0 AND PRLM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PRLM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PRLT-DEDUCT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PRLT-DEDUCT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD PRLT-DEDUCT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD PRLT-DEDUCT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PRLT-DEDUCT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE FED TAX GRID TIER FOR THE FED TAX YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3090-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FED-TAX-GRID-RATE(1) TO PRLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF PRLM-FED-TAX-YTD NOT < WS-FED-TAX-GRID-LOW(WS-TX)
                   MOVE WS-FED-TAX-GRID-RATE(WS-TX) TO PRLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PRLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PRLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3100-DESCRIBE-REGION.
           EVALUATE PRLM-REGION-CD
               WHEN 'DP'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'TU'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'WD'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE FED TAX GRID TIER FOR THE DEDUCT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3110-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FED-TAX-GRID-RATE(1) TO PRLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF PRLT-DEDUCT-AMT NOT < WS-FED-TAX-GRID-LOW(WS-TX)
                   MOVE WS-FED-TAX-GRID-RATE(WS-TX) TO PRLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PRLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PRLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3120-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF PRLM-NEXT-REVIEW-DATE > 0 AND PRLM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PRLM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PRLT-DEDUCT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PRLT-DEDUCT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD PRLT-DEDUCT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PRLT-DEDUCT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PRLT-DEDUCT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E176 CONTACT REQUIRED
      * E548 DEDUCT YTD NEGATIVE
      * E754 DEDUCT YTD OVER 5000
      * E968 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3130-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PRLM-CONTACT = SPACES
                   MOVE 'E176' TO WS-REJECT-CODE
               WHEN PRLM-DEDUCT-YTD < 0
                   MOVE 'E548' TO WS-REJECT-CODE
               WHEN PRLM-DEDUCT-YTD > 5000.00
                   MOVE 'E754' TO WS-REJECT-CODE
               WHEN PRLM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E968' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3140-COUNT-ITEM.
           IF PRLM-ITEM-CNT < 9999999
               ADD 1 TO PRLM-ITEM-CNT
           END-IF
           IF PRLM-ITEM-CNT NOT < 3
               MOVE 'Y' TO PRLM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FED TAX GRID TIER FOR THE STATE TAX AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3150-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-FED-TAX-GRID-RATE(1) TO PRLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF PRLM-STATE-TAX-AMT NOT < WS-FED-TAX-GRID-LOW(WS-TX)
                   MOVE WS-FED-TAX-GRID-RATE(WS-TX) TO PRLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PRLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PRLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PAY FREQ CD:
      * JT -> XM WHEN NOTICE DATE IS PAST
      * XM -> ZT WHEN NOTICE DATE IS PAST
      * ZT -> JT WHEN NOTICE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3160-SET-PAY-FREQ.
           EVALUATE PRLM-PAY-FREQ-CD
               WHEN 'JT'
                   IF PRLM-NOTICE-DATE > 0 AND PRLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XM' TO PRLM-PAY-FREQ-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XM'
                   IF PRLM-NOTICE-DATE > 0 AND PRLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZT' TO PRLM-PAY-FREQ-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZT'
                   IF PRLM-NOTICE-DATE > 0 AND PRLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JT' TO PRLM-PAY-FREQ-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FED TAX AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3170-FLAG-FED-TAX.
           IF PRLM-FED-TAX-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PRLM-FED-TAX-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PAY FREQ CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3180-DESCRIBE-PAY-FREQ.
           EVALUATE PRLM-PAY-FREQ-CD
               WHEN 'JT'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'XM'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'ZT'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * NY -> PL WHEN EXPIRY DATE IS PAST
      * PL -> VK WHEN EXPIRY DATE IS PAST
      * VK -> WB WHEN EXPIRY DATE IS PAST
      * WB -> XL WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3190-SET-SOURCE.
           EVALUATE PRLM-SOURCE-CD
               WHEN 'NY'
                   IF PRLM-EXPIRY-DATE > 0 AND PRLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PL' TO PRLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PL'
                   IF PRLM-EXPIRY-DATE > 0 AND PRLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VK' TO PRLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VK'
                   IF PRLM-EXPIRY-DATE > 0 AND PRLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WB' TO PRLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WB'
                   IF PRLM-EXPIRY-DATE > 0 AND PRLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XL' TO PRLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE DEDUCT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DEDUCT AMT.
      *----------------------------------------------------------------*
       3200-PRORATE-DEDUCT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PRLT-DEDUCT-AMT ROUNDED =
                   PRLM-DEDUCT-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PRLT-DEDUCT-AMT
           END-IF
           ADD PRLT-DEDUCT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE FED TAX GRID TIER FOR THE GROSS AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3210-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-FED-TAX-GRID-RATE(1) TO PRLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF PRLM-GROSS-AMT NOT < WS-FED-TAX-GRID-LOW(WS-TX)
                   MOVE WS-FED-TAX-GRID-RATE(WS-TX) TO PRLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PRLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PRLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FED TAX YTD AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3220-ACCRUE-FED-TAX.
           IF PRLM-FED-TAX-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PRLM-FED-TAX-YTD * PRLM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO PRLM-GROSS-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PAY FREQ CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3230-DESCRIBE-PAY-FREQ.
           EVALUATE PRLM-PAY-FREQ-CD
               WHEN 'JT'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'XM'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ZT'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DEDUCT AMT IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3240-LIMIT-DEDUCT.
           IF PRLM-DEDUCT-AMT < 25.00
               MOVE 25.00 TO PRLM-DEDUCT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PRLM-DEDUCT-AMT > 5000.00
               MOVE 5000.00 TO PRLM-DEDUCT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NET YTD IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3250-LIMIT-NET.
           IF PRLM-NET-YTD < 100.00
               MOVE 100.00 TO PRLM-NET-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PRLM-NET-YTD > 500.00
               MOVE 500.00 TO PRLM-NET-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FICA AMT IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3260-LIMIT-FICA.
           IF PRLM-FICA-AMT < 0.00
               MOVE 0.00 TO PRLM-FICA-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PRLM-FICA-AMT > 5000.00
               MOVE 5000.00 TO PRLM-FICA-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3270-DESCRIBE-REASON.
           EVALUATE PRLM-REASON-CD
               WHEN 'B'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'C'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'G'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'H'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'L'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'T'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'X'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3280-COUNT-SEQ.
           IF PRLT-SEQ < 9999999
               ADD 1 TO PRLT-SEQ
           END-IF
           IF PRLT-SEQ NOT < 6
               MOVE 'Y' TO PRLM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * PR -> AD WHEN NEXT DUE DATE IS PAST
      * AD -> VD WHEN NEXT DUE DATE IS PAST
      * VD -> TX WHEN NEXT DUE DATE IS PAST
      * TX -> DP WHEN NEXT DUE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3290-SET-TRAN.
           EVALUATE PRLT-TRAN-CD
               WHEN 'PR'
                   IF PRLM-NEXT-DUE-DATE > 0 AND PRLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AD' TO PRLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AD'
                   IF PRLM-NEXT-DUE-DATE > 0 AND PRLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VD' TO PRLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VD'
                   IF PRLM-NEXT-DUE-DATE > 0 AND PRLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TX' TO PRLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TX'
                   IF PRLM-NEXT-DUE-DATE > 0 AND PRLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DP' TO PRLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT DEDUCT YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3300-SPLIT-DEDUCT.
           COMPUTE WS-WORK-AMT = PRLM-DEDUCT-YTD / 2
           COMPUTE WS-WORK-AMT-2 = PRLM-DEDUCT-YTD - (WS-WORK-AMT * 2)
           COMPUTE PRLM-NET-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3310-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF PRLM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PRLM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO PRLM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 30 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3320-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF PRLM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PRLM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO PRLM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE NET YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO STATE TAX AMT.
      *----------------------------------------------------------------*
       3330-PRORATE-STATE-TAX.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PRLM-STATE-TAX-AMT ROUNDED =
                   PRLM-NET-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PRLM-STATE-TAX-AMT
           END-IF
           ADD PRLM-STATE-TAX-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FICA AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3340-ACCRUE-FICA.
           IF PRLT-FICA-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PRLT-FICA-AMT * PRLM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E727 MEMO REQUIRED
      * E538 NET AMT NEGATIVE
      * E049 NET AMT OVER 250000
      * E037 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3350-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PRLT-MEMO = SPACES
                   MOVE 'E727' TO WS-REJECT-CODE
               WHEN PRLT-NET-AMT < 0
                   MOVE 'E538' TO WS-REJECT-CODE
               WHEN PRLT-NET-AMT > 250000.00
                   MOVE 'E049' TO WS-REJECT-CODE
               WHEN PRLM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E037' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN OPEN DATE IS PAST
      * BAT -> MAL WHEN OPEN DATE IS PAST
      * MAL -> TEL WHEN OPEN DATE IS PAST
      * TEL -> ONL WHEN OPEN DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3360-SET-CHANNEL.
           EVALUATE PRLT-CHANNEL-CD
               WHEN 'BRN'
                   IF PRLM-OPEN-DATE > 0 AND PRLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO PRLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF PRLM-OPEN-DATE > 0 AND PRLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO PRLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF PRLM-OPEN-DATE > 0 AND PRLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO PRLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF PRLM-OPEN-DATE > 0 AND PRLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO PRLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E161 CUST ID REQUIRED
      * E534 FED TAX AMT NEGATIVE
      * E444 FED TAX AMT OVER 250000
      * E280 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3370-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PRLM-CUST-ID = SPACES
                   MOVE 'E161' TO WS-REJECT-CODE
               WHEN PRLM-FED-TAX-AMT < 0
                   MOVE 'E534' TO WS-REJECT-CODE
               WHEN PRLM-FED-TAX-AMT > 250000.00
                   MOVE 'E444' TO WS-REJECT-CODE
               WHEN PRLM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E280' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DEDUCT YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3380-ACCRUE-DEDUCT.
           IF PRLM-DEDUCT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PRLM-DEDUCT-YTD * PRLM-RATE / 100 / 360
               ADD WS-WORK-AMT TO PRLM-FED-TAX-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3390-COUNT-NOTICE.
           IF PRLM-NOTICE-CNT < 9999999
               ADD 1 TO PRLM-NOTICE-CNT
           END-IF
           IF PRLM-NOTICE-CNT NOT < 10
               MOVE 'Y' TO PRLM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE NET FEE ON THE DEDUCT YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3400-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN PRLM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO PRLT-NET-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PRLM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE PRLT-NET-AMT ROUNDED = PRLM-DEDUCT-YTD *
                       PRLM-RATE / 200
               WHEN PRLM-REASON-CD = 'B'
                   MOVE 15.00 TO PRLT-NET-AMT
               WHEN PRLM-REASON-CD = 'C'
                   MOVE 5.00 TO PRLT-NET-AMT
               WHEN OTHER
                   COMPUTE PRLT-NET-AMT ROUNDED = PRLM-DEDUCT-YTD *
                       PRLM-RATE / 100
           END-EVALUATE
           IF PRLT-NET-AMT > 0 AND PRLT-NET-AMT < 2.00
               MOVE 2.00 TO PRLT-NET-AMT
           END-IF
           IF PRLT-NET-AMT > 100.00
               MOVE 100.00 TO PRLT-NET-AMT
           END-IF
           ADD PRLT-NET-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE GROSS YTD BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3410-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PRLT-TRAN-CD
                   ADD PRLM-GROSS-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PRLT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PRLM-GROSS-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3420-COUNT-CYCLE.
           IF PRLM-CYCLE-CNT < 9999999
               ADD 1 TO PRLM-CYCLE-CNT
           END-IF
           IF PRLM-CYCLE-CNT NOT < 10
               MOVE 'Y' TO PRLM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STATE TAX AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3430-ACCRUE-STATE-TAX.
           IF PRLM-STATE-TAX-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PRLM-STATE-TAX-AMT * PRLM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO PRLM-FICA-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FICA YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3440-ACCRUE-FICA.
           IF PRLM-FICA-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PRLM-FICA-YTD * PRLM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO PRLM-NET-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FED TAX YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FICA AMT.
      *----------------------------------------------------------------*
       3450-PRORATE-FICA.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PRLT-FICA-AMT ROUNDED =
                   PRLM-FED-TAX-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PRLT-FICA-AMT
           END-IF
           ADD PRLT-FICA-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT DEDUCT AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3460-SPLIT-DEDUCT.
           COMPUTE WS-WORK-AMT = PRLT-DEDUCT-AMT / 2
           COMPUTE WS-WORK-AMT-2 = PRLT-DEDUCT-AMT - (WS-WORK-AMT * 2)
           COMPUTE PRLM-STATE-TAX-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3470-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF PRLM-EXPIRY-DATE > 0 AND PRLM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PRLM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PRLT-DEDUCT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PRLT-DEDUCT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD PRLT-DEDUCT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PRLT-DEDUCT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PRLT-DEDUCT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3480-DESCRIBE-STATUS.
           EVALUATE PRLM-STATUS-CD
               WHEN 'MV'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'RD'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'ST'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'TJ'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'UC'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3490-DESCRIBE-REASON.
           EVALUATE PRLM-REASON-CD
               WHEN 'B'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'C'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'G'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'H'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'L'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'T'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'X'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE NET YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO GROSS AMT.
      *----------------------------------------------------------------*
       3500-PRORATE-GROSS.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PRLM-GROSS-AMT ROUNDED =
                   PRLM-NET-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PRLM-GROSS-AMT
           END-IF
           ADD PRLM-GROSS-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT STATE TAX YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3510-SPLIT-STATE-TAX.
           COMPUTE WS-WORK-AMT = PRLM-STATE-TAX-YTD / 4
           COMPUTE WS-WORK-AMT-2 = PRLM-STATE-TAX-YTD - (WS-WORK-AMT *
               4)
           COMPUTE PRLT-STATE-TAX-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FICA AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3520-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PRLM-REASON-CD
                   ADD PRLT-FICA-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PRLM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PRLT-FICA-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3530-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF PRLT-POST-DATE > 0 AND PRLT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PRLT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PRLM-GROSS-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PRLM-GROSS-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD PRLM-GROSS-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD PRLM-GROSS-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PRLM-GROSS-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3540-COUNT-NOTICE.
           IF PRLM-NOTICE-CNT < 9999999
               ADD 1 TO PRLM-NOTICE-CNT
           END-IF
           IF PRLM-NOTICE-CNT NOT < 6
               MOVE 'Y' TO PRLM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FICA AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3550-FLAG-FICA.
           IF PRLT-FICA-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PRLT-FICA-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE NET GRID TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3560-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-NET-GRID-RATE(1) TO PRLM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF WS-WORK-AMT NOT < WS-NET-GRID-LOW(WS-TX)
                   MOVE WS-NET-GRID-RATE(WS-TX) TO PRLM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PRLM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PRLM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FICA AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3570-SPLIT-FICA.
           COMPUTE WS-WORK-AMT = PRLM-FICA-AMT / 2
           COMPUTE WS-WORK-AMT-2 = PRLM-FICA-AMT - (WS-WORK-AMT * 2)
           COMPUTE PRLT-STATE-TAX-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FICA AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3580-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PRLM-STATUS-CD
                   ADD PRLM-FICA-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PRLM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PRLM-FICA-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STATE TA SCHED TIER FOR THE STATE TAX AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3590-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-STATE-TA-SCHED-RATE(1) TO PRLM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF PRLM-STATE-TAX-AMT NOT < WS-STATE-TA-SCHED-LOW(WS-TX)
                   MOVE WS-STATE-TA-SCHED-RATE(WS-TX) TO PRLM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PRLM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PRLM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * PR -> AD WHEN EFFECTIVE DATE IS PAST
      * AD -> VD WHEN EFFECTIVE DATE IS PAST
      * VD -> TX WHEN EFFECTIVE DATE IS PAST
      * TX -> DP WHEN EFFECTIVE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3600-SET-TRAN.
           EVALUATE PRLT-TRAN-CD
               WHEN 'PR'
                   IF PRLM-EFFECTIVE-DATE > 0 AND PRLM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AD' TO PRLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AD'
                   IF PRLM-EFFECTIVE-DATE > 0 AND PRLM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VD' TO PRLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VD'
                   IF PRLM-EFFECTIVE-DATE > 0 AND PRLM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TX' TO PRLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TX'
                   IF PRLM-EFFECTIVE-DATE > 0 AND PRLM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DP' TO PRLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE FED TAX YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO DEDUCT AMT.
      *----------------------------------------------------------------*
       3610-PRORATE-DEDUCT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PRLT-DEDUCT-AMT ROUNDED =
                   PRLM-FED-TAX-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PRLT-DEDUCT-AMT
           END-IF
           ADD PRLT-DEDUCT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE STATE TA SCHED TIER FOR THE STATE TAX AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3620-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-STATE-TA-SCHED-RATE(1) TO PRLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF PRLM-STATE-TAX-AMT NOT < WS-STATE-TA-SCHED-LOW(WS-TX)
                   MOVE WS-STATE-TA-SCHED-RATE(WS-TX) TO PRLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PRLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PRLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3630-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF PRLM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PRLM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO PRLM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PAY FREQ CD:
      * JT -> XM WHEN NEXT REVIEW DATE IS PAST
      * XM -> ZT WHEN NEXT REVIEW DATE IS PAST
      * ZT -> JT WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3640-SET-PAY-FREQ.
           EVALUATE PRLM-PAY-FREQ-CD
               WHEN 'JT'
                   IF PRLM-NEXT-REVIEW-DATE > 0 AND
                       PRLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'XM' TO PRLM-PAY-FREQ-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XM'
                   IF PRLM-NEXT-REVIEW-DATE > 0 AND
                       PRLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'ZT' TO PRLM-PAY-FREQ-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZT'
                   IF PRLM-NEXT-REVIEW-DATE > 0 AND
                       PRLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'JT' TO PRLM-PAY-FREQ-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATE TAX AMT IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3650-LIMIT-STATE-TAX.
           IF PRLM-STATE-TAX-AMT < 100.00
               MOVE 100.00 TO PRLM-STATE-TAX-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PRLM-STATE-TAX-AMT > 500.00
               MOVE 500.00 TO PRLM-STATE-TAX-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE NET AMT BY PAY FREQ CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3660-ACCUM-PAY-FREQ.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PRLM-PAY-FREQ-CD
                   ADD PRLM-NET-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PRLM-PAY-FREQ-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PRLM-NET-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3670-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF PRLM-LAST-ACT-DATE > 0 AND PRLM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PRLM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PRLM-FICA-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PRLM-FICA-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD PRLM-FICA-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD PRLM-FICA-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PRLM-FICA-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * PR -> AD WHEN POST DATE IS PAST
      * AD -> VD WHEN POST DATE IS PAST
      * VD -> TX WHEN POST DATE IS PAST
      * TX -> DP WHEN POST DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3680-SET-TRAN.
           EVALUATE PRLT-TRAN-CD
               WHEN 'PR'
                   IF PRLT-POST-DATE > 0 AND PRLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AD' TO PRLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AD'
                   IF PRLT-POST-DATE > 0 AND PRLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VD' TO PRLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VD'
                   IF PRLT-POST-DATE > 0 AND PRLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TX' TO PRLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TX'
                   IF PRLT-POST-DATE > 0 AND PRLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DP' TO PRLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE FED TAX YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3690-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PRLM-SOURCE-CD
                   ADD PRLM-FED-TAX-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PRLM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PRLM-FED-TAX-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE NET FEE ON THE WORK AMT 2.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3700-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN PRLT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO PRLM-NET-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PRLM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE PRLM-NET-AMT ROUNDED = WS-WORK-AMT-2 *
                       PRLM-RATE / 200
               WHEN PRLM-REGION-CD = 'DP'
                   MOVE 10.00 TO PRLM-NET-AMT
               WHEN PRLM-REGION-CD = 'TU'
                   MOVE 5.00 TO PRLM-NET-AMT
               WHEN OTHER
                   COMPUTE PRLM-NET-AMT ROUNDED = WS-WORK-AMT-2 *
                       PRLM-RATE / 100
           END-EVALUATE
           IF PRLM-NET-AMT > 0 AND PRLM-NET-AMT < 1.00
               MOVE 1.00 TO PRLM-NET-AMT
           END-IF
           IF PRLM-NET-AMT > 250.00
               MOVE 250.00 TO PRLM-NET-AMT
           END-IF
           ADD PRLM-NET-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE FICA FEE ON THE DEDUCT AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3710-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN PRLM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO PRLM-FICA-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PRLM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE PRLM-FICA-AMT ROUNDED = PRLM-DEDUCT-AMT *
                       PRLM-FEE-PCT / 200
               WHEN PRLM-REASON-CD = 'B'
                   MOVE 15.00 TO PRLM-FICA-AMT
               WHEN PRLM-REASON-CD = 'C'
                   MOVE 12.00 TO PRLM-FICA-AMT
               WHEN OTHER
                   COMPUTE PRLM-FICA-AMT ROUNDED = PRLM-DEDUCT-AMT *
                       PRLM-FEE-PCT / 100
           END-EVALUATE
           IF PRLM-FICA-AMT > 0 AND PRLM-FICA-AMT < 5.00
               MOVE 5.00 TO PRLM-FICA-AMT
           END-IF
           IF PRLM-FICA-AMT > 35.00
               MOVE 35.00 TO PRLM-FICA-AMT
           END-IF
           ADD PRLM-FICA-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 365 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3720-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF PRLM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PRLM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO PRLM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 10 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3730-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF PRLM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PRLM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO PRLM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE STATE TAX AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3740-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PRLT-CHANNEL-CD
                   ADD PRLT-STATE-TAX-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PRLT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PRLT-STATE-TAX-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3750-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF PRLM-LAST-ACT-DATE > 0 AND PRLM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PRLM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PRLM-STATE-TAX-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PRLM-STATE-TAX-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD PRLM-STATE-TAX-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PRLM-STATE-TAX-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PRLM-STATE-TAX-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE NET YTD BY PAY FREQ CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3760-ACCUM-PAY-FREQ.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PRLM-PAY-FREQ-CD
                   ADD PRLM-NET-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PRLM-PAY-FREQ-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PRLM-NET-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATE TAX YTD IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3770-LIMIT-STATE-TAX.
           IF PRLM-STATE-TAX-YTD < 10.00
               MOVE 10.00 TO PRLM-STATE-TAX-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PRLM-STATE-TAX-YTD > 10000.00
               MOVE 10000.00 TO PRLM-STATE-TAX-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STATE TA SCHED TIER FOR THE FED TAX YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3780-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-STATE-TA-SCHED-RATE(1) TO PRLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF PRLM-FED-TAX-YTD NOT < WS-STATE-TA-SCHED-LOW(WS-TX)
                   MOVE WS-STATE-TA-SCHED-RATE(WS-TX) TO PRLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PRLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PRLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE GROSS AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3790-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PRLT-CHANNEL-CD
                   ADD PRLM-GROSS-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PRLT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PRLM-GROSS-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3800-COUNT-ITEM.
           IF PRLM-ITEM-CNT < 9999999
               ADD 1 TO PRLM-ITEM-CNT
           END-IF
           IF PRLM-ITEM-CNT NOT < 3
               MOVE 'Y' TO PRLM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3810-DESCRIBE-REASON.
           EVALUATE PRLM-REASON-CD
               WHEN 'B'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'C'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'G'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'H'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'L'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'T'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'X'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE FED TAX AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3820-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PRLM-REASON-CD
                   ADD PRLM-FED-TAX-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PRLM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PRLM-FED-TAX-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 365 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3830-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF PRLM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PRLM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO PRLM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TAX STATE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3840-DESCRIBE-TAX-STATE.
           EVALUATE PRLM-TAX-STATE-CD
               WHEN 'CAZ'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'LZU'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'TUF'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'WKE'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FICA AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3850-FLAG-FICA.
           IF PRLT-FICA-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PRLT-FICA-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE NET AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3860-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PRLM-STATUS-CD
                   ADD PRLT-NET-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PRLM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PRLT-NET-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STATE TAX AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3870-ACCRUE-STATE-TAX.
           IF PRLM-STATE-TAX-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PRLM-STATE-TAX-AMT * PRLM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO PRLM-DEDUCT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E769 OPERATOR REQUIRED
      * E575 FICA AMT NEGATIVE
      * E435 FICA AMT OVER 25000
      * E488 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3880-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PRLT-OPERATOR = SPACES
                   MOVE 'E769' TO WS-REJECT-CODE
               WHEN PRLM-FICA-AMT < 0
                   MOVE 'E575' TO WS-REJECT-CODE
               WHEN PRLM-FICA-AMT > 25000.00
                   MOVE 'E435' TO WS-REJECT-CODE
               WHEN PRLM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E488' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E471 REJECT CD REQUIRED
      * E527 NET AMT NEGATIVE
      * E082 NET AMT OVER 99999
      * E722 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3890-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PRLT-REJECT-CD = SPACES
                   MOVE 'E471' TO WS-REJECT-CODE
               WHEN PRLM-NET-AMT < 0
                   MOVE 'E527' TO WS-REJECT-CODE
               WHEN PRLM-NET-AMT > 99999.00
                   MOVE 'E082' TO WS-REJECT-CODE
               WHEN PRLM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E722' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FICA AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3900-FLAG-FICA.
           IF PRLM-FICA-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PRLM-FICA-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 45 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3910-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF PRLM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PRLM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO PRLM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE DEDUCT YTD BY PAY FREQ CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3920-ACCUM-PAY-FREQ.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PRLM-PAY-FREQ-CD
                   ADD PRLM-DEDUCT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PRLM-PAY-FREQ-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PRLM-DEDUCT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * NET AMT IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3930-LIMIT-NET.
           IF PRLM-NET-AMT < 25.00
               MOVE 25.00 TO PRLM-NET-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PRLM-NET-AMT > 50000.00
               MOVE 50000.00 TO PRLM-NET-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3940-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF PRLM-NEXT-DUE-DATE > 0 AND PRLM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PRLM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3950-FLAG-ITEM.
           IF WS-WORK-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT STATE TAX YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3960-SPLIT-STATE-TAX.
           COMPUTE WS-WORK-AMT = PRLM-STATE-TAX-YTD / 3
           COMPUTE WS-WORK-AMT-2 = PRLM-STATE-TAX-YTD - (WS-WORK-AMT *
               3)
           COMPUTE PRLM-DEDUCT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3970-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF PRLM-EFFECTIVE-DATE > 0 AND PRLM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PRLM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PRLM-STATE-TAX-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PRLM-STATE-TAX-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD PRLM-STATE-TAX-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD PRLM-STATE-TAX-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PRLM-STATE-TAX-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT FED TAX AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3980-SPLIT-FED-TAX.
           COMPUTE WS-WORK-AMT = PRLM-FED-TAX-AMT / 3
           COMPUTE WS-WORK-AMT-2 = PRLM-FED-TAX-AMT - (WS-WORK-AMT * 3)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON NET YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3990-ACCRUE-NET.
           IF PRLM-NET-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PRLM-NET-YTD * PRLM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DEDUCT AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4000-SPLIT-DEDUCT.
           COMPUTE WS-WORK-AMT = PRLT-DEDUCT-AMT / 4
           COMPUTE WS-WORK-AMT-2 = PRLT-DEDUCT-AMT - (WS-WORK-AMT * 4)
           COMPUTE PRLM-NET-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4010-COUNT-CYCLE.
           IF PRLM-CYCLE-CNT < 9999999
               ADD 1 TO PRLM-CYCLE-CNT
           END-IF
           IF PRLM-CYCLE-CNT NOT < 12
               MOVE 'Y' TO PRLT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4020-DESCRIBE-TRAN.
           EVALUATE PRLT-TRAN-CD
               WHEN 'PR'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'AD'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'VD'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'TX'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'DP'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE FED TAX AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4030-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PRLT-CHANNEL-CD
                   ADD PRLM-FED-TAX-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PRLT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PRLM-FED-TAX-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE NET GRID TIER FOR THE GROSS YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4040-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-NET-GRID-RATE(1) TO PRLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF PRLM-GROSS-YTD NOT < WS-NET-GRID-LOW(WS-TX)
                   MOVE WS-NET-GRID-RATE(WS-TX) TO PRLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PRLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PRLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NET AMT IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4050-LIMIT-NET.
           IF PRLM-NET-AMT < 25.00
               MOVE 25.00 TO PRLM-NET-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PRLM-NET-AMT > 10000.00
               MOVE 10000.00 TO PRLM-NET-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4060-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF PRLM-EFFECTIVE-DATE > 0 AND PRLM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PRLM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PRLT-FICA-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD PRLT-FICA-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD PRLT-FICA-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD PRLT-FICA-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PRLT-FICA-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       4070-COUNT-PAYMENT.
           IF PRLM-PAYMENT-CNT < 9999999
               ADD 1 TO PRLM-PAYMENT-CNT
           END-IF
           IF PRLM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO PRLM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * GROSS YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4080-FLAG-GROSS.
           IF PRLM-GROSS-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PRLM-GROSS-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATE TAX YTD IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4090-LIMIT-STATE-TAX.
           IF PRLM-STATE-TAX-YTD < 25.00
               MOVE 25.00 TO PRLM-STATE-TAX-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PRLM-STATE-TAX-YTD > 500.00
               MOVE 500.00 TO PRLM-STATE-TAX-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FED TAX GRID TIER FOR THE NET YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4100-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-FED-TAX-GRID-RATE(1) TO PRLM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF PRLM-NET-YTD NOT < WS-FED-TAX-GRID-LOW(WS-TX)
                   MOVE WS-FED-TAX-GRID-RATE(WS-TX) TO PRLM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PRLM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PRLM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4110-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF PRLM-NEXT-REVIEW-DATE > 0 AND PRLM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PRLM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PRLM-GROSS-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD PRLM-GROSS-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD PRLM-GROSS-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD PRLM-GROSS-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PRLM-GROSS-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E435 ADDR REQUIRED
      * E914 STATE TAX YTD NEGATIVE
      * E703 STATE TAX YTD OVER 99999
      * E189 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       4120-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PRLM-ADDR = SPACES
                   MOVE 'E435' TO WS-REJECT-CODE
               WHEN PRLM-STATE-TAX-YTD < 0
                   MOVE 'E914' TO WS-REJECT-CODE
               WHEN PRLM-STATE-TAX-YTD > 99999.00
                   MOVE 'E703' TO WS-REJECT-CODE
               WHEN PRLM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E189' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * NET AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4130-FLAG-NET.
           IF PRLT-NET-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PRLT-NET-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 15 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4140-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF PRLT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PRLT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO PRLM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 15 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4150-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF PRLM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PRLM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO PRLT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DEDUCT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO STATE TAX YTD.
      *----------------------------------------------------------------*
       4160-PRORATE-STATE-TAX.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PRLM-STATE-TAX-YTD ROUNDED =
                   PRLT-DEDUCT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PRLM-STATE-TAX-YTD
           END-IF
           ADD PRLM-STATE-TAX-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       4170-COUNT-PAYMENT.
           IF PRLM-PAYMENT-CNT < 9999999
               ADD 1 TO PRLM-PAYMENT-CNT
           END-IF
           IF PRLM-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO PRLM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       4180-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF PRLM-OPEN-DATE > 0 AND PRLM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PRLM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PRLT-NET-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PRLT-NET-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD PRLT-NET-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD PRLT-NET-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PRLT-NET-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       4190-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF PRLM-LAST-PAY-DATE > 0 AND PRLM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PRLM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PRLM-NET-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PRLM-NET-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD PRLM-NET-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD PRLM-NET-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PRLM-NET-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 90 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4200-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF PRLM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PRLM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO PRLT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FICA YTD BY PAY FREQ CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4210-ACCUM-PAY-FREQ.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PRLM-PAY-FREQ-CD
                   ADD PRLM-FICA-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PRLM-PAY-FREQ-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PRLM-FICA-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       4220-COUNT-SEQ.
           IF PRLT-SEQ < 9999999
               ADD 1 TO PRLT-SEQ
           END-IF
           IF PRLT-SEQ NOT < 3
               MOVE 'Y' TO PRLM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DEDUCT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FED TAX YTD.
      *----------------------------------------------------------------*
       4230-PRORATE-FED-TAX.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PRLM-FED-TAX-YTD ROUNDED =
                   PRLM-DEDUCT-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PRLM-FED-TAX-YTD
           END-IF
           ADD PRLM-FED-TAX-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4240-ACCRUE-MARGIN.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * PRLM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO PRLM-GROSS-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FICA YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4250-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PRLM-REASON-CD
                   ADD PRLM-FICA-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PRLM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PRLM-FICA-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DEDUCT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO NET YTD.
      *----------------------------------------------------------------*
       4260-PRORATE-NET.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PRLM-NET-YTD ROUNDED =
                   PRLM-DEDUCT-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PRLM-NET-YTD
           END-IF
           ADD PRLM-NET-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE FICA FEE ON THE FICA AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4270-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN PRLM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO PRLT-FICA-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PRLM-PREFERRED-FLAG = 'Y'
                   COMPUTE PRLT-FICA-AMT ROUNDED = PRLM-FICA-AMT *
                       PRLM-RATE / 200
               WHEN PRLM-REASON-CD = 'B'
                   MOVE 12.00 TO PRLT-FICA-AMT
               WHEN PRLM-REASON-CD = 'C'
                   MOVE 10.00 TO PRLT-FICA-AMT
               WHEN OTHER
                   COMPUTE PRLT-FICA-AMT ROUNDED = PRLM-FICA-AMT *
                       PRLM-RATE / 100
           END-EVALUATE
           IF PRLT-FICA-AMT > 0 AND PRLT-FICA-AMT < 1.00
               MOVE 1.00 TO PRLT-FICA-AMT
           END-IF
           IF PRLT-FICA-AMT > 100.00
               MOVE 100.00 TO PRLT-FICA-AMT
           END-IF
           ADD PRLT-FICA-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE PAY FREQ CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4280-DESCRIBE-PAY-FREQ.
           EVALUATE PRLM-PAY-FREQ-CD
               WHEN 'JT'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'XM'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'ZT'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * MV -> RD WHEN CLOSE DATE IS PAST
      * RD -> ST WHEN CLOSE DATE IS PAST
      * ST -> TJ WHEN CLOSE DATE IS PAST
      * TJ -> UC WHEN CLOSE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4290-SET-STATUS.
           EVALUATE PRLM-STATUS-CD
               WHEN 'MV'
                   IF PRLM-CLOSE-DATE > 0 AND PRLM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RD' TO PRLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RD'
                   IF PRLM-CLOSE-DATE > 0 AND PRLM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ST' TO PRLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ST'
                   IF PRLM-CLOSE-DATE > 0 AND PRLM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TJ' TO PRLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TJ'
                   IF PRLM-CLOSE-DATE > 0 AND PRLM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UC' TO PRLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4300-COUNT-SEQ.
           IF PRLT-SEQ < 9999999
               ADD 1 TO PRLT-SEQ
           END-IF
           IF PRLT-SEQ NOT < 10
               MOVE 'Y' TO PRLT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE NET YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4310-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PRLM-REGION-CD
                   ADD PRLM-NET-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PRLM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PRLM-NET-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FICA AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO GROSS AMT.
      *----------------------------------------------------------------*
       4320-PRORATE-GROSS.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PRLM-GROSS-AMT ROUNDED =
                   PRLT-FICA-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PRLM-GROSS-AMT
           END-IF
           ADD PRLM-GROSS-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E297 REF NO REQUIRED
      * E188 GROSS AMT NEGATIVE
      * E763 GROSS AMT OVER 5000
      * E125 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       4330-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PRLM-REF-NO = SPACES
                   MOVE 'E297' TO WS-REJECT-CODE
               WHEN PRLM-GROSS-AMT < 0
                   MOVE 'E188' TO WS-REJECT-CODE
               WHEN PRLM-GROSS-AMT > 5000.00
                   MOVE 'E763' TO WS-REJECT-CODE
               WHEN PRLM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E125' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE GROSS AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       4340-PRORATE-GROSS.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   PRLM-GROSS-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 10 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4350-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF PRLM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PRLM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO PRLM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE GROSS FEE ON THE DEDUCT AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4360-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN PRLM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO PRLM-GROSS-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PRLM-STOP-FLAG = 'Y'
                   COMPUTE PRLM-GROSS-AMT ROUNDED = PRLM-DEDUCT-AMT *
                       PRLM-FEE-PCT / 200
               WHEN PRLT-CHANNEL-CD = 'BRN'
                   MOVE 12.00 TO PRLM-GROSS-AMT
               WHEN PRLT-CHANNEL-CD = 'BAT'
                   MOVE 12.00 TO PRLM-GROSS-AMT
               WHEN OTHER
                   COMPUTE PRLM-GROSS-AMT ROUNDED = PRLM-DEDUCT-AMT *
                       PRLM-FEE-PCT / 100
           END-EVALUATE
           IF PRLM-GROSS-AMT > 0 AND PRLM-GROSS-AMT < 1.00
               MOVE 1.00 TO PRLM-GROSS-AMT
           END-IF
           IF PRLM-GROSS-AMT > 100.00
               MOVE 100.00 TO PRLM-GROSS-AMT
           END-IF
           ADD PRLM-GROSS-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4370-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF PRLM-LAST-ACT-DATE > 0 AND PRLM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PRLM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PRLM-FICA-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD PRLM-FICA-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD PRLM-FICA-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD PRLM-FICA-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PRLM-FICA-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TAX STATE CD:
      * CAZ -> LZU WHEN EXPIRY DATE IS PAST
      * LZU -> TUF WHEN EXPIRY DATE IS PAST
      * TUF -> WKE WHEN EXPIRY DATE IS PAST
      * WKE -> CAZ WHEN EXPIRY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4380-SET-TAX-STATE.
           EVALUATE PRLM-TAX-STATE-CD
               WHEN 'CAZ'
                   IF PRLM-EXPIRY-DATE > 0 AND PRLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LZU' TO PRLM-TAX-STATE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LZU'
                   IF PRLM-EXPIRY-DATE > 0 AND PRLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TUF' TO PRLM-TAX-STATE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TUF'
                   IF PRLM-EXPIRY-DATE > 0 AND PRLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WKE' TO PRLM-TAX-STATE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WKE'
                   IF PRLM-EXPIRY-DATE > 0 AND PRLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CAZ' TO PRLM-TAX-STATE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4390-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF PRLM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PRLM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO PRLM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4400-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF PRLM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PRLM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO PRLM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       4410-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF PRLM-NOTICE-DATE > 0 AND PRLM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PRLM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PRLM-NET-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD PRLM-NET-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD PRLM-NET-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD PRLM-NET-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PRLM-NET-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * MV -> RD WHEN NOTICE DATE IS PAST
      * RD -> ST WHEN NOTICE DATE IS PAST
      * ST -> TJ WHEN NOTICE DATE IS PAST
      * TJ -> UC WHEN NOTICE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4420-SET-STATUS.
           EVALUATE PRLM-STATUS-CD
               WHEN 'MV'
                   IF PRLM-NOTICE-DATE > 0 AND PRLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RD' TO PRLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RD'
                   IF PRLM-NOTICE-DATE > 0 AND PRLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ST' TO PRLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ST'
                   IF PRLM-NOTICE-DATE > 0 AND PRLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TJ' TO PRLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TJ'
                   IF PRLM-NOTICE-DATE > 0 AND PRLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UC' TO PRLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * GROSS YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4430-FLAG-GROSS.
           IF PRLM-GROSS-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PRLM-GROSS-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * MV -> RD WHEN POST DATE IS PAST
      * RD -> ST WHEN POST DATE IS PAST
      * ST -> TJ WHEN POST DATE IS PAST
      * TJ -> UC WHEN POST DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4440-SET-STATUS.
           EVALUATE PRLM-STATUS-CD
               WHEN 'MV'
                   IF PRLT-POST-DATE > 0 AND PRLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RD' TO PRLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RD'
                   IF PRLT-POST-DATE > 0 AND PRLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ST' TO PRLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ST'
                   IF PRLT-POST-DATE > 0 AND PRLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TJ' TO PRLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TJ'
                   IF PRLT-POST-DATE > 0 AND PRLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UC' TO PRLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT FICA YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4450-SPLIT-FICA.
           COMPUTE WS-WORK-AMT = PRLM-FICA-YTD / 4
           COMPUTE WS-WORK-AMT-2 = PRLM-FICA-YTD - (WS-WORK-AMT * 4)
           COMPUTE PRLM-GROSS-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE GROSS YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO STATE TAX AMT.
      *----------------------------------------------------------------*
       4460-PRORATE-STATE-TAX.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PRLT-STATE-TAX-AMT ROUNDED =
                   PRLM-GROSS-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PRLT-STATE-TAX-AMT
           END-IF
           ADD PRLT-STATE-TAX-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FICA YTD IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4470-LIMIT-FICA.
           IF PRLM-FICA-YTD < 10.00
               MOVE 10.00 TO PRLM-FICA-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PRLM-FICA-YTD > 50000.00
               MOVE 50000.00 TO PRLM-FICA-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FED TAX FEE ON THE GROSS AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4480-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN PRLM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO PRLM-FED-TAX-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PRLM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE PRLM-FED-TAX-AMT ROUNDED = PRLM-GROSS-AMT *
                       PRLM-RATE / 200
               WHEN PRLM-REGION-CD = 'DP'
                   MOVE 7.00 TO PRLM-FED-TAX-AMT
               WHEN PRLM-REGION-CD = 'TU'
                   MOVE 7.00 TO PRLM-FED-TAX-AMT
               WHEN OTHER
                   COMPUTE PRLM-FED-TAX-AMT ROUNDED = PRLM-GROSS-AMT *
                       PRLM-RATE / 100
           END-EVALUATE
           IF PRLM-FED-TAX-AMT > 0 AND PRLM-FED-TAX-AMT < 5.00
               MOVE 5.00 TO PRLM-FED-TAX-AMT
           END-IF
           IF PRLM-FED-TAX-AMT > 100.00
               MOVE 100.00 TO PRLM-FED-TAX-AMT
           END-IF
           ADD PRLM-FED-TAX-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE GROSS FEE ON THE FICA AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4490-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN PRLM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO PRLM-GROSS-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PRLM-HOLD-FLAG = 'Y'
                   COMPUTE PRLM-GROSS-AMT ROUNDED = PRLT-FICA-AMT *
                       PRLM-RATE / 200
               WHEN PRLT-CHANNEL-CD = 'BRN'
                   MOVE 10.00 TO PRLM-GROSS-AMT
               WHEN PRLT-CHANNEL-CD = 'BAT'
                   MOVE 20.00 TO PRLM-GROSS-AMT
               WHEN OTHER
                   COMPUTE PRLM-GROSS-AMT ROUNDED = PRLT-FICA-AMT *
                       PRLM-RATE / 100
           END-EVALUATE
           IF PRLM-GROSS-AMT > 0 AND PRLM-GROSS-AMT < 2.00
               MOVE 2.00 TO PRLM-GROSS-AMT
           END-IF
           IF PRLM-GROSS-AMT > 75.00
               MOVE 75.00 TO PRLM-GROSS-AMT
           END-IF
           ADD PRLM-GROSS-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E855 MEMO REQUIRED
      * E924 NET YTD NEGATIVE
      * E890 NET YTD OVER 250000
      * E814 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       4500-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PRLT-MEMO = SPACES
                   MOVE 'E855' TO WS-REJECT-CODE
               WHEN PRLM-NET-YTD < 0
                   MOVE 'E924' TO WS-REJECT-CODE
               WHEN PRLM-NET-YTD > 250000.00
                   MOVE 'E890' TO WS-REJECT-CODE
               WHEN PRLM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E814' TO WS-REJECT-CODE
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
       4510-SPLIT-STATE-TAX.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 3)
           COMPUTE PRLM-STATE-TAX-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DEDUCT YTD FEE ON THE STATE TAX YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4520-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN PRLM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO PRLM-DEDUCT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PRLT-REVERSAL-FLAG = 'Y'
                   COMPUTE PRLM-DEDUCT-YTD ROUNDED = PRLM-STATE-TAX-YTD
                       * PRLM-MARGIN / 200
               WHEN PRLM-TAX-STATE-CD = 'CAZ'
                   MOVE 3.00 TO PRLM-DEDUCT-YTD
               WHEN PRLM-TAX-STATE-CD = 'LZU'
                   MOVE 5.00 TO PRLM-DEDUCT-YTD
               WHEN OTHER
                   COMPUTE PRLM-DEDUCT-YTD ROUNDED = PRLM-STATE-TAX-YTD
                       * PRLM-MARGIN / 100
           END-EVALUATE
           IF PRLM-DEDUCT-YTD > 0 AND PRLM-DEDUCT-YTD < 5.00
               MOVE 5.00 TO PRLM-DEDUCT-YTD
           END-IF
           IF PRLM-DEDUCT-YTD > 25.00
               MOVE 25.00 TO PRLM-DEDUCT-YTD
           END-IF
           ADD PRLM-DEDUCT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FICA AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4530-FLAG-FICA.
           IF PRLT-FICA-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PRLT-FICA-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DEDUCT FEE ON THE STATE TAX AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4540-ASSESS-STOP.
           EVALUATE TRUE
               WHEN PRLM-STOP-FLAG = 'Y'
                   MOVE ZERO TO PRLM-DEDUCT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PRLM-PREFERRED-FLAG = 'Y'
                   COMPUTE PRLM-DEDUCT-AMT ROUNDED = PRLM-STATE-TAX-AMT
                       * PRLM-FEE-PCT / 200
               WHEN PRLM-REASON-CD = 'B'
                   MOVE 10.00 TO PRLM-DEDUCT-AMT
               WHEN PRLM-REASON-CD = 'C'
                   MOVE 5.00 TO PRLM-DEDUCT-AMT
               WHEN OTHER
                   COMPUTE PRLM-DEDUCT-AMT ROUNDED = PRLM-STATE-TAX-AMT
                       * PRLM-FEE-PCT / 100
           END-EVALUATE
           IF PRLM-DEDUCT-AMT > 0 AND PRLM-DEDUCT-AMT < 5.00
               MOVE 5.00 TO PRLM-DEDUCT-AMT
           END-IF
           IF PRLM-DEDUCT-AMT > 25.00
               MOVE 25.00 TO PRLM-DEDUCT-AMT
           END-IF
           ADD PRLM-DEDUCT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4550-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 25.00
               MOVE 25.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 50000.00
               MOVE 50000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DEDUCT FEE ON THE FICA AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4560-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN PRLT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO PRLT-DEDUCT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PRLM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE PRLT-DEDUCT-AMT ROUNDED = PRLM-FICA-AMT *
                       PRLM-RATE / 200
               WHEN PRLM-STATUS-CD = 'MV'
                   MOVE 12.00 TO PRLT-DEDUCT-AMT
               WHEN PRLM-STATUS-CD = 'RD'
                   MOVE 12.00 TO PRLT-DEDUCT-AMT
               WHEN OTHER
                   COMPUTE PRLT-DEDUCT-AMT ROUNDED = PRLM-FICA-AMT *
                       PRLM-RATE / 100
           END-EVALUATE
           IF PRLT-DEDUCT-AMT > 0 AND PRLT-DEDUCT-AMT < 2.00
               MOVE 2.00 TO PRLT-DEDUCT-AMT
           END-IF
           IF PRLT-DEDUCT-AMT > 35.00
               MOVE 35.00 TO PRLT-DEDUCT-AMT
           END-IF
           ADD PRLT-DEDUCT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4570-DESCRIBE-REGION.
           EVALUATE PRLM-REGION-CD
               WHEN 'DP'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'TU'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'WD'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FED TAX YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4580-ACCRUE-FED-TAX.
           IF PRLM-FED-TAX-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PRLM-FED-TAX-YTD * PRLM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO PRLT-FICA-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E672 NAME REQUIRED
      * E308 WORK AMT NEGATIVE
      * E408 WORK AMT OVER 99999
      * E057 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       4590-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PRLM-NAME = SPACES
                   MOVE 'E672' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E308' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 99999.00
                   MOVE 'E408' TO WS-REJECT-CODE
               WHEN PRLM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E057' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE PRL FIELDS TO PYFEE AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7010-CALL-PYFEE.
           INITIALIZE PYFEE-PARMS
           MOVE PRLM-GROSS-YTD
             TO PYFEE-GROSS-YTD
           MOVE PRLM-DEDUCT-AMT
             TO PYFEE-DEDUCT-AMT
           MOVE PRLM-STATE-TAX-AMT
             TO PYFEE-STATE-TAX-AMT
           MOVE PRLM-NET-AMT
             TO PYFEE-NET-AMT
           MOVE PRLM-STATE-TAX-YTD
             TO PYFEE-STATE-TAX-YTD
           MOVE PRLM-DEDUCT-YTD
             TO PYFEE-DEDUCT-YTD
           MOVE PRLM-NOTICE-DATE
             TO PYFEE-NOTICE-DATE
           MOVE PRLM-LAST-ACT-DATE
             TO PYFEE-LAST-ACT-DATE
           MOVE PRLM-NEXT-DUE-DATE
             TO PYFEE-NEXT-DUE-DATE
           MOVE PRLM-CLOSE-DATE
             TO PYFEE-CLOSE-DATE
           MOVE PRLM-REGION-CD
             TO PYFEE-REGION-CD
           MOVE PRLM-PAY-FREQ-CD
             TO PYFEE-PAY-FREQ-CD
           MOVE PRLM-SOURCE-CD
             TO PYFEE-SOURCE-CD
           MOVE PRLM-TAX-STATE-CD
             TO PYFEE-TAX-STATE-CD
           MOVE PRLM-STOP-FLAG
             TO PYFEE-STOP-FLAG
           MOVE PRLM-AUTO-RENEW-FLAG
             TO PYFEE-AUTO-RENEW-FLAG
           MOVE PRLM-HOLD-FLAG
             TO PYFEE-HOLD-FLAG
           MOVE PRLM-CYCLE-CNT
             TO PYFEE-CYCLE-CNT
           MOVE PRLM-NOTICE-CNT
             TO PYFEE-NOTICE-CNT
           MOVE PRLM-MARGIN
             TO PYFEE-MARGIN
           MOVE PRLM-RATE
             TO PYFEE-RATE
           MOVE PRLM-CONTACT
             TO PYFEE-CONTACT
           MOVE PRLM-NAME
             TO PYFEE-NAME
           CALL 'PYFEE' USING PYFEE-PARMS
           EVALUATE PYFEE-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE PYFEE-GROSS-YTD
                     TO PRLM-GROSS-YTD
                   MOVE PYFEE-STATE-TAX-AMT
                     TO PRLM-STATE-TAX-AMT
                   CONTINUE
               WHEN 08
                   MOVE 'RFEE' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-PYFEE' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE PRL FIELDS TO PYPRC AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7000-CALL-PYPRC.
           INITIALIZE PYPRC-PARMS
           MOVE PRLM-STATE-TAX-YTD
             TO PYPRC-STATE-TAX-YTD
           MOVE PRLM-FED-TAX-AMT
             TO PYPRC-FED-TAX-AMT
           MOVE PRLM-NET-AMT
             TO PYPRC-NET-AMT
           MOVE PRLM-STATE-TAX-AMT
             TO PYPRC-STATE-TAX-AMT
           MOVE PRLM-FICA-YTD
             TO PYPRC-FICA-YTD
           MOVE PRLM-FED-TAX-YTD
             TO PYPRC-FED-TAX-YTD
           MOVE PRLM-NEXT-DUE-DATE
             TO PYPRC-NEXT-DUE-DATE
           MOVE PRLM-LAST-PAY-DATE
             TO PYPRC-LAST-PAY-DATE
           MOVE PRLM-OPEN-DATE
             TO PYPRC-OPEN-DATE
           MOVE PRLM-EXPIRY-DATE
             TO PYPRC-EXPIRY-DATE
           MOVE PRLM-TAX-STATE-CD
             TO PYPRC-TAX-STATE-CD
           MOVE PRLM-STATUS-CD
             TO PYPRC-STATUS-CD
           MOVE PRLM-SOURCE-CD
             TO PYPRC-SOURCE-CD
           MOVE PRLM-REGION-CD
             TO PYPRC-REGION-CD
           MOVE PRLM-STOP-FLAG
             TO PYPRC-STOP-FLAG
           MOVE PRLM-AUTO-RENEW-FLAG
             TO PYPRC-AUTO-RENEW-FLAG
           MOVE PRLM-PREFERRED-FLAG
             TO PYPRC-PREFERRED-FLAG
           MOVE PRLM-NOTICE-CNT
             TO PYPRC-NOTICE-CNT
           MOVE PRLM-ITEM-CNT
             TO PYPRC-ITEM-CNT
           MOVE PRLM-FEE-PCT
             TO PYPRC-FEE-PCT
           MOVE PRLM-MARGIN
             TO PYPRC-MARGIN
           MOVE PRLM-CUST-ID
             TO PYPRC-CUST-ID
           MOVE PRLM-CONTACT
             TO PYPRC-CONTACT
           CALL 'PYPRC' USING PYPRC-PARMS
           EVALUATE PYPRC-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE PYPRC-NET-AMT
                     TO PRLM-NET-AMT
                   MOVE PYPRC-FED-TAX-YTD
                     TO PRLM-FED-TAX-YTD
                   MOVE PYPRC-PREFERRED-FLAG
                     TO PRLM-PREFERRED-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RPRC' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-PYPRC' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE PRL FIELDS TO PYPEN AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7040-CALL-PYPEN.
           INITIALIZE PYPEN-PARMS
           MOVE PRLM-NET-AMT
             TO PYPEN-NET-AMT
           MOVE PRLM-FED-TAX-YTD
             TO PYPEN-FED-TAX-YTD
           MOVE PRLM-GROSS-AMT
             TO PYPEN-GROSS-AMT
           MOVE PRLM-DEDUCT-YTD
             TO PYPEN-DEDUCT-YTD
           MOVE PRLM-FICA-YTD
             TO PYPEN-FICA-YTD
           MOVE PRLM-STATE-TAX-YTD
             TO PYPEN-STATE-TAX-YTD
           MOVE PRLM-EFFECTIVE-DATE
             TO PYPEN-EFFECTIVE-DATE
           MOVE PRLM-CLOSE-DATE
             TO PYPEN-CLOSE-DATE
           MOVE PRLM-NEXT-DUE-DATE
             TO PYPEN-NEXT-DUE-DATE
           MOVE PRLM-EXPIRY-DATE
             TO PYPEN-EXPIRY-DATE
           MOVE PRLM-SOURCE-CD
             TO PYPEN-SOURCE-CD
           MOVE PRLM-REASON-CD
             TO PYPEN-REASON-CD
           MOVE PRLM-PAY-FREQ-CD
             TO PYPEN-PAY-FREQ-CD
           MOVE PRLM-TAX-STATE-CD
             TO PYPEN-TAX-STATE-CD
           MOVE PRLM-PREFERRED-FLAG
             TO PYPEN-PREFERRED-FLAG
           MOVE PRLM-SUSPEND-FLAG
             TO PYPEN-SUSPEND-FLAG
           MOVE PRLM-STOP-FLAG
             TO PYPEN-STOP-FLAG
           MOVE PRLM-PAYMENT-CNT
             TO PYPEN-PAYMENT-CNT
           MOVE PRLM-CYCLE-CNT
             TO PYPEN-CYCLE-CNT
           MOVE PRLM-MARGIN
             TO PYPEN-MARGIN
           MOVE PRLM-RATE
             TO PYPEN-RATE
           MOVE PRLM-CUST-ID
             TO PYPEN-CUST-ID
           MOVE PRLM-REF-NO
             TO PYPEN-REF-NO
           CALL 'PYPEN' USING PYPEN-PARMS
           EVALUATE PYPEN-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE PYPEN-NET-AMT
                     TO PRLM-NET-AMT
                   MOVE PYPEN-FED-TAX-YTD
                     TO PRLM-FED-TAX-YTD
                   MOVE PYPEN-GROSS-AMT
                     TO PRLM-GROSS-AMT
                   MOVE PYPEN-DEDUCT-YTD
                     TO PRLM-DEDUCT-YTD
                   MOVE PYPEN-STATE-TAX-YTD
                     TO PRLM-STATE-TAX-YTD
                   CONTINUE
               WHEN 08
                   MOVE 'RPEN' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7040-CALL-PYPEN' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE PRLT-KEY TO RD-KEY
           MOVE PRLT-TRAN-CD TO RD-CODE
           MOVE PRLT-DEDUCT-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE PRLT-KEY TO RD-KEY
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
           DISPLAY 'PYLACCR READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'PYLACCR' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
