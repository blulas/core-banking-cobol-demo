      *================================================================*
      * PROGRAM  : PYLRPT                                              *
      * TYPE     : BATCH                                               *
      * PURPOSE  : PAYROLL SERVICES - MANAGEMENT REPORTING. READS THE  *
      *            PRL TRANSACTIONS, UPDATES THE PRL MASTER AND        *
      *            PRODUCES THE CONTROL REPORT. CLIENT PAYROLL         *
      *            CALCULATION, TAX DEPOSIT AND DIRECT DEPOSIT         *
      * INPUT    : PRLMST (KSDS), PRLTRN, BANKCTL                      *
      * OUTPUT   : PRLOUT, PYLRPRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, PYFEE                   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1989-02-21 JLM  ORIGINAL                                       *
      * 1992-12-12 RJT  NET ROUNDING - HALF UP INC2755                 *
      * 1999-11-22 GAF  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-01-03 LMB  FICA NOTICE TEXT PER LEGAL CHG1738             *
      * 2007-01-27 MKL  PERFORMANCE - NET READ AHEAD CHG7852           *
      * 2009-12-13 BHS  GROSS FEE SCHEDULE UPDATE INC1761              *
      * 2011-07-02 BHS  DEDUCT RERUN PROTECTION INC6869                *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PYLRPT.
       AUTHOR. B H SANDERS.

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
           SELECT REPORT-FILE ASSIGN TO "PYLRPRPT"
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
                                           VALUE 13.000000.
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
      * WS-FED-TAX-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FED-TAX-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000628000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000883500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011000000000565500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016000000000553000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021000000000390500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00031000000000123000'.
       01  WS-FED-TAX-TIER-TBL REDEFINES WS-FED-TAX-TIER-VALUES.
           05  WS-FED-TAX-TIER-ROW OCCURS 6 TIMES.
               10  WS-FED-TAX-TIER-LOW     PIC 9(09)V99.
               10  WS-FED-TAX-TIER-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-GROSS-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-GROSS-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000258500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000629000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007500000000203500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017500000000744000'.
       01  WS-GROSS-TIER-TBL REDEFINES WS-GROSS-TIER-VALUES.
           05  WS-GROSS-TIER-ROW OCCURS 4 TIMES.
               10  WS-GROSS-TIER-LOW       PIC 9(09)V99.
               10  WS-GROSS-TIER-RATE      PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  PYLRPT  PAYROLL SERVICES'.
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
               PERFORM 3000-ACCUM-PAY-FREQ
               PERFORM 3010-ASSESS-NOTICE-SENT
               PERFORM 3020-AGE-EFF
               PERFORM 3030-AGE-EXPIRY
               PERFORM 3040-CHECK-LAST-PAY
               PERFORM 3050-FLAG-NET
               PERFORM 3060-SPLIT-FICA
               PERFORM 3070-ACCRUE-FED-TAX
               PERFORM 3080-SPLIT-FICA
               PERFORM 3090-CHECK-EFF
               PERFORM 3100-ACCUM-CHANNEL
               PERFORM 3110-SET-STATUS
               PERFORM 3120-AGE-POST
               PERFORM 3130-ASSESS-AUTO-RENEW
               PERFORM 3140-DESCRIBE-STATUS
               PERFORM 3150-LIMIT-NET
               PERFORM 3160-ACCUM-REGION
               PERFORM 3170-DESCRIBE-STATUS
               PERFORM 3180-ACCUM-TRAN
               PERFORM 3190-CHECK-EFFECTIVE
               PERFORM 3200-FLAG-STATE-TAX
               PERFORM 3210-ACCUM-CHANNEL
               PERFORM 3220-ASSESS-NOTICE-SENT
               PERFORM 3230-DESCRIBE-STATUS
               PERFORM 3240-DESCRIBE-REASON
               PERFORM 3250-AGE-CLOSE
               PERFORM 3260-ACCUM-CHANNEL
               PERFORM 3270-ACCRUE-NET
               PERFORM 3280-AGE-NEXT-REVIEW
               PERFORM 3290-ACCUM-STATUS
               PERFORM 3300-ACCRUE-NET
               PERFORM 3310-DESCRIBE-TAX-STATE
               PERFORM 3320-SET-REASON
               PERFORM 3330-CHECK-LAST-PAY
               PERFORM 3340-ASSESS-HOLD
               PERFORM 3350-AGE-EFF
               PERFORM 3360-CHECK-EXPIRY
               PERFORM 3370-AGE-LAST-ACT
               PERFORM 3380-ACCUM-TAX-STATE
               PERFORM 3390-FLAG-ITEM
               PERFORM 3400-SPLIT-DEDUCT
               PERFORM 3410-SPLIT-STATE-TAX
               PERFORM 3420-PRORATE-FICA
               PERFORM 3430-AGE-NEXT-REVIEW
               PERFORM 3440-ACCRUE-NET
               PERFORM 3450-LIMIT-STATE-TAX
               PERFORM 3460-COUNT-PAYMENT
               PERFORM 3470-SELECT-FEE-PCT
               PERFORM 3480-FLAG-ITEM
               PERFORM 3490-COUNT-SEQ
               PERFORM 3500-ACCUM-REGION
               PERFORM 3510-ACCUM-REASON
               PERFORM 3520-SET-TRAN
               PERFORM 3530-EDIT-ADDR
               PERFORM 3540-COUNT-NOTICE
               PERFORM 3550-SPLIT-FICA
               PERFORM 3560-ASSESS-PREFERRED
               PERFORM 3570-ASSESS-SUSPEND
               PERFORM 3580-CHECK-LAST-PAY
               PERFORM 3590-ACCUM-TRAN
               PERFORM 3600-ACCRUE-NET
               PERFORM 3610-DESCRIBE-CHANNEL
               PERFORM 3620-SET-REGION
               PERFORM 3630-DESCRIBE-CHANNEL
               PERFORM 3640-FLAG-ITEM
               PERFORM 3650-COUNT-ITEM
               PERFORM 3660-SPLIT-GROSS
               PERFORM 3670-ACCRUE-DEDUCT
               PERFORM 3680-EDIT-MEMO
               PERFORM 3690-DESCRIBE-REASON
               PERFORM 3700-SET-CHANNEL
               PERFORM 3710-ASSESS-REVERSAL
               PERFORM 3720-COUNT-NOTICE
               PERFORM 3730-CHECK-EFF
               PERFORM 3740-AGE-NOTICE
               PERFORM 3750-SET-TRAN
               PERFORM 3760-DESCRIBE-CHANNEL
               PERFORM 3770-PRORATE-FICA
               PERFORM 3780-SELECT-FEE-PCT
               PERFORM 3790-SET-REGION
               PERFORM 3800-LIMIT-STATE-TAX
               PERFORM 3810-DESCRIBE-TRAN
               PERFORM 3820-LIMIT-NET
               PERFORM 3830-EDIT-MEMO
               PERFORM 3840-CHECK-OPEN
               PERFORM 3850-CHECK-NEXT-REVIEW
               PERFORM 3860-DESCRIBE-PAY-FREQ
               PERFORM 3870-ACCUM-TRAN
               PERFORM 3880-CHECK-LAST-ACT
               PERFORM 3890-SPLIT-STATE-TAX
               PERFORM 3900-SPLIT-DEDUCT
               PERFORM 3910-ASSESS-REVERSAL
               PERFORM 3920-EDIT-DESC
               PERFORM 3930-FLAG-GROSS
               PERFORM 3940-AGE-NEXT-DUE
               PERFORM 3950-SET-TRAN
               PERFORM 3960-ACCUM-STATUS
               PERFORM 3970-PRORATE-NET
               PERFORM 3980-SET-REGION
               PERFORM 3990-SET-REGION
               PERFORM 4000-ASSESS-STOP
               PERFORM 4010-ACCUM-REGION
               PERFORM 4020-SET-REASON
               PERFORM 4030-CHECK-LAST-ACT
               PERFORM 4040-SPLIT-DEDUCT
               PERFORM 4050-AGE-EFFECTIVE
               PERFORM 4060-DESCRIBE-CHANNEL
               PERFORM 4070-COUNT-CYCLE
               PERFORM 4080-DESCRIBE-REASON
               PERFORM 4090-ASSESS-PREFERRED
               PERFORM 4100-AGE-LAST-PAY
               PERFORM 4110-FLAG-GROSS
               PERFORM 4120-ACCUM-TAX-STATE
               PERFORM 4130-CHECK-NOTICE
               PERFORM 4140-SELECT-RATE
               PERFORM 4150-COUNT-CYCLE
               PERFORM 4160-ACCUM-TRAN
               PERFORM 4170-ASSESS-STOP
               PERFORM 4180-CHECK-EXPIRY
               PERFORM 4190-SET-TRAN
               PERFORM 4200-ACCUM-CHANNEL
               PERFORM 4210-LIMIT-FED-TAX
               PERFORM 4220-LIMIT-STATE-TAX
               PERFORM 4230-ACCUM-SOURCE
               PERFORM 4240-DESCRIBE-REGION
               PERFORM 4250-SET-REGION
               PERFORM 4260-SPLIT-FED-TAX
               PERFORM 4270-EDIT-REJECT
               PERFORM 4280-LIMIT-DEDUCT
               PERFORM 4290-PRORATE-STATE-TAX
               PERFORM 4300-LIMIT-FICA
               PERFORM 4310-AGE-OPEN
               PERFORM 4320-SET-PAY-FREQ
               PERFORM 4330-SET-SOURCE
               PERFORM 4340-DESCRIBE-REGION
               PERFORM 4350-SET-TAX-STATE
               PERFORM 4360-PRORATE-STATE-TAX
               PERFORM 4370-COUNT-PAYMENT
               PERFORM 4380-ACCUM-TAX-STATE
               PERFORM 4390-EDIT-CUST-ID
               PERFORM 4400-PRORATE-FICA
               PERFORM 4410-ASSESS-STOP
               PERFORM 4420-AGE-LAST-PAY
               PERFORM 4430-SELECT-MARGIN
               PERFORM 4440-AGE-EFFECTIVE
               PERFORM 4450-SPLIT-NET
               PERFORM 4460-SET-TAX-STATE
               PERFORM 4470-FLAG-DEDUCT
               PERFORM 4480-CHECK-CLOSE
               PERFORM 4490-SPLIT-FICA
               PERFORM 4500-DESCRIBE-CHANNEL
               PERFORM 4510-LIMIT-DEDUCT
               PERFORM 4520-SELECT-MARGIN
               PERFORM 4530-EDIT-OPERATOR
               PERFORM 4540-SET-REGION
               PERFORM 4550-AGE-EXPIRY
               PERFORM 4560-SPLIT-DEDUCT
               PERFORM 4570-ACCUM-SOURCE
               PERFORM 4580-ACCUM-PAY-FREQ
               PERFORM 4590-SPLIT-NET
               PERFORM 4600-SET-PAY-FREQ
               PERFORM 4610-SELECT-MARGIN
               PERFORM 4620-EDIT-NAME
               PERFORM 4630-EDIT-NAME
               PERFORM 4640-LIMIT-NET
               PERFORM 4650-ASSESS-PREFERRED
               PERFORM 4660-ASSESS-HOLD
               PERFORM 4670-LIMIT-STATE-TAX
               PERFORM 4680-DESCRIBE-PAY-FREQ
               PERFORM 4690-CHECK-NEXT-REVIEW
               PERFORM 4700-EDIT-MEMO
               PERFORM 4710-COUNT-SEQ
               PERFORM 4720-LIMIT-FED-TAX
               PERFORM 4730-AGE-EFFECTIVE
               PERFORM 7010-CALL-PYFEE
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
      * ACCUMULATE STATE TAX AMT BY PAY FREQ CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3000-ACCUM-PAY-FREQ.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PRLM-PAY-FREQ-CD
                   ADD PRLT-STATE-TAX-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PRLM-PAY-FREQ-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PRLT-STATE-TAX-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE GROSS FEE ON THE FED TAX AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3010-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN PRLM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO PRLM-GROSS-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PRLM-HOLD-FLAG = 'Y'
                   COMPUTE PRLM-GROSS-AMT ROUNDED = PRLM-FED-TAX-AMT *
                       PRLM-RATE / 200
               WHEN PRLM-PAY-FREQ-CD = 'JT'
                   MOVE 15.00 TO PRLM-GROSS-AMT
               WHEN PRLM-PAY-FREQ-CD = 'XM'
                   MOVE 10.00 TO PRLM-GROSS-AMT
               WHEN OTHER
                   COMPUTE PRLM-GROSS-AMT ROUNDED = PRLM-FED-TAX-AMT *
                       PRLM-RATE / 100
           END-EVALUATE
           IF PRLM-GROSS-AMT > 0 AND PRLM-GROSS-AMT < 1.00
               MOVE 1.00 TO PRLM-GROSS-AMT
           END-IF
           IF PRLM-GROSS-AMT > 100.00
               MOVE 100.00 TO PRLM-GROSS-AMT
           END-IF
           ADD PRLM-GROSS-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3020-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF PRLT-EFF-DATE > 0 AND PRLT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PRLT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PRLM-NET-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PRLM-NET-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD PRLM-NET-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PRLM-NET-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PRLM-NET-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3030-AGE-EXPIRY.
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
                   ADD PRLM-FED-TAX-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PRLM-FED-TAX-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD PRLM-FED-TAX-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PRLM-FED-TAX-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PRLM-FED-TAX-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3040-CHECK-LAST-PAY.
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
           IF WS-DAYS > 180
               MOVE 'Y' TO PRLT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NET AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3050-FLAG-NET.
           IF PRLT-NET-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PRLT-NET-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FICA AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3060-SPLIT-FICA.
           COMPUTE WS-WORK-AMT = PRLT-FICA-AMT / 2
           COMPUTE WS-WORK-AMT-2 = PRLT-FICA-AMT - (WS-WORK-AMT * 2)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FED TAX AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3070-ACCRUE-FED-TAX.
           IF PRLM-FED-TAX-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PRLM-FED-TAX-AMT * PRLM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO PRLM-NET-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FICA AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3080-SPLIT-FICA.
           COMPUTE WS-WORK-AMT = PRLM-FICA-AMT / 12
           COMPUTE WS-WORK-AMT-2 = PRLM-FICA-AMT - (WS-WORK-AMT * 12)
           COMPUTE PRLM-STATE-TAX-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 365 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3090-CHECK-EFF.
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
           IF WS-DAYS > 365
               MOVE 'Y' TO PRLM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE NET AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3100-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PRLT-CHANNEL-CD
                   ADD PRLM-NET-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PRLT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PRLM-NET-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * MV -> RD WHEN NOTICE DATE IS PAST
      * RD -> ST WHEN NOTICE DATE IS PAST
      * ST -> TJ WHEN NOTICE DATE IS PAST
      * TJ -> UC WHEN NOTICE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3110-SET-STATUS.
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
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3120-AGE-POST.
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
               WHEN WS-DAYS NOT > 30
                   ADD PRLM-GROSS-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD PRLM-GROSS-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PRLM-GROSS-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE NET YTD FEE ON THE STATE TAX YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3130-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN PRLM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO PRLM-NET-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PRLM-PREFERRED-FLAG = 'Y'
                   COMPUTE PRLM-NET-YTD ROUNDED = PRLM-STATE-TAX-YTD *
                       PRLM-MARGIN / 200
               WHEN PRLM-REASON-CD = 'B'
                   MOVE 20.00 TO PRLM-NET-YTD
               WHEN PRLM-REASON-CD = 'C'
                   MOVE 3.00 TO PRLM-NET-YTD
               WHEN OTHER
                   COMPUTE PRLM-NET-YTD ROUNDED = PRLM-STATE-TAX-YTD *
                       PRLM-MARGIN / 100
           END-EVALUATE
           IF PRLM-NET-YTD > 0 AND PRLM-NET-YTD < 10.00
               MOVE 10.00 TO PRLM-NET-YTD
           END-IF
           IF PRLM-NET-YTD > 35.00
               MOVE 35.00 TO PRLM-NET-YTD
           END-IF
           ADD PRLM-NET-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3140-DESCRIBE-STATUS.
           EVALUATE PRLM-STATUS-CD
               WHEN 'MV'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'RD'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ST'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'TJ'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'UC'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NET AMT IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3150-LIMIT-NET.
           IF PRLM-NET-AMT < 100.00
               MOVE 100.00 TO PRLM-NET-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PRLM-NET-AMT > 10000.00
               MOVE 10000.00 TO PRLM-NET-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE DEDUCT YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3160-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PRLM-REGION-CD
                   ADD PRLM-DEDUCT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PRLM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PRLM-DEDUCT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3170-DESCRIBE-STATUS.
           EVALUATE PRLM-STATUS-CD
               WHEN 'MV'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'RD'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'ST'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'TJ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'UC'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE FED TAX YTD BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3180-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PRLT-TRAN-CD
                   ADD PRLM-FED-TAX-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PRLT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PRLM-FED-TAX-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3190-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF PRLM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PRLM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO PRLM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATE TAX YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3200-FLAG-STATE-TAX.
           IF PRLM-STATE-TAX-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PRLM-STATE-TAX-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FICA AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3210-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PRLT-CHANNEL-CD
                   ADD PRLM-FICA-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PRLT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PRLM-FICA-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FICA YTD FEE ON THE STATE TAX AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3220-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN PRLM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO PRLM-FICA-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PRLM-HOLD-FLAG = 'Y'
                   COMPUTE PRLM-FICA-YTD ROUNDED = PRLM-STATE-TAX-AMT *
                       PRLM-FEE-PCT / 200
               WHEN PRLM-TAX-STATE-CD = 'CAZ'
                   MOVE 5.00 TO PRLM-FICA-YTD
               WHEN PRLM-TAX-STATE-CD = 'LZU'
                   MOVE 5.00 TO PRLM-FICA-YTD
               WHEN OTHER
                   COMPUTE PRLM-FICA-YTD ROUNDED = PRLM-STATE-TAX-AMT *
                       PRLM-FEE-PCT / 100
           END-EVALUATE
           IF PRLM-FICA-YTD > 0 AND PRLM-FICA-YTD < 2.00
               MOVE 2.00 TO PRLM-FICA-YTD
           END-IF
           IF PRLM-FICA-YTD > 250.00
               MOVE 250.00 TO PRLM-FICA-YTD
           END-IF
           ADD PRLM-FICA-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3230-DESCRIBE-STATUS.
           EVALUATE PRLM-STATUS-CD
               WHEN 'MV'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'RD'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ST'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'TJ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'UC'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3240-DESCRIBE-REASON.
           EVALUATE PRLM-REASON-CD
               WHEN 'B'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'C'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'G'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'H'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'L'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'T'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'X'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3250-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF PRLM-CLOSE-DATE > 0 AND PRLM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PRLM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PRLM-STATE-TAX-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PRLM-STATE-TAX-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD PRLM-STATE-TAX-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PRLM-STATE-TAX-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PRLM-STATE-TAX-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE GROSS AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3260-ACCUM-CHANNEL.
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
      * DAILY INTEREST ON NET AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3270-ACCRUE-NET.
           IF PRLT-NET-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PRLT-NET-AMT * PRLM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO PRLT-DEDUCT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3280-AGE-NEXT-REVIEW.
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
                   ADD PRLT-FICA-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD PRLT-FICA-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD PRLT-FICA-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PRLT-FICA-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PRLT-FICA-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE FICA AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3290-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PRLM-STATUS-CD
                   ADD PRLT-FICA-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PRLM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PRLT-FICA-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON NET AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3300-ACCRUE-NET.
           IF PRLT-NET-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PRLT-NET-AMT * PRLM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO PRLM-NET-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TAX STATE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3310-DESCRIBE-TAX-STATE.
           EVALUATE PRLM-TAX-STATE-CD
               WHEN 'CAZ'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'LZU'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'TUF'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'WKE'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * B -> C WHEN EXPIRY DATE IS PAST
      * C -> G WHEN EXPIRY DATE IS PAST
      * G -> H WHEN EXPIRY DATE IS PAST
      * H -> L WHEN EXPIRY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3320-SET-REASON.
           EVALUATE PRLM-REASON-CD
               WHEN 'B'
                   IF PRLM-EXPIRY-DATE > 0 AND PRLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'C' TO PRLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'C'
                   IF PRLM-EXPIRY-DATE > 0 AND PRLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'G' TO PRLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'G'
                   IF PRLM-EXPIRY-DATE > 0 AND PRLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO PRLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF PRLM-EXPIRY-DATE > 0 AND PRLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO PRLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 15 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3330-CHECK-LAST-PAY.
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
           IF WS-DAYS > 15
               MOVE 'Y' TO PRLM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE NET FEE ON THE FICA AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3340-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN PRLM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO PRLM-NET-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PRLT-REVERSAL-FLAG = 'Y'
                   COMPUTE PRLM-NET-AMT ROUNDED = PRLM-FICA-AMT *
                       PRLM-FEE-PCT / 200
               WHEN PRLM-TAX-STATE-CD = 'CAZ'
                   MOVE 5.00 TO PRLM-NET-AMT
               WHEN PRLM-TAX-STATE-CD = 'LZU'
                   MOVE 7.00 TO PRLM-NET-AMT
               WHEN OTHER
                   COMPUTE PRLM-NET-AMT ROUNDED = PRLM-FICA-AMT *
                       PRLM-FEE-PCT / 100
           END-EVALUATE
           IF PRLM-NET-AMT > 0 AND PRLM-NET-AMT < 5.00
               MOVE 5.00 TO PRLM-NET-AMT
           END-IF
           IF PRLM-NET-AMT > 250.00
               MOVE 250.00 TO PRLM-NET-AMT
           END-IF
           ADD PRLM-NET-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-90, 91-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3350-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF PRLT-EFF-DATE > 0 AND PRLT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PRLT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PRLM-STATE-TAX-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD PRLM-STATE-TAX-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD PRLM-STATE-TAX-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PRLM-STATE-TAX-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PRLM-STATE-TAX-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3360-CHECK-EXPIRY.
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
           IF WS-DAYS > 30
               MOVE 'Y' TO PRLM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-90, 91-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3370-AGE-LAST-ACT.
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
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE DEDUCT AMT BY TAX STATE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3380-ACCUM-TAX-STATE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PRLM-TAX-STATE-CD
                   ADD PRLT-DEDUCT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PRLM-TAX-STATE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PRLT-DEDUCT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3390-FLAG-ITEM.
           IF WS-WORK-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DEDUCT YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3400-SPLIT-DEDUCT.
           COMPUTE WS-WORK-AMT = PRLM-DEDUCT-YTD / 12
           COMPUTE WS-WORK-AMT-2 = PRLM-DEDUCT-YTD - (WS-WORK-AMT * 12)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3410-SPLIT-STATE-TAX.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 12)
           COMPUTE PRLM-STATE-TAX-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FED TAX YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FICA AMT.
      *----------------------------------------------------------------*
       3420-PRORATE-FICA.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PRLT-FICA-AMT ROUNDED =
                   PRLM-FED-TAX-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PRLT-FICA-AMT
           END-IF
           ADD PRLT-FICA-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3430-AGE-NEXT-REVIEW.
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
                   ADD PRLM-NET-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD PRLM-NET-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD PRLM-NET-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD PRLM-NET-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PRLM-NET-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON NET YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3440-ACCRUE-NET.
           IF PRLM-NET-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PRLM-NET-YTD * PRLM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO PRLM-FED-TAX-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATE TAX AMT IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3450-LIMIT-STATE-TAX.
           IF PRLT-STATE-TAX-AMT < 0.00
               MOVE 0.00 TO PRLT-STATE-TAX-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PRLT-STATE-TAX-AMT > 50000.00
               MOVE 50000.00 TO PRLT-STATE-TAX-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3460-COUNT-PAYMENT.
           IF PRLM-PAYMENT-CNT < 9999999
               ADD 1 TO PRLM-PAYMENT-CNT
           END-IF
           IF PRLM-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO PRLM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FED TAX TIER TIER FOR THE FED TAX AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3470-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FED-TAX-TIER-RATE(1) TO PRLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF PRLM-FED-TAX-AMT NOT < WS-FED-TAX-TIER-LOW(WS-TX)
                   MOVE WS-FED-TAX-TIER-RATE(WS-TX) TO PRLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PRLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PRLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3480-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3490-COUNT-SEQ.
           IF PRLT-SEQ < 9999999
               ADD 1 TO PRLT-SEQ
           END-IF
           IF PRLT-SEQ NOT < 12
               MOVE 'Y' TO PRLM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE STATE TAX AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3500-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PRLM-REGION-CD
                   ADD PRLT-STATE-TAX-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PRLM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PRLT-STATE-TAX-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FICA AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3510-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PRLM-REASON-CD
                   ADD PRLM-FICA-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PRLM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PRLM-FICA-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * PR -> AD WHEN NEXT DUE DATE IS PAST
      * AD -> VD WHEN NEXT DUE DATE IS PAST
      * VD -> TX WHEN NEXT DUE DATE IS PAST
      * TX -> DP WHEN NEXT DUE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3520-SET-TRAN.
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
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E236 ADDR REQUIRED
      * E303 NET AMT NEGATIVE
      * E708 NET AMT OVER 99999
      * E894 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3530-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PRLM-ADDR = SPACES
                   MOVE 'E236' TO WS-REJECT-CODE
               WHEN PRLM-NET-AMT < 0
                   MOVE 'E303' TO WS-REJECT-CODE
               WHEN PRLM-NET-AMT > 99999.00
                   MOVE 'E708' TO WS-REJECT-CODE
               WHEN PRLM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E894' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3540-COUNT-NOTICE.
           IF PRLM-NOTICE-CNT < 9999999
               ADD 1 TO PRLM-NOTICE-CNT
           END-IF
           IF PRLM-NOTICE-CNT NOT < 10
               MOVE 'Y' TO PRLM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FICA AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3550-SPLIT-FICA.
           COMPUTE WS-WORK-AMT = PRLM-FICA-AMT / 4
           COMPUTE WS-WORK-AMT-2 = PRLM-FICA-AMT - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE WORK AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3560-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN PRLM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PRLM-HOLD-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = WS-WORK-AMT *
                       PRLM-FEE-PCT / 200
               WHEN PRLM-STATUS-CD = 'MV'
                   MOVE 15.00 TO WS-WORK-AMT-2
               WHEN PRLM-STATUS-CD = 'RD'
                   MOVE 3.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = WS-WORK-AMT *
                       PRLM-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 2.00
               MOVE 2.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 50.00
               MOVE 50.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE STATE TAX FEE ON THE STATE TAX AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3570-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN PRLM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO PRLM-STATE-TAX-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PRLM-PREFERRED-FLAG = 'Y'
                   COMPUTE PRLM-STATE-TAX-AMT ROUNDED =
                       PRLT-STATE-TAX-AMT * PRLM-FEE-PCT / 200
               WHEN PRLM-STATUS-CD = 'MV'
                   MOVE 7.00 TO PRLM-STATE-TAX-AMT
               WHEN PRLM-STATUS-CD = 'RD'
                   MOVE 10.00 TO PRLM-STATE-TAX-AMT
               WHEN OTHER
                   COMPUTE PRLM-STATE-TAX-AMT ROUNDED =
                       PRLT-STATE-TAX-AMT * PRLM-FEE-PCT / 100
           END-EVALUATE
           IF PRLM-STATE-TAX-AMT > 0 AND PRLM-STATE-TAX-AMT < 2.00
               MOVE 2.00 TO PRLM-STATE-TAX-AMT
           END-IF
           IF PRLM-STATE-TAX-AMT > 50.00
               MOVE 50.00 TO PRLM-STATE-TAX-AMT
           END-IF
           ADD PRLM-STATE-TAX-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 10 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3580-CHECK-LAST-PAY.
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
               MOVE 'Y' TO PRLM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FICA YTD BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3590-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PRLT-TRAN-CD
                   ADD PRLM-FICA-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PRLT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PRLM-FICA-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON NET YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3600-ACCRUE-NET.
           IF PRLM-NET-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PRLM-NET-YTD * PRLM-RATE / 100 / 365
               ADD WS-WORK-AMT TO PRLM-STATE-TAX-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3610-DESCRIBE-CHANNEL.
           EVALUATE PRLT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * DP -> TU WHEN NOTICE DATE IS PAST
      * TU -> WD WHEN NOTICE DATE IS PAST
      * WD -> DP WHEN NOTICE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3620-SET-REGION.
           EVALUATE PRLM-REGION-CD
               WHEN 'DP'
                   IF PRLM-NOTICE-DATE > 0 AND PRLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TU' TO PRLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TU'
                   IF PRLM-NOTICE-DATE > 0 AND PRLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WD' TO PRLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WD'
                   IF PRLM-NOTICE-DATE > 0 AND PRLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DP' TO PRLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3630-DESCRIBE-CHANNEL.
           EVALUATE PRLT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3640-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3650-COUNT-ITEM.
           IF PRLM-ITEM-CNT < 9999999
               ADD 1 TO PRLM-ITEM-CNT
           END-IF
           IF PRLM-ITEM-CNT NOT < 12
               MOVE 'Y' TO PRLM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT GROSS AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3660-SPLIT-GROSS.
           COMPUTE WS-WORK-AMT = PRLM-GROSS-AMT / 3
           COMPUTE WS-WORK-AMT-2 = PRLM-GROSS-AMT - (WS-WORK-AMT * 3)
           COMPUTE PRLM-NET-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DEDUCT AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3670-ACCRUE-DEDUCT.
           IF PRLM-DEDUCT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PRLM-DEDUCT-AMT * PRLM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO PRLM-DEDUCT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E641 MEMO REQUIRED
      * E027 STATE TAX AMT NEGATIVE
      * E071 STATE TAX AMT OVER 1000000
      * E093 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3680-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PRLT-MEMO = SPACES
                   MOVE 'E641' TO WS-REJECT-CODE
               WHEN PRLT-STATE-TAX-AMT < 0
                   MOVE 'E027' TO WS-REJECT-CODE
               WHEN PRLT-STATE-TAX-AMT > 1000000.00
                   MOVE 'E071' TO WS-REJECT-CODE
               WHEN PRLM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E093' TO WS-REJECT-CODE
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
       3690-DESCRIBE-REASON.
           EVALUATE PRLM-REASON-CD
               WHEN 'B'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'C'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'G'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'H'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'L'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'T'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'X'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN OPEN DATE IS PAST
      * BAT -> MAL WHEN OPEN DATE IS PAST
      * MAL -> TEL WHEN OPEN DATE IS PAST
      * TEL -> ONL WHEN OPEN DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3700-SET-CHANNEL.
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
      * ASSESS THE GROSS FEE ON THE GROSS YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3710-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN PRLT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO PRLM-GROSS-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PRLM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE PRLM-GROSS-AMT ROUNDED = PRLM-GROSS-YTD *
                       PRLM-RATE / 200
               WHEN PRLM-REGION-CD = 'DP'
                   MOVE 10.00 TO PRLM-GROSS-AMT
               WHEN PRLM-REGION-CD = 'TU'
                   MOVE 20.00 TO PRLM-GROSS-AMT
               WHEN OTHER
                   COMPUTE PRLM-GROSS-AMT ROUNDED = PRLM-GROSS-YTD *
                       PRLM-RATE / 100
           END-EVALUATE
           IF PRLM-GROSS-AMT > 0 AND PRLM-GROSS-AMT < 5.00
               MOVE 5.00 TO PRLM-GROSS-AMT
           END-IF
           IF PRLM-GROSS-AMT > 100.00
               MOVE 100.00 TO PRLM-GROSS-AMT
           END-IF
           ADD PRLM-GROSS-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3720-COUNT-NOTICE.
           IF PRLM-NOTICE-CNT < 9999999
               ADD 1 TO PRLM-NOTICE-CNT
           END-IF
           IF PRLM-NOTICE-CNT NOT < 12
               MOVE 'Y' TO PRLM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 30 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3730-CHECK-EFF.
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
           IF WS-DAYS > 30
               MOVE 'Y' TO PRLM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3740-AGE-NOTICE.
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
                   ADD PRLM-FICA-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD PRLM-FICA-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD PRLM-FICA-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PRLM-FICA-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PRLM-FICA-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * PR -> AD WHEN EXPIRY DATE IS PAST
      * AD -> VD WHEN EXPIRY DATE IS PAST
      * VD -> TX WHEN EXPIRY DATE IS PAST
      * TX -> DP WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3750-SET-TRAN.
           EVALUATE PRLT-TRAN-CD
               WHEN 'PR'
                   IF PRLM-EXPIRY-DATE > 0 AND PRLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AD' TO PRLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AD'
                   IF PRLM-EXPIRY-DATE > 0 AND PRLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VD' TO PRLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VD'
                   IF PRLM-EXPIRY-DATE > 0 AND PRLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TX' TO PRLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TX'
                   IF PRLM-EXPIRY-DATE > 0 AND PRLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DP' TO PRLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3760-DESCRIBE-CHANNEL.
           EVALUATE PRLT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE STATE TAX AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FICA AMT.
      *----------------------------------------------------------------*
       3770-PRORATE-FICA.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PRLT-FICA-AMT ROUNDED =
                   PRLT-STATE-TAX-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PRLT-FICA-AMT
           END-IF
           ADD PRLT-FICA-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE FED TAX TIER TIER FOR THE STATE TAX AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3780-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FED-TAX-TIER-RATE(1) TO PRLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF PRLM-STATE-TAX-AMT NOT < WS-FED-TAX-TIER-LOW(WS-TX)
                   MOVE WS-FED-TAX-TIER-RATE(WS-TX) TO PRLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PRLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PRLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * DP -> TU WHEN EFF DATE IS PAST
      * TU -> WD WHEN EFF DATE IS PAST
      * WD -> DP WHEN EFF DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3790-SET-REGION.
           EVALUATE PRLM-REGION-CD
               WHEN 'DP'
                   IF PRLT-EFF-DATE > 0 AND PRLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TU' TO PRLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TU'
                   IF PRLT-EFF-DATE > 0 AND PRLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WD' TO PRLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WD'
                   IF PRLT-EFF-DATE > 0 AND PRLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DP' TO PRLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATE TAX AMT IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3800-LIMIT-STATE-TAX.
           IF PRLT-STATE-TAX-AMT < 10.00
               MOVE 10.00 TO PRLT-STATE-TAX-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PRLT-STATE-TAX-AMT > 1000.00
               MOVE 1000.00 TO PRLT-STATE-TAX-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3810-DESCRIBE-TRAN.
           EVALUATE PRLT-TRAN-CD
               WHEN 'PR'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'AD'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'VD'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'TX'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'DP'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NET YTD IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3820-LIMIT-NET.
           IF PRLM-NET-YTD < 100.00
               MOVE 100.00 TO PRLM-NET-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PRLM-NET-YTD > 2500.00
               MOVE 2500.00 TO PRLM-NET-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E955 MEMO REQUIRED
      * E339 DEDUCT AMT NEGATIVE
      * E796 DEDUCT AMT OVER 25000
      * E942 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3830-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PRLT-MEMO = SPACES
                   MOVE 'E955' TO WS-REJECT-CODE
               WHEN PRLT-DEDUCT-AMT < 0
                   MOVE 'E339' TO WS-REJECT-CODE
               WHEN PRLT-DEDUCT-AMT > 25000.00
                   MOVE 'E796' TO WS-REJECT-CODE
               WHEN PRLM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E942' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 10 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3840-CHECK-OPEN.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO PRLM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 90 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3850-CHECK-NEXT-REVIEW.
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
           IF WS-DAYS > 90
               MOVE 'Y' TO PRLT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PAY FREQ CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3860-DESCRIBE-PAY-FREQ.
           EVALUATE PRLM-PAY-FREQ-CD
               WHEN 'JT'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'XM'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'ZT'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE NET YTD BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3870-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PRLT-TRAN-CD
                   ADD PRLM-NET-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PRLT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PRLM-NET-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 60 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3880-CHECK-LAST-ACT.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO PRLT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT STATE TAX AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3890-SPLIT-STATE-TAX.
           COMPUTE WS-WORK-AMT = PRLT-STATE-TAX-AMT / 12
           COMPUTE WS-WORK-AMT-2 = PRLT-STATE-TAX-AMT - (WS-WORK-AMT *
               12)
           COMPUTE PRLM-NET-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DEDUCT AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3900-SPLIT-DEDUCT.
           COMPUTE WS-WORK-AMT = PRLM-DEDUCT-AMT / 12
           COMPUTE WS-WORK-AMT-2 = PRLM-DEDUCT-AMT - (WS-WORK-AMT * 12)
           COMPUTE PRLM-DEDUCT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE NET YTD FEE ON THE FICA AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3910-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN PRLT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO PRLM-NET-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PRLM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE PRLM-NET-YTD ROUNDED = PRLM-FICA-AMT *
                       PRLM-FEE-PCT / 200
               WHEN PRLM-TAX-STATE-CD = 'CAZ'
                   MOVE 3.00 TO PRLM-NET-YTD
               WHEN PRLM-TAX-STATE-CD = 'LZU'
                   MOVE 20.00 TO PRLM-NET-YTD
               WHEN OTHER
                   COMPUTE PRLM-NET-YTD ROUNDED = PRLM-FICA-AMT *
                       PRLM-FEE-PCT / 100
           END-EVALUATE
           IF PRLM-NET-YTD > 0 AND PRLM-NET-YTD < 10.00
               MOVE 10.00 TO PRLM-NET-YTD
           END-IF
           IF PRLM-NET-YTD > 150.00
               MOVE 150.00 TO PRLM-NET-YTD
           END-IF
           ADD PRLM-NET-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E352 DESC REQUIRED
      * E943 NET AMT NEGATIVE
      * E370 NET AMT OVER 250000
      * E172 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3920-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PRLM-DESC = SPACES
                   MOVE 'E352' TO WS-REJECT-CODE
               WHEN PRLM-NET-AMT < 0
                   MOVE 'E943' TO WS-REJECT-CODE
               WHEN PRLM-NET-AMT > 250000.00
                   MOVE 'E370' TO WS-REJECT-CODE
               WHEN PRLM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E172' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * GROSS AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3930-FLAG-GROSS.
           IF PRLM-GROSS-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PRLM-GROSS-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-180 AND OVER 180 DAYS.
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
                   ADD PRLM-FICA-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD PRLM-FICA-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD PRLM-FICA-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PRLM-FICA-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PRLM-FICA-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * PR -> AD WHEN OPEN DATE IS PAST
      * AD -> VD WHEN OPEN DATE IS PAST
      * VD -> TX WHEN OPEN DATE IS PAST
      * TX -> DP WHEN OPEN DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3950-SET-TRAN.
           EVALUATE PRLT-TRAN-CD
               WHEN 'PR'
                   IF PRLM-OPEN-DATE > 0 AND PRLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AD' TO PRLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AD'
                   IF PRLM-OPEN-DATE > 0 AND PRLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VD' TO PRLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VD'
                   IF PRLM-OPEN-DATE > 0 AND PRLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TX' TO PRLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TX'
                   IF PRLM-OPEN-DATE > 0 AND PRLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DP' TO PRLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE STATE TAX AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3960-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PRLM-STATUS-CD
                   ADD PRLM-STATE-TAX-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PRLM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PRLM-STATE-TAX-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FED TAX AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO NET AMT.
      *----------------------------------------------------------------*
       3970-PRORATE-NET.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PRLT-NET-AMT ROUNDED =
                   PRLM-FED-TAX-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PRLT-NET-AMT
           END-IF
           ADD PRLT-NET-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * DP -> TU WHEN POST DATE IS PAST
      * TU -> WD WHEN POST DATE IS PAST
      * WD -> DP WHEN POST DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3980-SET-REGION.
           EVALUATE PRLM-REGION-CD
               WHEN 'DP'
                   IF PRLT-POST-DATE > 0 AND PRLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TU' TO PRLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TU'
                   IF PRLT-POST-DATE > 0 AND PRLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WD' TO PRLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WD'
                   IF PRLT-POST-DATE > 0 AND PRLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DP' TO PRLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * DP -> TU WHEN POST DATE IS PAST
      * TU -> WD WHEN POST DATE IS PAST
      * WD -> DP WHEN POST DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3990-SET-REGION.
           EVALUATE PRLM-REGION-CD
               WHEN 'DP'
                   IF PRLT-POST-DATE > 0 AND PRLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TU' TO PRLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TU'
                   IF PRLT-POST-DATE > 0 AND PRLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WD' TO PRLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WD'
                   IF PRLT-POST-DATE > 0 AND PRLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DP' TO PRLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE STATE TAX FEE ON THE NET YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       4000-ASSESS-STOP.
           EVALUATE TRUE
               WHEN PRLM-STOP-FLAG = 'Y'
                   MOVE ZERO TO PRLT-STATE-TAX-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PRLM-PREFERRED-FLAG = 'Y'
                   COMPUTE PRLT-STATE-TAX-AMT ROUNDED = PRLM-NET-YTD *
                       PRLM-FEE-PCT / 200
               WHEN PRLM-STATUS-CD = 'MV'
                   MOVE 3.00 TO PRLT-STATE-TAX-AMT
               WHEN PRLM-STATUS-CD = 'RD'
                   MOVE 20.00 TO PRLT-STATE-TAX-AMT
               WHEN OTHER
                   COMPUTE PRLT-STATE-TAX-AMT ROUNDED = PRLM-NET-YTD *
                       PRLM-FEE-PCT / 100
           END-EVALUATE
           IF PRLT-STATE-TAX-AMT > 0 AND PRLT-STATE-TAX-AMT < 5.00
               MOVE 5.00 TO PRLT-STATE-TAX-AMT
           END-IF
           IF PRLT-STATE-TAX-AMT > 250.00
               MOVE 250.00 TO PRLT-STATE-TAX-AMT
           END-IF
           ADD PRLT-STATE-TAX-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE GROSS YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4010-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PRLM-REGION-CD
                   ADD PRLM-GROSS-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PRLM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PRLM-GROSS-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * B -> C WHEN LAST ACT DATE IS PAST
      * C -> G WHEN LAST ACT DATE IS PAST
      * G -> H WHEN LAST ACT DATE IS PAST
      * H -> L WHEN LAST ACT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4020-SET-REASON.
           EVALUATE PRLM-REASON-CD
               WHEN 'B'
                   IF PRLM-LAST-ACT-DATE > 0 AND PRLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'C' TO PRLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'C'
                   IF PRLM-LAST-ACT-DATE > 0 AND PRLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'G' TO PRLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'G'
                   IF PRLM-LAST-ACT-DATE > 0 AND PRLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO PRLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF PRLM-LAST-ACT-DATE > 0 AND PRLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO PRLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 120 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4030-CHECK-LAST-ACT.
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
           IF WS-DAYS > 120
               MOVE 'Y' TO PRLT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DEDUCT AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4040-SPLIT-DEDUCT.
           COMPUTE WS-WORK-AMT = PRLM-DEDUCT-AMT / 2
           COMPUTE WS-WORK-AMT-2 = PRLM-DEDUCT-AMT - (WS-WORK-AMT * 2)
           COMPUTE PRLT-NET-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4050-AGE-EFFECTIVE.
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
                   ADD PRLT-NET-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD PRLT-NET-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD PRLT-NET-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PRLT-NET-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PRLT-NET-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4060-DESCRIBE-CHANNEL.
           EVALUATE PRLT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       4070-COUNT-CYCLE.
           IF PRLM-CYCLE-CNT < 9999999
               ADD 1 TO PRLM-CYCLE-CNT
           END-IF
           IF PRLM-CYCLE-CNT NOT < 3
               MOVE 'Y' TO PRLM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4080-DESCRIBE-REASON.
           EVALUATE PRLM-REASON-CD
               WHEN 'B'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'C'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'G'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'H'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'L'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'T'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'X'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE DEDUCT FEE ON THE NET YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4090-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN PRLM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO PRLM-DEDUCT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PRLM-STOP-FLAG = 'Y'
                   COMPUTE PRLM-DEDUCT-AMT ROUNDED = PRLM-NET-YTD *
                       PRLM-RATE / 200
               WHEN PRLM-REGION-CD = 'DP'
                   MOVE 10.00 TO PRLM-DEDUCT-AMT
               WHEN PRLM-REGION-CD = 'TU'
                   MOVE 20.00 TO PRLM-DEDUCT-AMT
               WHEN OTHER
                   COMPUTE PRLM-DEDUCT-AMT ROUNDED = PRLM-NET-YTD *
                       PRLM-RATE / 100
           END-EVALUATE
           IF PRLM-DEDUCT-AMT > 0 AND PRLM-DEDUCT-AMT < 2.00
               MOVE 2.00 TO PRLM-DEDUCT-AMT
           END-IF
           IF PRLM-DEDUCT-AMT > 100.00
               MOVE 100.00 TO PRLM-DEDUCT-AMT
           END-IF
           ADD PRLM-DEDUCT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-120, 121-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4100-AGE-LAST-PAY.
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
                   ADD PRLT-NET-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 120
                   ADD PRLT-NET-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD PRLT-NET-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PRLT-NET-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PRLT-NET-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * GROSS AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4110-FLAG-GROSS.
           IF PRLM-GROSS-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PRLM-GROSS-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE STATE TAX AMT BY TAX STATE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       4120-ACCUM-TAX-STATE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PRLM-TAX-STATE-CD
                   ADD PRLT-STATE-TAX-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PRLM-TAX-STATE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PRLT-STATE-TAX-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4130-CHECK-NOTICE.
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
      * SELECT THE GROSS TIER TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4140-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-GROSS-TIER-RATE(1) TO PRLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF WS-WORK-AMT NOT < WS-GROSS-TIER-LOW(WS-TX)
                   MOVE WS-GROSS-TIER-RATE(WS-TX) TO PRLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PRLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PRLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4150-COUNT-CYCLE.
           IF PRLM-CYCLE-CNT < 9999999
               ADD 1 TO PRLM-CYCLE-CNT
           END-IF
           IF PRLM-CYCLE-CNT NOT < 6
               MOVE 'Y' TO PRLT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE GROSS AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4160-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PRLT-TRAN-CD
                   ADD PRLM-GROSS-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PRLT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PRLM-GROSS-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FED TAX YTD FEE ON THE WORK AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4170-ASSESS-STOP.
           EVALUATE TRUE
               WHEN PRLM-STOP-FLAG = 'Y'
                   MOVE ZERO TO PRLM-FED-TAX-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PRLM-PREFERRED-FLAG = 'Y'
                   COMPUTE PRLM-FED-TAX-YTD ROUNDED = WS-WORK-AMT *
                       PRLM-MARGIN / 200
               WHEN PRLT-CHANNEL-CD = 'BRN'
                   MOVE 20.00 TO PRLM-FED-TAX-YTD
               WHEN PRLT-CHANNEL-CD = 'BAT'
                   MOVE 20.00 TO PRLM-FED-TAX-YTD
               WHEN OTHER
                   COMPUTE PRLM-FED-TAX-YTD ROUNDED = WS-WORK-AMT *
                       PRLM-MARGIN / 100
           END-EVALUATE
           IF PRLM-FED-TAX-YTD > 0 AND PRLM-FED-TAX-YTD < 10.00
               MOVE 10.00 TO PRLM-FED-TAX-YTD
           END-IF
           IF PRLM-FED-TAX-YTD > 35.00
               MOVE 35.00 TO PRLM-FED-TAX-YTD
           END-IF
           ADD PRLM-FED-TAX-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4180-CHECK-EXPIRY.
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
           IF WS-DAYS > 30
               MOVE 'Y' TO PRLM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * PR -> AD WHEN OPEN DATE IS PAST
      * AD -> VD WHEN OPEN DATE IS PAST
      * VD -> TX WHEN OPEN DATE IS PAST
      * TX -> DP WHEN OPEN DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4190-SET-TRAN.
           EVALUATE PRLT-TRAN-CD
               WHEN 'PR'
                   IF PRLM-OPEN-DATE > 0 AND PRLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AD' TO PRLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AD'
                   IF PRLM-OPEN-DATE > 0 AND PRLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VD' TO PRLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VD'
                   IF PRLM-OPEN-DATE > 0 AND PRLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TX' TO PRLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TX'
                   IF PRLM-OPEN-DATE > 0 AND PRLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DP' TO PRLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE NET AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4200-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PRLT-CHANNEL-CD
                   ADD PRLT-NET-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PRLT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PRLT-NET-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FED TAX YTD IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4210-LIMIT-FED-TAX.
           IF PRLM-FED-TAX-YTD < 25.00
               MOVE 25.00 TO PRLM-FED-TAX-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PRLM-FED-TAX-YTD > 5000.00
               MOVE 5000.00 TO PRLM-FED-TAX-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATE TAX AMT IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4220-LIMIT-STATE-TAX.
           IF PRLT-STATE-TAX-AMT < 25.00
               MOVE 25.00 TO PRLT-STATE-TAX-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PRLT-STATE-TAX-AMT > 10000.00
               MOVE 10000.00 TO PRLT-STATE-TAX-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE NET AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4230-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PRLM-SOURCE-CD
                   ADD PRLM-NET-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PRLM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PRLM-NET-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4240-DESCRIBE-REGION.
           EVALUATE PRLM-REGION-CD
               WHEN 'DP'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'TU'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'WD'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * DP -> TU WHEN LAST ACT DATE IS PAST
      * TU -> WD WHEN LAST ACT DATE IS PAST
      * WD -> DP WHEN LAST ACT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4250-SET-REGION.
           EVALUATE PRLM-REGION-CD
               WHEN 'DP'
                   IF PRLM-LAST-ACT-DATE > 0 AND PRLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TU' TO PRLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TU'
                   IF PRLM-LAST-ACT-DATE > 0 AND PRLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WD' TO PRLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WD'
                   IF PRLM-LAST-ACT-DATE > 0 AND PRLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DP' TO PRLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT FED TAX AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4260-SPLIT-FED-TAX.
           COMPUTE WS-WORK-AMT = PRLM-FED-TAX-AMT / 4
           COMPUTE WS-WORK-AMT-2 = PRLM-FED-TAX-AMT - (WS-WORK-AMT * 4)
           COMPUTE PRLM-NET-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E401 REJECT CD REQUIRED
      * E432 STATE TAX AMT NEGATIVE
      * E759 STATE TAX AMT OVER 10000
      * E332 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       4270-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PRLT-REJECT-CD = SPACES
                   MOVE 'E401' TO WS-REJECT-CODE
               WHEN PRLT-STATE-TAX-AMT < 0
                   MOVE 'E432' TO WS-REJECT-CODE
               WHEN PRLT-STATE-TAX-AMT > 10000.00
                   MOVE 'E759' TO WS-REJECT-CODE
               WHEN PRLM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E332' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DEDUCT AMT IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4280-LIMIT-DEDUCT.
           IF PRLT-DEDUCT-AMT < 50.00
               MOVE 50.00 TO PRLT-DEDUCT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PRLT-DEDUCT-AMT > 500.00
               MOVE 500.00 TO PRLT-DEDUCT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE STATE TAX YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       4290-PRORATE-STATE-TAX.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PRLM-STATE-TAX-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FICA AMT IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4300-LIMIT-FICA.
           IF PRLM-FICA-AMT < 25.00
               MOVE 25.00 TO PRLM-FICA-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PRLM-FICA-AMT > 2500.00
               MOVE 2500.00 TO PRLM-FICA-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4310-AGE-OPEN.
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
                   ADD PRLM-NET-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD PRLM-NET-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD PRLM-NET-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PRLM-NET-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PRLM-NET-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PAY FREQ CD:
      * JT -> XM WHEN LAST ACT DATE IS PAST
      * XM -> ZT WHEN LAST ACT DATE IS PAST
      * ZT -> JT WHEN LAST ACT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4320-SET-PAY-FREQ.
           EVALUATE PRLM-PAY-FREQ-CD
               WHEN 'JT'
                   IF PRLM-LAST-ACT-DATE > 0 AND PRLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XM' TO PRLM-PAY-FREQ-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XM'
                   IF PRLM-LAST-ACT-DATE > 0 AND PRLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZT' TO PRLM-PAY-FREQ-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZT'
                   IF PRLM-LAST-ACT-DATE > 0 AND PRLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JT' TO PRLM-PAY-FREQ-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * NY -> PL WHEN EFF DATE IS PAST
      * PL -> VK WHEN EFF DATE IS PAST
      * VK -> WB WHEN EFF DATE IS PAST
      * WB -> XL WHEN EFF DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4330-SET-SOURCE.
           EVALUATE PRLM-SOURCE-CD
               WHEN 'NY'
                   IF PRLT-EFF-DATE > 0 AND PRLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PL' TO PRLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PL'
                   IF PRLT-EFF-DATE > 0 AND PRLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VK' TO PRLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VK'
                   IF PRLT-EFF-DATE > 0 AND PRLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WB' TO PRLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WB'
                   IF PRLT-EFF-DATE > 0 AND PRLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XL' TO PRLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4340-DESCRIBE-REGION.
           EVALUATE PRLM-REGION-CD
               WHEN 'DP'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'TU'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'WD'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TAX STATE CD:
      * CAZ -> LZU WHEN OPEN DATE IS PAST
      * LZU -> TUF WHEN OPEN DATE IS PAST
      * TUF -> WKE WHEN OPEN DATE IS PAST
      * WKE -> CAZ WHEN OPEN DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4350-SET-TAX-STATE.
           EVALUATE PRLM-TAX-STATE-CD
               WHEN 'CAZ'
                   IF PRLM-OPEN-DATE > 0 AND PRLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LZU' TO PRLM-TAX-STATE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LZU'
                   IF PRLM-OPEN-DATE > 0 AND PRLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TUF' TO PRLM-TAX-STATE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TUF'
                   IF PRLM-OPEN-DATE > 0 AND PRLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WKE' TO PRLM-TAX-STATE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WKE'
                   IF PRLM-OPEN-DATE > 0 AND PRLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CAZ' TO PRLM-TAX-STATE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE FICA AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO STATE TAX AMT.
      *----------------------------------------------------------------*
       4360-PRORATE-STATE-TAX.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PRLT-STATE-TAX-AMT ROUNDED =
                   PRLM-FICA-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PRLT-STATE-TAX-AMT
           END-IF
           ADD PRLT-STATE-TAX-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       4370-COUNT-PAYMENT.
           IF PRLM-PAYMENT-CNT < 9999999
               ADD 1 TO PRLM-PAYMENT-CNT
           END-IF
           IF PRLM-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO PRLM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE STATE TAX YTD BY TAX STATE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       4380-ACCUM-TAX-STATE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PRLM-TAX-STATE-CD
                   ADD PRLM-STATE-TAX-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PRLM-TAX-STATE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PRLM-STATE-TAX-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E069 CUST ID REQUIRED
      * E863 STATE TAX AMT NEGATIVE
      * E327 STATE TAX AMT OVER 5000
      * E336 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       4390-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PRLM-CUST-ID = SPACES
                   MOVE 'E069' TO WS-REJECT-CODE
               WHEN PRLT-STATE-TAX-AMT < 0
                   MOVE 'E863' TO WS-REJECT-CODE
               WHEN PRLT-STATE-TAX-AMT > 5000.00
                   MOVE 'E327' TO WS-REJECT-CODE
               WHEN PRLT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E336' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE STATE TAX AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FICA AMT.
      *----------------------------------------------------------------*
       4400-PRORATE-FICA.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PRLM-FICA-AMT ROUNDED =
                   PRLT-STATE-TAX-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PRLM-FICA-AMT
           END-IF
           ADD PRLM-FICA-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE DEDUCT YTD FEE ON THE WORK AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4410-ASSESS-STOP.
           EVALUATE TRUE
               WHEN PRLM-STOP-FLAG = 'Y'
                   MOVE ZERO TO PRLM-DEDUCT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PRLM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE PRLM-DEDUCT-YTD ROUNDED = WS-WORK-AMT *
                       PRLM-FEE-PCT / 200
               WHEN PRLT-CHANNEL-CD = 'BRN'
                   MOVE 15.00 TO PRLM-DEDUCT-YTD
               WHEN PRLT-CHANNEL-CD = 'BAT'
                   MOVE 20.00 TO PRLM-DEDUCT-YTD
               WHEN OTHER
                   COMPUTE PRLM-DEDUCT-YTD ROUNDED = WS-WORK-AMT *
                       PRLM-FEE-PCT / 100
           END-EVALUATE
           IF PRLM-DEDUCT-YTD > 0 AND PRLM-DEDUCT-YTD < 5.00
               MOVE 5.00 TO PRLM-DEDUCT-YTD
           END-IF
           IF PRLM-DEDUCT-YTD > 35.00
               MOVE 35.00 TO PRLM-DEDUCT-YTD
           END-IF
           ADD PRLM-DEDUCT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       4420-AGE-LAST-PAY.
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
                   ADD PRLM-STATE-TAX-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PRLM-STATE-TAX-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD PRLM-STATE-TAX-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD PRLM-STATE-TAX-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PRLM-STATE-TAX-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE FED TAX TIER TIER FOR THE STATE TAX AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4430-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-FED-TAX-TIER-RATE(1) TO PRLM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF PRLT-STATE-TAX-AMT NOT < WS-FED-TAX-TIER-LOW(WS-TX)
                   MOVE WS-FED-TAX-TIER-RATE(WS-TX) TO PRLM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PRLM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PRLM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4440-AGE-EFFECTIVE.
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
                   ADD PRLM-FICA-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PRLM-FICA-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD PRLM-FICA-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PRLM-FICA-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PRLM-FICA-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT NET AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4450-SPLIT-NET.
           COMPUTE WS-WORK-AMT = PRLT-NET-AMT / 12
           COMPUTE WS-WORK-AMT-2 = PRLT-NET-AMT - (WS-WORK-AMT * 12)
           COMPUTE PRLM-STATE-TAX-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TAX STATE CD:
      * CAZ -> LZU WHEN LAST ACT DATE IS PAST
      * LZU -> TUF WHEN LAST ACT DATE IS PAST
      * TUF -> WKE WHEN LAST ACT DATE IS PAST
      * WKE -> CAZ WHEN LAST ACT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4460-SET-TAX-STATE.
           EVALUATE PRLM-TAX-STATE-CD
               WHEN 'CAZ'
                   IF PRLM-LAST-ACT-DATE > 0 AND PRLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LZU' TO PRLM-TAX-STATE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LZU'
                   IF PRLM-LAST-ACT-DATE > 0 AND PRLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TUF' TO PRLM-TAX-STATE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TUF'
                   IF PRLM-LAST-ACT-DATE > 0 AND PRLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WKE' TO PRLM-TAX-STATE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WKE'
                   IF PRLM-LAST-ACT-DATE > 0 AND PRLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CAZ' TO PRLM-TAX-STATE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DEDUCT YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4470-FLAG-DEDUCT.
           IF PRLM-DEDUCT-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PRLM-DEDUCT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4480-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF PRLM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PRLM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO PRLM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FICA AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4490-SPLIT-FICA.
           COMPUTE WS-WORK-AMT = PRLT-FICA-AMT / 3
           COMPUTE WS-WORK-AMT-2 = PRLT-FICA-AMT - (WS-WORK-AMT * 3)
           COMPUTE PRLT-NET-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4500-DESCRIBE-CHANNEL.
           EVALUATE PRLT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DEDUCT AMT IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4510-LIMIT-DEDUCT.
           IF PRLM-DEDUCT-AMT < 0.00
               MOVE 0.00 TO PRLM-DEDUCT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PRLM-DEDUCT-AMT > 5000.00
               MOVE 5000.00 TO PRLM-DEDUCT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FED TAX TIER TIER FOR THE FICA YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4520-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-FED-TAX-TIER-RATE(1) TO PRLM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF PRLM-FICA-YTD NOT < WS-FED-TAX-TIER-LOW(WS-TX)
                   MOVE WS-FED-TAX-TIER-RATE(WS-TX) TO PRLM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PRLM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PRLM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E838 OPERATOR REQUIRED
      * E402 GROSS AMT NEGATIVE
      * E738 GROSS AMT OVER 1000000
      * E957 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       4530-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PRLT-OPERATOR = SPACES
                   MOVE 'E838' TO WS-REJECT-CODE
               WHEN PRLM-GROSS-AMT < 0
                   MOVE 'E402' TO WS-REJECT-CODE
               WHEN PRLM-GROSS-AMT > 1000000.00
                   MOVE 'E738' TO WS-REJECT-CODE
               WHEN PRLM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E957' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * DP -> TU WHEN CLOSE DATE IS PAST
      * TU -> WD WHEN CLOSE DATE IS PAST
      * WD -> DP WHEN CLOSE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4540-SET-REGION.
           EVALUATE PRLM-REGION-CD
               WHEN 'DP'
                   IF PRLM-CLOSE-DATE > 0 AND PRLM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TU' TO PRLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TU'
                   IF PRLM-CLOSE-DATE > 0 AND PRLM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WD' TO PRLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WD'
                   IF PRLM-CLOSE-DATE > 0 AND PRLM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DP' TO PRLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4550-AGE-EXPIRY.
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
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT DEDUCT AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4560-SPLIT-DEDUCT.
           COMPUTE WS-WORK-AMT = PRLM-DEDUCT-AMT / 4
           COMPUTE WS-WORK-AMT-2 = PRLM-DEDUCT-AMT - (WS-WORK-AMT * 4)
           COMPUTE PRLT-DEDUCT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FICA AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4570-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PRLM-SOURCE-CD
                   ADD PRLT-FICA-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PRLM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PRLT-FICA-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE STATE TAX YTD BY PAY FREQ CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       4580-ACCUM-PAY-FREQ.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PRLM-PAY-FREQ-CD
                   ADD PRLM-STATE-TAX-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PRLM-PAY-FREQ-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PRLM-STATE-TAX-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT NET YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4590-SPLIT-NET.
           COMPUTE WS-WORK-AMT = PRLM-NET-YTD / 12
           COMPUTE WS-WORK-AMT-2 = PRLM-NET-YTD - (WS-WORK-AMT * 12)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PAY FREQ CD:
      * JT -> XM WHEN LAST PAY DATE IS PAST
      * XM -> ZT WHEN LAST PAY DATE IS PAST
      * ZT -> JT WHEN LAST PAY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4600-SET-PAY-FREQ.
           EVALUATE PRLM-PAY-FREQ-CD
               WHEN 'JT'
                   IF PRLM-LAST-PAY-DATE > 0 AND PRLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XM' TO PRLM-PAY-FREQ-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XM'
                   IF PRLM-LAST-PAY-DATE > 0 AND PRLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZT' TO PRLM-PAY-FREQ-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZT'
                   IF PRLM-LAST-PAY-DATE > 0 AND PRLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JT' TO PRLM-PAY-FREQ-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE FED TAX TIER TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4610-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-FED-TAX-TIER-RATE(1) TO PRLM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF WS-WORK-AMT NOT < WS-FED-TAX-TIER-LOW(WS-TX)
                   MOVE WS-FED-TAX-TIER-RATE(WS-TX) TO PRLM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PRLM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PRLM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E585 NAME REQUIRED
      * E647 FICA YTD NEGATIVE
      * E769 FICA YTD OVER 10000
      * E388 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       4620-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PRLM-NAME = SPACES
                   MOVE 'E585' TO WS-REJECT-CODE
               WHEN PRLM-FICA-YTD < 0
                   MOVE 'E647' TO WS-REJECT-CODE
               WHEN PRLM-FICA-YTD > 10000.00
                   MOVE 'E769' TO WS-REJECT-CODE
               WHEN PRLM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E388' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E905 NAME REQUIRED
      * E384 WORK AMT 2 NEGATIVE
      * E739 WORK AMT 2 OVER 99999
      * E777 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       4630-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PRLM-NAME = SPACES
                   MOVE 'E905' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E384' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 99999.00
                   MOVE 'E739' TO WS-REJECT-CODE
               WHEN PRLM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E777' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * NET AMT IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4640-LIMIT-NET.
           IF PRLT-NET-AMT < 0.00
               MOVE 0.00 TO PRLT-NET-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PRLT-NET-AMT > 10000.00
               MOVE 10000.00 TO PRLT-NET-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE NET YTD FEE ON THE NET AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4650-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN PRLM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO PRLM-NET-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PRLM-HOLD-FLAG = 'Y'
                   COMPUTE PRLM-NET-YTD ROUNDED = PRLT-NET-AMT *
                       PRLM-FEE-PCT / 200
               WHEN PRLT-CHANNEL-CD = 'BRN'
                   MOVE 20.00 TO PRLM-NET-YTD
               WHEN PRLT-CHANNEL-CD = 'BAT'
                   MOVE 7.00 TO PRLM-NET-YTD
               WHEN OTHER
                   COMPUTE PRLM-NET-YTD ROUNDED = PRLT-NET-AMT *
                       PRLM-FEE-PCT / 100
           END-EVALUATE
           IF PRLM-NET-YTD > 0 AND PRLM-NET-YTD < 2.00
               MOVE 2.00 TO PRLM-NET-YTD
           END-IF
           IF PRLM-NET-YTD > 25.00
               MOVE 25.00 TO PRLM-NET-YTD
           END-IF
           ADD PRLM-NET-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE DEDUCT FEE ON THE FICA YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4660-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN PRLM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO PRLM-DEDUCT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PRLM-STOP-FLAG = 'Y'
                   COMPUTE PRLM-DEDUCT-AMT ROUNDED = PRLM-FICA-YTD *
                       PRLM-FEE-PCT / 200
               WHEN PRLM-STATUS-CD = 'MV'
                   MOVE 10.00 TO PRLM-DEDUCT-AMT
               WHEN PRLM-STATUS-CD = 'RD'
                   MOVE 7.00 TO PRLM-DEDUCT-AMT
               WHEN OTHER
                   COMPUTE PRLM-DEDUCT-AMT ROUNDED = PRLM-FICA-YTD *
                       PRLM-FEE-PCT / 100
           END-EVALUATE
           IF PRLM-DEDUCT-AMT > 0 AND PRLM-DEDUCT-AMT < 2.00
               MOVE 2.00 TO PRLM-DEDUCT-AMT
           END-IF
           IF PRLM-DEDUCT-AMT > 35.00
               MOVE 35.00 TO PRLM-DEDUCT-AMT
           END-IF
           ADD PRLM-DEDUCT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATE TAX AMT IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4670-LIMIT-STATE-TAX.
           IF PRLT-STATE-TAX-AMT < 100.00
               MOVE 100.00 TO PRLT-STATE-TAX-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PRLT-STATE-TAX-AMT > 1000.00
               MOVE 1000.00 TO PRLT-STATE-TAX-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PAY FREQ CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4680-DESCRIBE-PAY-FREQ.
           EVALUATE PRLM-PAY-FREQ-CD
               WHEN 'JT'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'XM'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'ZT'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 120 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4690-CHECK-NEXT-REVIEW.
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
           IF WS-DAYS > 120
               MOVE 'Y' TO PRLM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E626 MEMO REQUIRED
      * E604 FICA AMT NEGATIVE
      * E255 FICA AMT OVER 50000
      * E218 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       4700-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PRLT-MEMO = SPACES
                   MOVE 'E626' TO WS-REJECT-CODE
               WHEN PRLM-FICA-AMT < 0
                   MOVE 'E604' TO WS-REJECT-CODE
               WHEN PRLM-FICA-AMT > 50000.00
                   MOVE 'E255' TO WS-REJECT-CODE
               WHEN PRLM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E218' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       4710-COUNT-SEQ.
           IF PRLT-SEQ < 9999999
               ADD 1 TO PRLT-SEQ
           END-IF
           IF PRLT-SEQ NOT < 12
               MOVE 'Y' TO PRLM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FED TAX YTD IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4720-LIMIT-FED-TAX.
           IF PRLM-FED-TAX-YTD < 0.00
               MOVE 0.00 TO PRLM-FED-TAX-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PRLM-FED-TAX-YTD > 1000.00
               MOVE 1000.00 TO PRLM-FED-TAX-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4730-AGE-EFFECTIVE.
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
                   ADD PRLT-NET-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD PRLT-NET-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD PRLT-NET-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD PRLT-NET-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PRLT-NET-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

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
                   MOVE PYFEE-NET-AMT
                     TO PRLM-NET-AMT
                   MOVE PYFEE-DEDUCT-YTD
                     TO PRLM-DEDUCT-YTD
                   MOVE PYFEE-AUTO-RENEW-FLAG
                     TO PRLM-AUTO-RENEW-FLAG
                   MOVE PYFEE-HOLD-FLAG
                     TO PRLM-HOLD-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RFEE' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-PYFEE' TO ABND-PARAGRAPH
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
           DISPLAY 'PYLRPT READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'PYLRPT' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
