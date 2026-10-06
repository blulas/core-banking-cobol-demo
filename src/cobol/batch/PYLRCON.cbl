      *================================================================*
      * PROGRAM  : PYLRCON                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : PAYROLL SERVICES - RECONCILIATION. READS THE PRL    *
      *            TRANSACTIONS, UPDATES THE PRL MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. CLIENT PAYROLL CALCULATION, TAX *
      *            DEPOSIT AND DIRECT DEPOSIT                          *
      * INPUT    : PRLMST (KSDS), PRLTRN, BANKCTL                      *
      * OUTPUT   : PRLOUT, PYLRCRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, PYFEE, PYTIER           *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1989-11-23 MKL  ORIGINAL                                       *
      * 1991-06-21 SKN  AUDIT FINDING - GROSS CONTROL TOTALS INC6489   *
      * 1994-01-26 DPS  FICA CALCULATION CORRECTED PRB6214             *
      * 1995-03-14 MKL  STATE-TAX TABLE EXPANDED PRB9343               *
      * 1996-11-11 EWS  STATE-TAX EXCEPTIONS TO OPS QUEUE PRB9856      *
      * 1996-09-09 LMB  NEW STATE-TAX REPORT COLUMNS PRB2192           *
      * 1999-08-04 SKN  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2001-03-06 SKN  STATE-TAX LIMIT REVISED CHG7652                *
      * 2003-07-01 JLM  GROSS LIMIT REVISED INC5700                    *
      * 2004-05-21 TWH  NEW DEDUCT REPORT COLUMNS PRB5106              *
      * 2007-06-04 JLM  DEDUCT ROUNDING - HALF UP INC8566              *
      * 2008-06-15 GAF  FED-TAX LIMIT REVISED REQ1496                  *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PYLRCON.
       AUTHOR. R J TRAVERS.

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
           SELECT REPORT-FILE ASSIGN TO "PYLRCRPT"
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
                                           VALUE 24.000000.
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
      * WS-FICA-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FICA-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000212000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000897500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012500000000770000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012750000000119500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015250000000666000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015300000000218500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017800000000356500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018300000000228000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023300000000717000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023800000000468500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00033800000000181500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00034050000000789500'.
       01  WS-FICA-TIER-TBL REDEFINES WS-FICA-TIER-VALUES.
           05  WS-FICA-TIER-ROW OCCURS 12 TIMES.
               10  WS-FICA-TIER-LOW        PIC 9(09)V99.
               10  WS-FICA-TIER-RATE       PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-DEDUCT-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-DEDUCT-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000822000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000195000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001250000000158500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001750000000844500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011750000000873000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012000000000753000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013000000000505000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014000000000530000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00019000000000803000'.
       01  WS-DEDUCT-GRID-TBL REDEFINES WS-DEDUCT-GRID-VALUES.
           05  WS-DEDUCT-GRID-ROW OCCURS 9 TIMES.
               10  WS-DEDUCT-GRID-LOW      PIC 9(09)V99.
               10  WS-DEDUCT-GRID-RATE     PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-FICA-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FICA-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000859000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000896000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002600000000239000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007600000000064000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008600000000608500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008650000000844500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008900000000623500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00009400000000212000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00009450000000284500'.
       01  WS-FICA-SCHED-TBL REDEFINES WS-FICA-SCHED-VALUES.
           05  WS-FICA-SCHED-ROW OCCURS 9 TIMES.
               10  WS-FICA-SCHED-LOW       PIC 9(09)V99.
               10  WS-FICA-SCHED-RATE      PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-DEDUCT-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-DEDUCT-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000384000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000024500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000750000000593500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000800000000671000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010800000000601500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010900000000411000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011400000000868000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016400000000681000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026400000000175000'.
       01  WS-DEDUCT-BAND-TBL REDEFINES WS-DEDUCT-BAND-VALUES.
           05  WS-DEDUCT-BAND-ROW OCCURS 9 TIMES.
               10  WS-DEDUCT-BAND-LOW      PIC 9(09)V99.
               10  WS-DEDUCT-BAND-RATE     PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  PYLRCON  PAYROLL SERVICES'.
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
       COPY PYTIERP.

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
               PERFORM 3000-SPLIT-FED-TAX
               PERFORM 3010-SELECT-RATE
               PERFORM 3020-AGE-LAST-ACT
               PERFORM 3030-PRORATE-STATE-TAX
               PERFORM 3040-SET-TRAN
               PERFORM 3050-FLAG-ITEM
               PERFORM 3060-DESCRIBE-REGION
               PERFORM 3070-FLAG-FED-TAX
               PERFORM 3080-AGE-NEXT-REVIEW
               PERFORM 3090-FLAG-NET
               PERFORM 3100-ACCRUE-STATE-TAX
               PERFORM 3110-DESCRIBE-REGION
               PERFORM 3120-ACCUM-STATUS
               PERFORM 3130-EDIT-DESC
               PERFORM 3140-LIMIT-DEDUCT
               PERFORM 3150-AGE-NEXT-DUE
               PERFORM 3160-SET-REGION
               PERFORM 3170-ACCUM-CHANNEL
               PERFORM 3180-SELECT-FEE-PCT
               PERFORM 3190-EDIT-REF-NO
               PERFORM 3200-SELECT-RATE
               PERFORM 3210-ACCUM-TAX-STATE
               PERFORM 3220-PRORATE-STATE-TAX
               PERFORM 3230-LIMIT-NET
               PERFORM 3240-CHECK-EFF
               PERFORM 3250-AGE-NEXT-DUE
               PERFORM 3260-SELECT-RATE
               PERFORM 3270-AGE-NOTICE
               PERFORM 3280-ACCRUE-STATE-TAX
               PERFORM 3290-PRORATE-STATE-TAX
               PERFORM 3300-LIMIT-FICA
               PERFORM 3310-CHECK-NEXT-DUE
               PERFORM 3320-PRORATE-FICA
               PERFORM 3330-SELECT-MARGIN
               PERFORM 3340-FLAG-NET
               PERFORM 3350-FLAG-GROSS
               PERFORM 3360-ACCRUE-FICA
               PERFORM 3370-SELECT-FEE-PCT
               PERFORM 3380-ACCRUE-STATE-TAX
               PERFORM 3390-DESCRIBE-REGION
               PERFORM 3400-ASSESS-HOLD
               PERFORM 3410-SET-SOURCE
               PERFORM 3420-CHECK-OPEN
               PERFORM 3430-DESCRIBE-REGION
               PERFORM 3440-AGE-CLOSE
               PERFORM 3450-ASSESS-NOTICE-SENT
               PERFORM 3460-FLAG-FICA
               PERFORM 3470-SPLIT-FICA
               PERFORM 3480-CHECK-POST
               PERFORM 3490-SPLIT-FICA
               PERFORM 3500-SPLIT-FICA
               PERFORM 3510-PRORATE-GROSS
               PERFORM 3520-SET-REGION
               PERFORM 3530-ASSESS-STOP
               PERFORM 3540-EDIT-REF-NO
               PERFORM 3550-FLAG-FICA
               PERFORM 3560-ACCUM-PAY-FREQ
               PERFORM 3570-CHECK-LAST-PAY
               PERFORM 3580-CHECK-POST
               PERFORM 3590-LIMIT-FED-TAX
               PERFORM 3600-DESCRIBE-REASON
               PERFORM 3610-CHECK-NEXT-REVIEW
               PERFORM 3620-LIMIT-ITEM
               PERFORM 3630-ACCUM-CHANNEL
               PERFORM 3640-AGE-EFFECTIVE
               PERFORM 3650-SPLIT-FICA
               PERFORM 3660-EDIT-REF-NO
               PERFORM 3670-SELECT-MARGIN
               PERFORM 3680-ACCRUE-STATE-TAX
               PERFORM 3690-FLAG-GROSS
               PERFORM 3700-DESCRIBE-SOURCE
               PERFORM 3710-ASSESS-STOP
               PERFORM 3720-AGE-LAST-ACT
               PERFORM 3730-ACCRUE-RATE
               PERFORM 3740-SELECT-FEE-PCT
               PERFORM 3750-CHECK-POST
               PERFORM 3760-PRORATE-DEDUCT
               PERFORM 3770-SPLIT-DEDUCT
               PERFORM 3780-COUNT-SEQ
               PERFORM 3790-ASSESS-HOLD
               PERFORM 3800-AGE-NOTICE
               PERFORM 3810-COUNT-SEQ
               PERFORM 3820-CHECK-NEXT-REVIEW
               PERFORM 3830-SPLIT-FED-TAX
               PERFORM 3840-DESCRIBE-STATUS
               PERFORM 3850-CHECK-OPEN
               PERFORM 3860-SPLIT-DEDUCT
               PERFORM 3870-ASSESS-AUTO-RENEW
               PERFORM 3880-AGE-CLOSE
               PERFORM 3890-CHECK-LAST-PAY
               PERFORM 3900-ACCRUE-GROSS
               PERFORM 3910-SET-REASON
               PERFORM 3920-FLAG-NET
               PERFORM 3930-DESCRIBE-CHANNEL
               PERFORM 3940-PRORATE-GROSS
               PERFORM 3950-COUNT-PAYMENT
               PERFORM 3960-CHECK-NEXT-DUE
               PERFORM 3970-CHECK-NEXT-DUE
               PERFORM 3980-SPLIT-NET
               PERFORM 3990-SET-TRAN
               PERFORM 4000-AGE-LAST-PAY
               PERFORM 4010-CHECK-NEXT-REVIEW
               PERFORM 4020-DESCRIBE-REASON
               PERFORM 4030-PRORATE-FICA
               PERFORM 4040-COUNT-PAYMENT
               PERFORM 4050-AGE-EFFECTIVE
               PERFORM 4060-SELECT-MARGIN
               PERFORM 4070-COUNT-CYCLE
               PERFORM 4080-SELECT-FEE-PCT
               PERFORM 4090-ACCUM-REASON
               PERFORM 4100-ACCRUE-DEDUCT
               PERFORM 4110-DESCRIBE-REASON
               PERFORM 4120-DESCRIBE-STATUS
               PERFORM 4130-LIMIT-ITEM
               PERFORM 4140-SELECT-FEE-PCT
               PERFORM 4150-SELECT-RATE
               PERFORM 4160-DESCRIBE-REGION
               PERFORM 4170-ACCUM-REGION
               PERFORM 4180-SET-REASON
               PERFORM 4190-FLAG-NET
               PERFORM 4200-ACCRUE-FICA
               PERFORM 4210-CHECK-LAST-ACT
               PERFORM 4220-SELECT-MARGIN
               PERFORM 4230-LIMIT-DEDUCT
               PERFORM 4240-LIMIT-STATE-TAX
               PERFORM 4250-SELECT-MARGIN
               PERFORM 4260-ACCUM-REGION
               PERFORM 7010-CALL-PYFEE
               PERFORM 7020-CALL-PYTIER
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
      * SPLIT FED TAX YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3000-SPLIT-FED-TAX.
           COMPUTE WS-WORK-AMT = PRLM-FED-TAX-YTD / 3
           COMPUTE WS-WORK-AMT-2 = PRLM-FED-TAX-YTD - (WS-WORK-AMT * 3)
           COMPUTE PRLT-FICA-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DEDUCT BAND TIER FOR THE FICA AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3010-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-DEDUCT-BAND-RATE(1) TO PRLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF PRLM-FICA-AMT NOT < WS-DEDUCT-BAND-LOW(WS-TX)
                   MOVE WS-DEDUCT-BAND-RATE(WS-TX) TO PRLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PRLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PRLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3020-AGE-LAST-ACT.
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
                   ADD PRLT-NET-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD PRLT-NET-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD PRLT-NET-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PRLT-NET-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PRLT-NET-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE DEDUCT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO STATE TAX AMT.
      *----------------------------------------------------------------*
       3030-PRORATE-STATE-TAX.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PRLM-STATE-TAX-AMT ROUNDED =
                   PRLM-DEDUCT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PRLM-STATE-TAX-AMT
           END-IF
           ADD PRLM-STATE-TAX-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * PR -> AD WHEN LAST ACT DATE IS PAST
      * AD -> VD WHEN LAST ACT DATE IS PAST
      * VD -> TX WHEN LAST ACT DATE IS PAST
      * TX -> DP WHEN LAST ACT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3040-SET-TRAN.
           EVALUATE PRLT-TRAN-CD
               WHEN 'PR'
                   IF PRLM-LAST-ACT-DATE > 0 AND PRLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AD' TO PRLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AD'
                   IF PRLM-LAST-ACT-DATE > 0 AND PRLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VD' TO PRLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VD'
                   IF PRLM-LAST-ACT-DATE > 0 AND PRLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TX' TO PRLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TX'
                   IF PRLM-LAST-ACT-DATE > 0 AND PRLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DP' TO PRLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3050-FLAG-ITEM.
           IF WS-WORK-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3060-DESCRIBE-REGION.
           EVALUATE PRLM-REGION-CD
               WHEN 'DP'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'TU'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'WD'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FED TAX AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3070-FLAG-FED-TAX.
           IF PRLM-FED-TAX-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PRLM-FED-TAX-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3080-AGE-NEXT-REVIEW.
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
                   ADD PRLM-STATE-TAX-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD PRLM-STATE-TAX-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD PRLM-STATE-TAX-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PRLM-STATE-TAX-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PRLM-STATE-TAX-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NET AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3090-FLAG-NET.
           IF PRLM-NET-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PRLM-NET-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STATE TAX AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3100-ACCRUE-STATE-TAX.
           IF PRLM-STATE-TAX-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PRLM-STATE-TAX-AMT * PRLM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO PRLT-FICA-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3110-DESCRIBE-REGION.
           EVALUATE PRLM-REGION-CD
               WHEN 'DP'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'TU'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'WD'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE DEDUCT YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3120-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PRLM-STATUS-CD
                   ADD PRLM-DEDUCT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PRLM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PRLM-DEDUCT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E511 DESC REQUIRED
      * E750 WORK AMT NEGATIVE
      * E751 WORK AMT OVER 99999
      * E164 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3130-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PRLM-DESC = SPACES
                   MOVE 'E511' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E750' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 99999.00
                   MOVE 'E751' TO WS-REJECT-CODE
               WHEN PRLM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E164' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DEDUCT AMT IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3140-LIMIT-DEDUCT.
           IF PRLT-DEDUCT-AMT < 25.00
               MOVE 25.00 TO PRLT-DEDUCT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PRLT-DEDUCT-AMT > 10000.00
               MOVE 10000.00 TO PRLT-DEDUCT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3150-AGE-NEXT-DUE.
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
                   ADD PRLM-FED-TAX-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PRLM-FED-TAX-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD PRLM-FED-TAX-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PRLM-FED-TAX-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PRLM-FED-TAX-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * DP -> TU WHEN NOTICE DATE IS PAST
      * TU -> WD WHEN NOTICE DATE IS PAST
      * WD -> DP WHEN NOTICE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3160-SET-REGION.
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
      * ACCUMULATE FED TAX AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3170-ACCUM-CHANNEL.
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
      * SELECT THE FICA TIER TIER FOR THE STATE TAX AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3180-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FICA-TIER-RATE(1) TO PRLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF PRLT-STATE-TAX-AMT NOT < WS-FICA-TIER-LOW(WS-TX)
                   MOVE WS-FICA-TIER-RATE(WS-TX) TO PRLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PRLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PRLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E582 REF NO REQUIRED
      * E425 GROSS YTD NEGATIVE
      * E016 GROSS YTD OVER 50000
      * E266 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3190-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PRLM-REF-NO = SPACES
                   MOVE 'E582' TO WS-REJECT-CODE
               WHEN PRLM-GROSS-YTD < 0
                   MOVE 'E425' TO WS-REJECT-CODE
               WHEN PRLM-GROSS-YTD > 50000.00
                   MOVE 'E016' TO WS-REJECT-CODE
               WHEN PRLM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E266' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FICA SCHED TIER FOR THE DEDUCT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3200-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-FICA-SCHED-RATE(1) TO PRLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF PRLM-DEDUCT-AMT NOT < WS-FICA-SCHED-LOW(WS-TX)
                   MOVE WS-FICA-SCHED-RATE(WS-TX) TO PRLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PRLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PRLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE NET YTD BY TAX STATE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3210-ACCUM-TAX-STATE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PRLM-TAX-STATE-CD
                   ADD PRLM-NET-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PRLM-TAX-STATE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PRLM-NET-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE STATE TAX YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3220-PRORATE-STATE-TAX.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PRLM-STATE-TAX-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * NET YTD IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3230-LIMIT-NET.
           IF PRLM-NET-YTD < 25.00
               MOVE 25.00 TO PRLM-NET-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PRLM-NET-YTD > 10000.00
               MOVE 10000.00 TO PRLM-NET-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 180 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3240-CHECK-EFF.
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
           IF WS-DAYS > 180
               MOVE 'Y' TO PRLM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3250-AGE-NEXT-DUE.
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
                   ADD PRLM-DEDUCT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PRLM-DEDUCT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD PRLM-DEDUCT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD PRLM-DEDUCT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PRLM-DEDUCT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE FICA TIER TIER FOR THE GROSS YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3260-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-FICA-TIER-RATE(1) TO PRLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF PRLM-GROSS-YTD NOT < WS-FICA-TIER-LOW(WS-TX)
                   MOVE WS-FICA-TIER-RATE(WS-TX) TO PRLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PRLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PRLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3270-AGE-NOTICE.
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
                   ADD PRLM-NET-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD PRLM-NET-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD PRLM-NET-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD PRLM-NET-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PRLM-NET-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STATE TAX AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3280-ACCRUE-STATE-TAX.
           IF PRLT-STATE-TAX-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PRLT-STATE-TAX-AMT * PRLM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO PRLM-FED-TAX-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FED TAX YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO STATE TAX AMT.
      *----------------------------------------------------------------*
       3290-PRORATE-STATE-TAX.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PRLM-STATE-TAX-AMT ROUNDED =
                   PRLM-FED-TAX-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PRLM-STATE-TAX-AMT
           END-IF
           ADD PRLM-STATE-TAX-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FICA AMT IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3300-LIMIT-FICA.
           IF PRLT-FICA-AMT < 10.00
               MOVE 10.00 TO PRLT-FICA-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PRLT-FICA-AMT > 1000.00
               MOVE 1000.00 TO PRLT-FICA-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
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
           IF WS-DAYS > 30
               MOVE 'Y' TO PRLM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DEDUCT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FICA YTD.
      *----------------------------------------------------------------*
       3320-PRORATE-FICA.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PRLM-FICA-YTD ROUNDED =
                   PRLM-DEDUCT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PRLM-FICA-YTD
           END-IF
           ADD PRLM-FICA-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE FICA TIER TIER FOR THE FICA YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3330-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-FICA-TIER-RATE(1) TO PRLM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF PRLM-FICA-YTD NOT < WS-FICA-TIER-LOW(WS-TX)
                   MOVE WS-FICA-TIER-RATE(WS-TX) TO PRLM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PRLM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PRLM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NET YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3340-FLAG-NET.
           IF PRLM-NET-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PRLM-NET-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * GROSS AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3350-FLAG-GROSS.
           IF PRLM-GROSS-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PRLM-GROSS-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FICA AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3360-ACCRUE-FICA.
           IF PRLT-FICA-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PRLT-FICA-AMT * PRLM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO PRLM-FICA-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FICA SCHED TIER FOR THE FED TAX YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3370-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FICA-SCHED-RATE(1) TO PRLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF PRLM-FED-TAX-YTD NOT < WS-FICA-SCHED-LOW(WS-TX)
                   MOVE WS-FICA-SCHED-RATE(WS-TX) TO PRLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PRLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PRLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STATE TAX YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3380-ACCRUE-STATE-TAX.
           IF PRLM-STATE-TAX-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PRLM-STATE-TAX-YTD * PRLM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO PRLM-STATE-TAX-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3390-DESCRIBE-REGION.
           EVALUATE PRLM-REGION-CD
               WHEN 'DP'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'TU'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'WD'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE NET FEE ON THE FICA AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3400-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN PRLM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO PRLT-NET-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PRLM-PREFERRED-FLAG = 'Y'
                   COMPUTE PRLT-NET-AMT ROUNDED = PRLT-FICA-AMT *
                       PRLM-FEE-PCT / 200
               WHEN PRLM-REGION-CD = 'DP'
                   MOVE 5.00 TO PRLT-NET-AMT
               WHEN PRLM-REGION-CD = 'TU'
                   MOVE 15.00 TO PRLT-NET-AMT
               WHEN OTHER
                   COMPUTE PRLT-NET-AMT ROUNDED = PRLT-FICA-AMT *
                       PRLM-FEE-PCT / 100
           END-EVALUATE
           IF PRLT-NET-AMT > 0 AND PRLT-NET-AMT < 5.00
               MOVE 5.00 TO PRLT-NET-AMT
           END-IF
           IF PRLT-NET-AMT > 75.00
               MOVE 75.00 TO PRLT-NET-AMT
           END-IF
           ADD PRLT-NET-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * NY -> PL WHEN EXPIRY DATE IS PAST
      * PL -> VK WHEN EXPIRY DATE IS PAST
      * VK -> WB WHEN EXPIRY DATE IS PAST
      * WB -> XL WHEN EXPIRY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3410-SET-SOURCE.
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
      * DAYS SINCE OPEN DATE. MORE THAN 30 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3420-CHECK-OPEN.
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
           IF WS-DAYS > 30
               MOVE 'Y' TO PRLT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3430-DESCRIBE-REGION.
           EVALUATE PRLM-REGION-CD
               WHEN 'DP'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'TU'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'WD'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3440-AGE-CLOSE.
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
                   ADD PRLM-FICA-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PRLM-FICA-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD PRLM-FICA-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PRLM-FICA-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PRLM-FICA-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE GROSS AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3450-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN PRLM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PRLT-REVERSAL-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = PRLM-GROSS-AMT *
                       PRLM-MARGIN / 200
               WHEN PRLT-TRAN-CD = 'PR'
                   MOVE 15.00 TO WS-WORK-AMT-2
               WHEN PRLT-TRAN-CD = 'AD'
                   MOVE 5.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = PRLM-GROSS-AMT *
                       PRLM-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 5.00
               MOVE 5.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 35.00
               MOVE 35.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FICA YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3460-FLAG-FICA.
           IF PRLM-FICA-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PRLM-FICA-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FICA YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3470-SPLIT-FICA.
           COMPUTE WS-WORK-AMT = PRLM-FICA-YTD / 2
           COMPUTE WS-WORK-AMT-2 = PRLM-FICA-YTD - (WS-WORK-AMT * 2)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 180 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3480-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF PRLT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PRLT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO PRLM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3490-SPLIT-FICA.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 2)
           COMPUTE PRLM-FICA-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FICA AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3500-SPLIT-FICA.
           COMPUTE WS-WORK-AMT = PRLM-FICA-AMT / 3
           COMPUTE WS-WORK-AMT-2 = PRLM-FICA-AMT - (WS-WORK-AMT * 3)
           COMPUTE PRLT-FICA-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE NET YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO GROSS YTD.
      *----------------------------------------------------------------*
       3510-PRORATE-GROSS.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PRLM-GROSS-YTD ROUNDED =
                   PRLM-NET-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PRLM-GROSS-YTD
           END-IF
           ADD PRLM-GROSS-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * DP -> TU WHEN LAST ACT DATE IS PAST
      * TU -> WD WHEN LAST ACT DATE IS PAST
      * WD -> DP WHEN LAST ACT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3520-SET-REGION.
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
      * ASSESS THE STATE TAX FEE ON THE FED TAX YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3530-ASSESS-STOP.
           EVALUATE TRUE
               WHEN PRLM-STOP-FLAG = 'Y'
                   MOVE ZERO TO PRLM-STATE-TAX-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PRLT-REVERSAL-FLAG = 'Y'
                   COMPUTE PRLM-STATE-TAX-AMT ROUNDED = PRLM-FED-TAX-YTD
                       * PRLM-MARGIN / 200
               WHEN PRLM-SOURCE-CD = 'NY'
                   MOVE 3.00 TO PRLM-STATE-TAX-AMT
               WHEN PRLM-SOURCE-CD = 'PL'
                   MOVE 15.00 TO PRLM-STATE-TAX-AMT
               WHEN OTHER
                   COMPUTE PRLM-STATE-TAX-AMT ROUNDED = PRLM-FED-TAX-YTD
                       * PRLM-MARGIN / 100
           END-EVALUATE
           IF PRLM-STATE-TAX-AMT > 0 AND PRLM-STATE-TAX-AMT < 5.00
               MOVE 5.00 TO PRLM-STATE-TAX-AMT
           END-IF
           IF PRLM-STATE-TAX-AMT > 50.00
               MOVE 50.00 TO PRLM-STATE-TAX-AMT
           END-IF
           ADD PRLM-STATE-TAX-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E703 REF NO REQUIRED
      * E765 FICA YTD NEGATIVE
      * E043 FICA YTD OVER 1000000
      * E314 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3540-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PRLM-REF-NO = SPACES
                   MOVE 'E703' TO WS-REJECT-CODE
               WHEN PRLM-FICA-YTD < 0
                   MOVE 'E765' TO WS-REJECT-CODE
               WHEN PRLM-FICA-YTD > 1000000.00
                   MOVE 'E043' TO WS-REJECT-CODE
               WHEN PRLM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E314' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FICA AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3550-FLAG-FICA.
           IF PRLT-FICA-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PRLT-FICA-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE STATE TAX YTD BY PAY FREQ CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3560-ACCUM-PAY-FREQ.
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
      * DAYS SINCE LAST PAY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3570-CHECK-LAST-PAY.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO PRLM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 180 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3580-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF PRLT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PRLT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO PRLM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FED TAX YTD IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3590-LIMIT-FED-TAX.
           IF PRLM-FED-TAX-YTD < 10.00
               MOVE 10.00 TO PRLM-FED-TAX-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PRLM-FED-TAX-YTD > 1000.00
               MOVE 1000.00 TO PRLM-FED-TAX-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3600-DESCRIBE-REASON.
           EVALUATE PRLM-REASON-CD
               WHEN 'B'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'C'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'G'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'H'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'L'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'T'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'X'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 30 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3610-CHECK-NEXT-REVIEW.
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
               MOVE 'Y' TO PRLM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3620-LIMIT-ITEM.
           IF WS-WORK-AMT < 50.00
               MOVE 50.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 50000.00
               MOVE 50000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FED TAX AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3630-ACCUM-CHANNEL.
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
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3640-AGE-EFFECTIVE.
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
                   ADD PRLM-GROSS-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD PRLM-GROSS-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD PRLM-GROSS-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD PRLM-GROSS-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PRLM-GROSS-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT FICA YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3650-SPLIT-FICA.
           COMPUTE WS-WORK-AMT = PRLM-FICA-YTD / 3
           COMPUTE WS-WORK-AMT-2 = PRLM-FICA-YTD - (WS-WORK-AMT * 3)
           COMPUTE PRLM-FED-TAX-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E836 REF NO REQUIRED
      * E911 FED TAX AMT NEGATIVE
      * E326 FED TAX AMT OVER 25000
      * E620 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3660-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PRLM-REF-NO = SPACES
                   MOVE 'E836' TO WS-REJECT-CODE
               WHEN PRLM-FED-TAX-AMT < 0
                   MOVE 'E911' TO WS-REJECT-CODE
               WHEN PRLM-FED-TAX-AMT > 25000.00
                   MOVE 'E326' TO WS-REJECT-CODE
               WHEN PRLM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E620' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DEDUCT BAND TIER FOR THE FED TAX YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3670-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-DEDUCT-BAND-RATE(1) TO PRLM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF PRLM-FED-TAX-YTD NOT < WS-DEDUCT-BAND-LOW(WS-TX)
                   MOVE WS-DEDUCT-BAND-RATE(WS-TX) TO PRLM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PRLM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PRLM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STATE TAX AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3680-ACCRUE-STATE-TAX.
           IF PRLT-STATE-TAX-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PRLT-STATE-TAX-AMT * PRLM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO PRLM-GROSS-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * GROSS AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3690-FLAG-GROSS.
           IF PRLM-GROSS-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PRLM-GROSS-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3700-DESCRIBE-SOURCE.
           EVALUATE PRLM-SOURCE-CD
               WHEN 'NY'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'PL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'VK'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'WB'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'XL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE DEDUCT YTD FEE ON THE FICA YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3710-ASSESS-STOP.
           EVALUATE TRUE
               WHEN PRLM-STOP-FLAG = 'Y'
                   MOVE ZERO TO PRLM-DEDUCT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PRLM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE PRLM-DEDUCT-YTD ROUNDED = PRLM-FICA-YTD *
                       PRLM-MARGIN / 200
               WHEN PRLM-PAY-FREQ-CD = 'JT'
                   MOVE 15.00 TO PRLM-DEDUCT-YTD
               WHEN PRLM-PAY-FREQ-CD = 'XM'
                   MOVE 12.00 TO PRLM-DEDUCT-YTD
               WHEN OTHER
                   COMPUTE PRLM-DEDUCT-YTD ROUNDED = PRLM-FICA-YTD *
                       PRLM-MARGIN / 100
           END-EVALUATE
           IF PRLM-DEDUCT-YTD > 0 AND PRLM-DEDUCT-YTD < 5.00
               MOVE 5.00 TO PRLM-DEDUCT-YTD
           END-IF
           IF PRLM-DEDUCT-YTD > 50.00
               MOVE 50.00 TO PRLM-DEDUCT-YTD
           END-IF
           ADD PRLM-DEDUCT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3720-AGE-LAST-ACT.
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
                   ADD PRLM-FED-TAX-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD PRLM-FED-TAX-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD PRLM-FED-TAX-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD PRLM-FED-TAX-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PRLM-FED-TAX-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3730-ACCRUE-RATE.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * PRLM-RATE / 100 / 360
               ADD WS-WORK-AMT TO PRLT-DEDUCT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DEDUCT GRID TIER FOR THE STATE TAX AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3740-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DEDUCT-GRID-RATE(1) TO PRLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF PRLT-STATE-TAX-AMT NOT < WS-DEDUCT-GRID-LOW(WS-TX)
                   MOVE WS-DEDUCT-GRID-RATE(WS-TX) TO PRLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PRLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PRLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 15 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3750-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF PRLT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PRLT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO PRLM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE NET YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO DEDUCT YTD.
      *----------------------------------------------------------------*
       3760-PRORATE-DEDUCT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PRLM-DEDUCT-YTD ROUNDED =
                   PRLM-NET-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PRLM-DEDUCT-YTD
           END-IF
           ADD PRLM-DEDUCT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3770-SPLIT-DEDUCT.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 2)
           COMPUTE PRLM-DEDUCT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3780-COUNT-SEQ.
           IF PRLT-SEQ < 9999999
               ADD 1 TO PRLT-SEQ
           END-IF
           IF PRLT-SEQ NOT < 5
               MOVE 'Y' TO PRLM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE NET FEE ON THE FED TAX YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3790-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN PRLM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO PRLT-NET-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PRLT-REVERSAL-FLAG = 'Y'
                   COMPUTE PRLT-NET-AMT ROUNDED = PRLM-FED-TAX-YTD *
                       PRLM-RATE / 200
               WHEN PRLM-SOURCE-CD = 'NY'
                   MOVE 12.00 TO PRLT-NET-AMT
               WHEN PRLM-SOURCE-CD = 'PL'
                   MOVE 15.00 TO PRLT-NET-AMT
               WHEN OTHER
                   COMPUTE PRLT-NET-AMT ROUNDED = PRLM-FED-TAX-YTD *
                       PRLM-RATE / 100
           END-EVALUATE
           IF PRLT-NET-AMT > 0 AND PRLT-NET-AMT < 5.00
               MOVE 5.00 TO PRLT-NET-AMT
           END-IF
           IF PRLT-NET-AMT > 150.00
               MOVE 150.00 TO PRLT-NET-AMT
           END-IF
           ADD PRLT-NET-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3800-AGE-NOTICE.
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
                   ADD PRLM-STATE-TAX-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD PRLM-STATE-TAX-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD PRLM-STATE-TAX-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PRLM-STATE-TAX-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PRLM-STATE-TAX-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3810-COUNT-SEQ.
           IF PRLT-SEQ < 9999999
               ADD 1 TO PRLT-SEQ
           END-IF
           IF PRLT-SEQ NOT < 12
               MOVE 'Y' TO PRLM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 365 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3820-CHECK-NEXT-REVIEW.
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
           IF WS-DAYS > 365
               MOVE 'Y' TO PRLM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FED TAX YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3830-SPLIT-FED-TAX.
           COMPUTE WS-WORK-AMT = PRLM-FED-TAX-YTD / 4
           COMPUTE WS-WORK-AMT-2 = PRLM-FED-TAX-YTD - (WS-WORK-AMT * 4)
           COMPUTE PRLM-DEDUCT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3840-DESCRIBE-STATUS.
           EVALUATE PRLM-STATUS-CD
               WHEN 'MV'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'RD'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'ST'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'TJ'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'UC'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 10 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3850-CHECK-OPEN.
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
               MOVE 'Y' TO PRLM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DEDUCT AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3860-SPLIT-DEDUCT.
           COMPUTE WS-WORK-AMT = PRLM-DEDUCT-AMT / 2
           COMPUTE WS-WORK-AMT-2 = PRLM-DEDUCT-AMT - (WS-WORK-AMT * 2)
           COMPUTE PRLM-FED-TAX-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE FICA YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3870-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN PRLM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PRLM-STOP-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = PRLM-FICA-YTD *
                       PRLM-FEE-PCT / 200
               WHEN PRLM-TAX-STATE-CD = 'CAZ'
                   MOVE 12.00 TO WS-WORK-AMT-2
               WHEN PRLM-TAX-STATE-CD = 'LZU'
                   MOVE 20.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = PRLM-FICA-YTD *
                       PRLM-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 1.00
               MOVE 1.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 150.00
               MOVE 150.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3880-AGE-CLOSE.
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
                   ADD PRLT-FICA-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD PRLT-FICA-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD PRLT-FICA-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD PRLT-FICA-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PRLT-FICA-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3890-CHECK-LAST-PAY.
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
           IF WS-DAYS > 30
               MOVE 'Y' TO PRLM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON GROSS AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3900-ACCRUE-GROSS.
           IF PRLM-GROSS-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PRLM-GROSS-AMT * PRLM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO PRLT-STATE-TAX-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * B -> C WHEN EFF DATE IS PAST
      * C -> G WHEN EFF DATE IS PAST
      * G -> H WHEN EFF DATE IS PAST
      * H -> L WHEN EFF DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3910-SET-REASON.
           EVALUATE PRLM-REASON-CD
               WHEN 'B'
                   IF PRLT-EFF-DATE > 0 AND PRLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'C' TO PRLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'C'
                   IF PRLT-EFF-DATE > 0 AND PRLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'G' TO PRLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'G'
                   IF PRLT-EFF-DATE > 0 AND PRLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO PRLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF PRLT-EFF-DATE > 0 AND PRLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO PRLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NET AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3920-FLAG-NET.
           IF PRLM-NET-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PRLM-NET-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3930-DESCRIBE-CHANNEL.
           EVALUATE PRLT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE DEDUCT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO GROSS AMT.
      *----------------------------------------------------------------*
       3940-PRORATE-GROSS.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PRLM-GROSS-AMT ROUNDED =
                   PRLM-DEDUCT-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PRLM-GROSS-AMT
           END-IF
           ADD PRLM-GROSS-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3950-COUNT-PAYMENT.
           IF PRLM-PAYMENT-CNT < 9999999
               ADD 1 TO PRLM-PAYMENT-CNT
           END-IF
           IF PRLM-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO PRLM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3960-CHECK-NEXT-DUE.
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
               MOVE 'Y' TO PRLM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3970-CHECK-NEXT-DUE.
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
           IF WS-DAYS > 90
               MOVE 'Y' TO PRLM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT NET AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3980-SPLIT-NET.
           COMPUTE WS-WORK-AMT = PRLM-NET-AMT / 12
           COMPUTE WS-WORK-AMT-2 = PRLM-NET-AMT - (WS-WORK-AMT * 12)
           COMPUTE PRLM-GROSS-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * PR -> AD WHEN EXPIRY DATE IS PAST
      * AD -> VD WHEN EXPIRY DATE IS PAST
      * VD -> TX WHEN EXPIRY DATE IS PAST
      * TX -> DP WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3990-SET-TRAN.
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
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4000-AGE-LAST-PAY.
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
                   ADD PRLM-GROSS-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD PRLM-GROSS-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD PRLM-GROSS-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PRLM-GROSS-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PRLM-GROSS-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 120 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4010-CHECK-NEXT-REVIEW.
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
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4020-DESCRIBE-REASON.
           EVALUATE PRLM-REASON-CD
               WHEN 'B'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'C'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'G'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'H'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'L'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'T'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'X'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE FED TAX YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FICA AMT.
      *----------------------------------------------------------------*
       4030-PRORATE-FICA.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PRLT-FICA-AMT ROUNDED =
                   PRLM-FED-TAX-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PRLT-FICA-AMT
           END-IF
           ADD PRLT-FICA-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       4040-COUNT-PAYMENT.
           IF PRLM-PAYMENT-CNT < 9999999
               ADD 1 TO PRLM-PAYMENT-CNT
           END-IF
           IF PRLM-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO PRLM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-150 AND OVER 150 DAYS.
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
                   ADD PRLM-GROSS-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD PRLM-GROSS-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD PRLM-GROSS-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PRLM-GROSS-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PRLM-GROSS-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE FICA TIER TIER FOR THE FED TAX AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4060-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-FICA-TIER-RATE(1) TO PRLM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF PRLM-FED-TAX-AMT NOT < WS-FICA-TIER-LOW(WS-TX)
                   MOVE WS-FICA-TIER-RATE(WS-TX) TO PRLM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PRLM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PRLM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       4070-COUNT-CYCLE.
           IF PRLM-CYCLE-CNT < 9999999
               ADD 1 TO PRLM-CYCLE-CNT
           END-IF
           IF PRLM-CYCLE-CNT NOT < 10
               MOVE 'Y' TO PRLM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DEDUCT GRID TIER FOR THE DEDUCT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4080-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DEDUCT-GRID-RATE(1) TO PRLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF PRLM-DEDUCT-AMT NOT < WS-DEDUCT-GRID-LOW(WS-TX)
                   MOVE WS-DEDUCT-GRID-RATE(WS-TX) TO PRLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PRLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PRLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4090-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PRLM-REASON-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PRLM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DEDUCT AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4100-ACCRUE-DEDUCT.
           IF PRLT-DEDUCT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PRLT-DEDUCT-AMT * PRLM-RATE / 100 / 360
               ADD WS-WORK-AMT TO PRLM-DEDUCT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4110-DESCRIBE-REASON.
           EVALUATE PRLM-REASON-CD
               WHEN 'B'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'C'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'G'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'H'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'L'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'T'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'X'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4120-DESCRIBE-STATUS.
           EVALUATE PRLM-STATUS-CD
               WHEN 'MV'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'RD'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ST'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'TJ'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'UC'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4130-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 25.00
               MOVE 25.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 5000.00
               MOVE 5000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DEDUCT BAND TIER FOR THE FICA YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4140-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DEDUCT-BAND-RATE(1) TO PRLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF PRLM-FICA-YTD NOT < WS-DEDUCT-BAND-LOW(WS-TX)
                   MOVE WS-DEDUCT-BAND-RATE(WS-TX) TO PRLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PRLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PRLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DEDUCT BAND TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4150-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-DEDUCT-BAND-RATE(1) TO PRLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF WS-WORK-AMT NOT < WS-DEDUCT-BAND-LOW(WS-TX)
                   MOVE WS-DEDUCT-BAND-RATE(WS-TX) TO PRLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PRLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PRLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4160-DESCRIBE-REGION.
           EVALUATE PRLM-REGION-CD
               WHEN 'DP'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'TU'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'WD'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE FICA AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4170-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PRLM-REGION-CD
                   ADD PRLT-FICA-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PRLM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PRLT-FICA-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * B -> C WHEN OPEN DATE IS PAST
      * C -> G WHEN OPEN DATE IS PAST
      * G -> H WHEN OPEN DATE IS PAST
      * H -> L WHEN OPEN DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4180-SET-REASON.
           EVALUATE PRLM-REASON-CD
               WHEN 'B'
                   IF PRLM-OPEN-DATE > 0 AND PRLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'C' TO PRLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'C'
                   IF PRLM-OPEN-DATE > 0 AND PRLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'G' TO PRLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'G'
                   IF PRLM-OPEN-DATE > 0 AND PRLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO PRLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF PRLM-OPEN-DATE > 0 AND PRLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO PRLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NET AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4190-FLAG-NET.
           IF PRLT-NET-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PRLT-NET-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FICA AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4200-ACCRUE-FICA.
           IF PRLM-FICA-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PRLM-FICA-AMT * PRLM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 60 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4210-CHECK-LAST-ACT.
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
      * SELECT THE FICA SCHED TIER FOR THE STATE TAX YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4220-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-FICA-SCHED-RATE(1) TO PRLM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF PRLM-STATE-TAX-YTD NOT < WS-FICA-SCHED-LOW(WS-TX)
                   MOVE WS-FICA-SCHED-RATE(WS-TX) TO PRLM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PRLM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PRLM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DEDUCT AMT IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4230-LIMIT-DEDUCT.
           IF PRLT-DEDUCT-AMT < 10.00
               MOVE 10.00 TO PRLT-DEDUCT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PRLT-DEDUCT-AMT > 5000.00
               MOVE 5000.00 TO PRLT-DEDUCT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATE TAX AMT IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4240-LIMIT-STATE-TAX.
           IF PRLT-STATE-TAX-AMT < 25.00
               MOVE 25.00 TO PRLT-STATE-TAX-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PRLT-STATE-TAX-AMT > 2500.00
               MOVE 2500.00 TO PRLT-STATE-TAX-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DEDUCT GRID TIER FOR THE DEDUCT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4250-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-DEDUCT-GRID-RATE(1) TO PRLM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF PRLM-DEDUCT-YTD NOT < WS-DEDUCT-GRID-LOW(WS-TX)
                   MOVE WS-DEDUCT-GRID-RATE(WS-TX) TO PRLM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PRLM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PRLM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FED TAX AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4260-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PRLM-REGION-CD
                   ADD PRLM-FED-TAX-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PRLM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PRLM-FED-TAX-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
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
                   MOVE PYFEE-DEDUCT-AMT
                     TO PRLM-DEDUCT-AMT
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

      *----------------------------------------------------------------*
      * PASS THE PRL FIELDS TO PYTIER AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7020-CALL-PYTIER.
           INITIALIZE PYTIER-PARMS
           MOVE PRLM-DEDUCT-AMT
             TO PYTIER-DEDUCT-AMT
           MOVE PRLM-NET-YTD
             TO PYTIER-NET-YTD
           MOVE PRLM-STATE-TAX-AMT
             TO PYTIER-STATE-TAX-AMT
           MOVE PRLM-NET-AMT
             TO PYTIER-NET-AMT
           MOVE PRLM-GROSS-AMT
             TO PYTIER-GROSS-AMT
           MOVE PRLM-FED-TAX-AMT
             TO PYTIER-FED-TAX-AMT
           MOVE PRLM-NEXT-DUE-DATE
             TO PYTIER-NEXT-DUE-DATE
           MOVE PRLM-EXPIRY-DATE
             TO PYTIER-EXPIRY-DATE
           MOVE PRLM-CLOSE-DATE
             TO PYTIER-CLOSE-DATE
           MOVE PRLM-OPEN-DATE
             TO PYTIER-OPEN-DATE
           MOVE PRLM-PAY-FREQ-CD
             TO PYTIER-PAY-FREQ-CD
           MOVE PRLM-TAX-STATE-CD
             TO PYTIER-TAX-STATE-CD
           MOVE PRLM-STATUS-CD
             TO PYTIER-STATUS-CD
           MOVE PRLM-REASON-CD
             TO PYTIER-REASON-CD
           MOVE PRLM-SUSPEND-FLAG
             TO PYTIER-SUSPEND-FLAG
           MOVE PRLM-HOLD-FLAG
             TO PYTIER-HOLD-FLAG
           MOVE PRLM-PREFERRED-FLAG
             TO PYTIER-PREFERRED-FLAG
           MOVE PRLM-ITEM-CNT
             TO PYTIER-ITEM-CNT
           MOVE PRLM-PAYMENT-CNT
             TO PYTIER-PAYMENT-CNT
           MOVE PRLM-RATE
             TO PYTIER-RATE
           MOVE PRLM-FEE-PCT
             TO PYTIER-FEE-PCT
           MOVE PRLM-DESC
             TO PYTIER-DESC
           MOVE PRLM-CONTACT
             TO PYTIER-CONTACT
           CALL 'PYTIER' USING PYTIER-PARMS
           EVALUATE PYTIER-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE PYTIER-NET-YTD
                     TO PRLM-NET-YTD
                   MOVE PYTIER-NET-AMT
                     TO PRLM-NET-AMT
                   MOVE PYTIER-SUSPEND-FLAG
                     TO PRLM-SUSPEND-FLAG
                   MOVE PYTIER-HOLD-FLAG
                     TO PRLM-HOLD-FLAG
                   MOVE PYTIER-PREFERRED-FLAG
                     TO PRLM-PREFERRED-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RIER' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-PYTIER' TO ABND-PARAGRAPH
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
           DISPLAY 'PYLRCON READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'PYLRCON' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
