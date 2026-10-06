      *================================================================*
      * PROGRAM  : PYLNOTC                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : PAYROLL SERVICES - CUSTOMER NOTICES. READS THE PRL  *
      *            TRANSACTIONS, UPDATES THE PRL MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. CLIENT PAYROLL CALCULATION, TAX *
      *            DEPOSIT AND DIRECT DEPOSIT                          *
      * INPUT    : PRLMST (KSDS), PRLTRN, BANKCTL                      *
      * OUTPUT   : PRLOUT, PYLNORPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, PYPRC, PYFEE, PYTIER    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1989-02-05 CRP  ORIGINAL                                       *
      * 2001-12-23 DPS  STATE-TAX RERUN PROTECTION PRB7177             *
      * 2002-12-12 EWS  PERFORMANCE - GROSS READ AHEAD INC5432         *
      * 2007-03-13 DPS  STATE-TAX EXCEPTIONS TO OPS QUEUE PRB8640      *
      * 2011-06-15 RJT  NET CALCULATION CORRECTED INC0787              *
      * 2011-10-09 JLM  NET CALCULATION CORRECTED CHG8550              *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PYLNOTC.
       AUTHOR. J L MARSH.

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
           SELECT REPORT-FILE ASSIGN TO "PYLNORPT"
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
                                           VALUE 19.000000.
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
      * WS-DEDUCT-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-DEDUCT-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000780500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000208500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001100000000643500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001150000000135000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003650000000723000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004150000000467500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006650000000401000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006900000000218000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007900000000832500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008900000000465000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00009150000000653000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00009200000000476000'.
       01  WS-DEDUCT-TIER-TBL REDEFINES WS-DEDUCT-TIER-VALUES.
           05  WS-DEDUCT-TIER-ROW OCCURS 12 TIMES.
               10  WS-DEDUCT-TIER-LOW      PIC 9(09)V99.
               10  WS-DEDUCT-TIER-RATE     PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-FICA-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FICA-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000106500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000482000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010100000000071000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015100000000512000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015600000000803000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015850000000751000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015900000000293000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016150000000842500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016400000000108000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016900000000204000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017150000000230000'.
       01  WS-FICA-GRID-TBL REDEFINES WS-FICA-GRID-VALUES.
           05  WS-FICA-GRID-ROW OCCURS 11 TIMES.
               10  WS-FICA-GRID-LOW        PIC 9(09)V99.
               10  WS-FICA-GRID-RATE       PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  PYLNOTC  PAYROLL SERVICES'.
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
       COPY PYPRCP.
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
               PERFORM 3000-DESCRIBE-PAY-FREQ
               PERFORM 3010-LIMIT-FED-TAX
               PERFORM 3020-EDIT-REF-NO
               PERFORM 3030-SET-REGION
               PERFORM 3040-LIMIT-FICA
               PERFORM 3050-SELECT-RATE
               PERFORM 3060-ACCRUE-NET
               PERFORM 3070-SELECT-RATE
               PERFORM 3080-SPLIT-FICA
               PERFORM 3090-ACCUM-TRAN
               PERFORM 3100-PRORATE-FICA
               PERFORM 3110-DESCRIBE-TAX-STATE
               PERFORM 3120-PRORATE-STATE-TAX
               PERFORM 3130-ASSESS-AUTO-RENEW
               PERFORM 3140-CHECK-OPEN
               PERFORM 3150-COUNT-SEQ
               PERFORM 3160-EDIT-REF-NO
               PERFORM 3170-SPLIT-NET
               PERFORM 3180-LIMIT-STATE-TAX
               PERFORM 3190-PRORATE-FED-TAX
               PERFORM 3200-DESCRIBE-SOURCE
               PERFORM 3210-SET-CHANNEL
               PERFORM 3220-ASSESS-SUSPEND
               PERFORM 3230-ACCRUE-NET
               PERFORM 3240-SET-PAY-FREQ
               PERFORM 3250-DESCRIBE-REASON
               PERFORM 3260-ACCUM-CHANNEL
               PERFORM 3270-FLAG-ITEM
               PERFORM 3280-PRORATE-FED-TAX
               PERFORM 3290-CHECK-EXPIRY
               PERFORM 3300-SPLIT-STATE-TAX
               PERFORM 3310-FLAG-NET
               PERFORM 3320-SPLIT-FED-TAX
               PERFORM 3330-DESCRIBE-STATUS
               PERFORM 3340-SELECT-MARGIN
               PERFORM 3350-PRORATE-FICA
               PERFORM 3360-AGE-OPEN
               PERFORM 3370-ASSESS-NOTICE-SENT
               PERFORM 3380-FLAG-FICA
               PERFORM 3390-DESCRIBE-REGION
               PERFORM 3400-PRORATE-NET
               PERFORM 3410-ACCRUE-DEDUCT
               PERFORM 3420-ASSESS-NOTICE-SENT
               PERFORM 3430-LIMIT-GROSS
               PERFORM 3440-PRORATE-FED-TAX
               PERFORM 3450-AGE-NOTICE
               PERFORM 3460-ASSESS-REVERSAL
               PERFORM 3470-CHECK-NOTICE
               PERFORM 3480-SET-REASON
               PERFORM 3490-SPLIT-FED-TAX
               PERFORM 3500-DESCRIBE-PAY-FREQ
               PERFORM 3510-DESCRIBE-REASON
               PERFORM 3520-ACCRUE-STATE-TAX
               PERFORM 3530-PRORATE-FICA
               PERFORM 3540-AGE-EXPIRY
               PERFORM 3550-AGE-POST
               PERFORM 3560-ACCUM-REGION
               PERFORM 3570-FLAG-DEDUCT
               PERFORM 3580-ASSESS-REVERSAL
               PERFORM 3590-PRORATE-STATE-TAX
               PERFORM 3600-PRORATE-NET
               PERFORM 3610-SELECT-RATE
               PERFORM 3620-ACCRUE-RATE
               PERFORM 3630-PRORATE-FED-TAX
               PERFORM 3640-AGE-CLOSE
               PERFORM 3650-SPLIT-STATE-TAX
               PERFORM 3660-COUNT-SEQ
               PERFORM 3670-EDIT-ADDR
               PERFORM 3680-COUNT-PAYMENT
               PERFORM 3690-DESCRIBE-TRAN
               PERFORM 3700-LIMIT-FICA
               PERFORM 3710-COUNT-NOTICE
               PERFORM 3720-CHECK-CLOSE
               PERFORM 3730-EDIT-NAME
               PERFORM 3740-SET-TRAN
               PERFORM 3750-COUNT-NOTICE
               PERFORM 3760-ACCRUE-FEE-PCT
               PERFORM 3770-FLAG-STATE-TAX
               PERFORM 3780-COUNT-ITEM
               PERFORM 3790-PRORATE-NET
               PERFORM 3800-SELECT-RATE
               PERFORM 3810-SET-REASON
               PERFORM 3820-LIMIT-FED-TAX
               PERFORM 3830-PRORATE-NET
               PERFORM 3840-CHECK-LAST-ACT
               PERFORM 3850-ASSESS-HOLD
               PERFORM 3860-AGE-EFF
               PERFORM 3870-FLAG-GROSS
               PERFORM 3880-CHECK-EXPIRY
               PERFORM 3890-LIMIT-NET
               PERFORM 3900-DESCRIBE-PAY-FREQ
               PERFORM 3910-LIMIT-DEDUCT
               PERFORM 3920-CHECK-EFFECTIVE
               PERFORM 3930-ACCUM-PAY-FREQ
               PERFORM 3940-EDIT-ADDR
               PERFORM 3950-CHECK-CLOSE
               PERFORM 3960-EDIT-DESC
               PERFORM 3970-PRORATE-NET
               PERFORM 3980-FLAG-DEDUCT
               PERFORM 3990-DESCRIBE-PAY-FREQ
               PERFORM 4000-LIMIT-STATE-TAX
               PERFORM 4010-AGE-LAST-PAY
               PERFORM 4020-SET-TAX-STATE
               PERFORM 4030-FLAG-STATE-TAX
               PERFORM 4040-EDIT-ADDR
               PERFORM 4050-FLAG-FICA
               PERFORM 4060-SET-TRAN
               PERFORM 4070-ACCUM-STATUS
               PERFORM 4080-ASSESS-STOP
               PERFORM 4090-CHECK-LAST-PAY
               PERFORM 4100-ACCUM-TAX-STATE
               PERFORM 4110-PRORATE-NET
               PERFORM 4120-FLAG-NET
               PERFORM 4130-DESCRIBE-CHANNEL
               PERFORM 4140-EDIT-CUST-ID
               PERFORM 4150-LIMIT-DEDUCT
               PERFORM 4160-ACCUM-REGION
               PERFORM 4170-ACCRUE-STATE-TAX
               PERFORM 4180-COUNT-PAYMENT
               PERFORM 4190-ACCUM-TRAN
               PERFORM 4200-EDIT-CONTACT
               PERFORM 4210-PRORATE-FICA
               PERFORM 4220-LIMIT-DEDUCT
               PERFORM 4230-LIMIT-STATE-TAX
               PERFORM 4240-FLAG-DEDUCT
               PERFORM 4250-ACCRUE-FED-TAX
               PERFORM 4260-DESCRIBE-CHANNEL
               PERFORM 4270-ACCUM-TAX-STATE
               PERFORM 4280-ACCRUE-GROSS
               PERFORM 4290-ACCUM-TRAN
               PERFORM 4300-SET-SOURCE
               PERFORM 4310-SET-PAY-FREQ
               PERFORM 4320-COUNT-NOTICE
               PERFORM 4330-LIMIT-NET
               PERFORM 4340-AGE-EFF
               PERFORM 4350-ASSESS-SUSPEND
               PERFORM 4360-ACCRUE-NET
               PERFORM 4370-ACCRUE-STATE-TAX
               PERFORM 4380-EDIT-REF-NO
               PERFORM 4390-LIMIT-FICA
               PERFORM 4400-ASSESS-HOLD
               PERFORM 4410-PRORATE-NET
               PERFORM 4420-SELECT-MARGIN
               PERFORM 4430-ACCUM-CHANNEL
               PERFORM 4440-AGE-NEXT-DUE
               PERFORM 4450-SPLIT-FED-TAX
               PERFORM 4460-SELECT-FEE-PCT
               PERFORM 4470-EDIT-DESC
               PERFORM 7000-CALL-PYPRC
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
      * TRANSLATE PAY FREQ CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3000-DESCRIBE-PAY-FREQ.
           EVALUATE PRLM-PAY-FREQ-CD
               WHEN 'JT'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'XM'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'ZT'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FED TAX YTD IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3010-LIMIT-FED-TAX.
           IF PRLM-FED-TAX-YTD < 25.00
               MOVE 25.00 TO PRLM-FED-TAX-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PRLM-FED-TAX-YTD > 50000.00
               MOVE 50000.00 TO PRLM-FED-TAX-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E740 REF NO REQUIRED
      * E919 STATE TAX AMT NEGATIVE
      * E299 STATE TAX AMT OVER 10000
      * E262 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3020-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PRLM-REF-NO = SPACES
                   MOVE 'E740' TO WS-REJECT-CODE
               WHEN PRLT-STATE-TAX-AMT < 0
                   MOVE 'E919' TO WS-REJECT-CODE
               WHEN PRLT-STATE-TAX-AMT > 10000.00
                   MOVE 'E299' TO WS-REJECT-CODE
               WHEN PRLT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E262' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * DP -> TU WHEN POST DATE IS PAST
      * TU -> WD WHEN POST DATE IS PAST
      * WD -> DP WHEN POST DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3030-SET-REGION.
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
      * FICA AMT IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3040-LIMIT-FICA.
           IF PRLM-FICA-AMT < 0.00
               MOVE 0.00 TO PRLM-FICA-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PRLM-FICA-AMT > 2500.00
               MOVE 2500.00 TO PRLM-FICA-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FICA GRID TIER FOR THE FED TAX AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3050-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-FICA-GRID-RATE(1) TO PRLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF PRLM-FED-TAX-AMT NOT < WS-FICA-GRID-LOW(WS-TX)
                   MOVE WS-FICA-GRID-RATE(WS-TX) TO PRLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PRLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PRLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON NET AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3060-ACCRUE-NET.
           IF PRLM-NET-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PRLM-NET-AMT * PRLM-RATE / 100 / 360
               ADD WS-WORK-AMT TO PRLM-STATE-TAX-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DEDUCT TIER TIER FOR THE NET YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3070-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-DEDUCT-TIER-RATE(1) TO PRLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF PRLM-NET-YTD NOT < WS-DEDUCT-TIER-LOW(WS-TX)
                   MOVE WS-DEDUCT-TIER-RATE(WS-TX) TO PRLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PRLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PRLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FICA YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3080-SPLIT-FICA.
           COMPUTE WS-WORK-AMT = PRLM-FICA-YTD / 3
           COMPUTE WS-WORK-AMT-2 = PRLM-FICA-YTD - (WS-WORK-AMT * 3)
           COMPUTE PRLT-STATE-TAX-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FICA AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3090-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PRLT-TRAN-CD
                   ADD PRLT-FICA-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PRLT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PRLT-FICA-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE STATE TAX AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FICA YTD.
      *----------------------------------------------------------------*
       3100-PRORATE-FICA.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PRLM-FICA-YTD ROUNDED =
                   PRLT-STATE-TAX-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PRLM-FICA-YTD
           END-IF
           ADD PRLM-FICA-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE TAX STATE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3110-DESCRIBE-TAX-STATE.
           EVALUATE PRLM-TAX-STATE-CD
               WHEN 'CAZ'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'LZU'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'TUF'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'WKE'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE DEDUCT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO STATE TAX AMT.
      *----------------------------------------------------------------*
       3120-PRORATE-STATE-TAX.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PRLT-STATE-TAX-AMT ROUNDED =
                   PRLM-DEDUCT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PRLT-STATE-TAX-AMT
           END-IF
           ADD PRLT-STATE-TAX-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE DEDUCT YTD FEE ON THE NET AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3130-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN PRLM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO PRLM-DEDUCT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PRLM-HOLD-FLAG = 'Y'
                   COMPUTE PRLM-DEDUCT-YTD ROUNDED = PRLT-NET-AMT *
                       PRLM-FEE-PCT / 200
               WHEN PRLM-TAX-STATE-CD = 'CAZ'
                   MOVE 7.00 TO PRLM-DEDUCT-YTD
               WHEN PRLM-TAX-STATE-CD = 'LZU'
                   MOVE 20.00 TO PRLM-DEDUCT-YTD
               WHEN OTHER
                   COMPUTE PRLM-DEDUCT-YTD ROUNDED = PRLT-NET-AMT *
                       PRLM-FEE-PCT / 100
           END-EVALUATE
           IF PRLM-DEDUCT-YTD > 0 AND PRLM-DEDUCT-YTD < 5.00
               MOVE 5.00 TO PRLM-DEDUCT-YTD
           END-IF
           IF PRLM-DEDUCT-YTD > 50.00
               MOVE 50.00 TO PRLM-DEDUCT-YTD
           END-IF
           ADD PRLM-DEDUCT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 60 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3140-CHECK-OPEN.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO PRLM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3150-COUNT-SEQ.
           IF PRLT-SEQ < 9999999
               ADD 1 TO PRLT-SEQ
           END-IF
           IF PRLT-SEQ NOT < 6
               MOVE 'Y' TO PRLM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E204 REF NO REQUIRED
      * E961 FICA AMT NEGATIVE
      * E084 FICA AMT OVER 25000
      * E970 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3160-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PRLM-REF-NO = SPACES
                   MOVE 'E204' TO WS-REJECT-CODE
               WHEN PRLT-FICA-AMT < 0
                   MOVE 'E961' TO WS-REJECT-CODE
               WHEN PRLT-FICA-AMT > 25000.00
                   MOVE 'E084' TO WS-REJECT-CODE
               WHEN PRLM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E970' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT NET AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3170-SPLIT-NET.
           COMPUTE WS-WORK-AMT = PRLT-NET-AMT / 4
           COMPUTE WS-WORK-AMT-2 = PRLT-NET-AMT - (WS-WORK-AMT * 4)
           COMPUTE PRLT-FICA-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATE TAX AMT IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3180-LIMIT-STATE-TAX.
           IF PRLM-STATE-TAX-AMT < 25.00
               MOVE 25.00 TO PRLM-STATE-TAX-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PRLM-STATE-TAX-AMT > 500.00
               MOVE 500.00 TO PRLM-STATE-TAX-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FED TAX AMT.
      *----------------------------------------------------------------*
       3190-PRORATE-FED-TAX.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PRLM-FED-TAX-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PRLM-FED-TAX-AMT
           END-IF
           ADD PRLM-FED-TAX-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3200-DESCRIBE-SOURCE.
           EVALUATE PRLM-SOURCE-CD
               WHEN 'NY'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'PL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'VK'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'WB'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'XL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN POST DATE IS PAST
      * BAT -> MAL WHEN POST DATE IS PAST
      * MAL -> TEL WHEN POST DATE IS PAST
      * TEL -> ONL WHEN POST DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3210-SET-CHANNEL.
           EVALUATE PRLT-CHANNEL-CD
               WHEN 'BRN'
                   IF PRLT-POST-DATE > 0 AND PRLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO PRLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF PRLT-POST-DATE > 0 AND PRLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO PRLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF PRLT-POST-DATE > 0 AND PRLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO PRLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF PRLT-POST-DATE > 0 AND PRLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO PRLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE FED TAX YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3220-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN PRLM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PRLT-REVERSAL-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = PRLM-FED-TAX-YTD *
                       PRLM-MARGIN / 200
               WHEN PRLM-PAY-FREQ-CD = 'JT'
                   MOVE 10.00 TO WS-WORK-AMT-2
               WHEN PRLM-PAY-FREQ-CD = 'XM'
                   MOVE 7.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = PRLM-FED-TAX-YTD *
                       PRLM-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 2.00
               MOVE 2.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 250.00
               MOVE 250.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON NET AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3230-ACCRUE-NET.
           IF PRLT-NET-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PRLT-NET-AMT * PRLM-RATE / 100 / 360
               ADD WS-WORK-AMT TO PRLT-STATE-TAX-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PAY FREQ CD:
      * JT -> XM WHEN OPEN DATE IS PAST
      * XM -> ZT WHEN OPEN DATE IS PAST
      * ZT -> JT WHEN OPEN DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3240-SET-PAY-FREQ.
           EVALUATE PRLM-PAY-FREQ-CD
               WHEN 'JT'
                   IF PRLM-OPEN-DATE > 0 AND PRLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XM' TO PRLM-PAY-FREQ-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XM'
                   IF PRLM-OPEN-DATE > 0 AND PRLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZT' TO PRLM-PAY-FREQ-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZT'
                   IF PRLM-OPEN-DATE > 0 AND PRLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JT' TO PRLM-PAY-FREQ-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3250-DESCRIBE-REASON.
           EVALUATE PRLM-REASON-CD
               WHEN 'B'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'C'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'G'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'H'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'L'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'T'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'X'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE DEDUCT YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3260-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PRLT-CHANNEL-CD
                   ADD PRLM-DEDUCT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PRLT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PRLM-DEDUCT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3270-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FED TAX YTD.
      *----------------------------------------------------------------*
       3280-PRORATE-FED-TAX.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PRLM-FED-TAX-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PRLM-FED-TAX-YTD
           END-IF
           ADD PRLM-FED-TAX-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 365 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3290-CHECK-EXPIRY.
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
           IF WS-DAYS > 365
               MOVE 'Y' TO PRLT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT STATE TAX AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3300-SPLIT-STATE-TAX.
           COMPUTE WS-WORK-AMT = PRLT-STATE-TAX-AMT / 12
           COMPUTE WS-WORK-AMT-2 = PRLT-STATE-TAX-AMT - (WS-WORK-AMT *
               12)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NET YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3310-FLAG-NET.
           IF PRLM-NET-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PRLM-NET-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FED TAX YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3320-SPLIT-FED-TAX.
           COMPUTE WS-WORK-AMT = PRLM-FED-TAX-YTD / 12
           COMPUTE WS-WORK-AMT-2 = PRLM-FED-TAX-YTD - (WS-WORK-AMT * 12)
           COMPUTE PRLT-NET-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3330-DESCRIBE-STATUS.
           EVALUATE PRLM-STATUS-CD
               WHEN 'MV'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'RD'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'ST'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'TJ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'UC'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE FICA GRID TIER FOR THE NET YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3340-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-FICA-GRID-RATE(1) TO PRLM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF PRLM-NET-YTD NOT < WS-FICA-GRID-LOW(WS-TX)
                   MOVE WS-FICA-GRID-RATE(WS-TX) TO PRLM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PRLM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PRLM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FICA AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3350-PRORATE-FICA.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PRLM-FICA-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3360-AGE-OPEN.
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
                   ADD PRLM-FED-TAX-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD PRLM-FED-TAX-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD PRLM-FED-TAX-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PRLM-FED-TAX-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PRLM-FED-TAX-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE FICA FEE ON THE STATE TAX YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3370-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN PRLM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO PRLT-FICA-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PRLM-PREFERRED-FLAG = 'Y'
                   COMPUTE PRLT-FICA-AMT ROUNDED = PRLM-STATE-TAX-YTD *
                       PRLM-RATE / 200
               WHEN PRLM-REASON-CD = 'B'
                   MOVE 15.00 TO PRLT-FICA-AMT
               WHEN PRLM-REASON-CD = 'C'
                   MOVE 10.00 TO PRLT-FICA-AMT
               WHEN OTHER
                   COMPUTE PRLT-FICA-AMT ROUNDED = PRLM-STATE-TAX-YTD *
                       PRLM-RATE / 100
           END-EVALUATE
           IF PRLT-FICA-AMT > 0 AND PRLT-FICA-AMT < 10.00
               MOVE 10.00 TO PRLT-FICA-AMT
           END-IF
           IF PRLT-FICA-AMT > 100.00
               MOVE 100.00 TO PRLT-FICA-AMT
           END-IF
           ADD PRLT-FICA-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FICA YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3380-FLAG-FICA.
           IF PRLM-FICA-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PRLM-FICA-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3390-DESCRIBE-REGION.
           EVALUATE PRLM-REGION-CD
               WHEN 'DP'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'TU'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'WD'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE NET AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO NET YTD.
      *----------------------------------------------------------------*
       3400-PRORATE-NET.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PRLM-NET-YTD ROUNDED =
                   PRLT-NET-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PRLM-NET-YTD
           END-IF
           ADD PRLM-NET-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DEDUCT AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3410-ACCRUE-DEDUCT.
           IF PRLT-DEDUCT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PRLT-DEDUCT-AMT * PRLM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO PRLM-FED-TAX-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FED TAX FEE ON THE FED TAX YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3420-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN PRLM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO PRLM-FED-TAX-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PRLM-STOP-FLAG = 'Y'
                   COMPUTE PRLM-FED-TAX-AMT ROUNDED = PRLM-FED-TAX-YTD *
                       PRLM-RATE / 200
               WHEN PRLM-STATUS-CD = 'MV'
                   MOVE 20.00 TO PRLM-FED-TAX-AMT
               WHEN PRLM-STATUS-CD = 'RD'
                   MOVE 20.00 TO PRLM-FED-TAX-AMT
               WHEN OTHER
                   COMPUTE PRLM-FED-TAX-AMT ROUNDED = PRLM-FED-TAX-YTD *
                       PRLM-RATE / 100
           END-EVALUATE
           IF PRLM-FED-TAX-AMT > 0 AND PRLM-FED-TAX-AMT < 5.00
               MOVE 5.00 TO PRLM-FED-TAX-AMT
           END-IF
           IF PRLM-FED-TAX-AMT > 150.00
               MOVE 150.00 TO PRLM-FED-TAX-AMT
           END-IF
           ADD PRLM-FED-TAX-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * GROSS AMT IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3430-LIMIT-GROSS.
           IF PRLM-GROSS-AMT < 25.00
               MOVE 25.00 TO PRLM-GROSS-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PRLM-GROSS-AMT > 500.00
               MOVE 500.00 TO PRLM-GROSS-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE GROSS AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FED TAX AMT.
      *----------------------------------------------------------------*
       3440-PRORATE-FED-TAX.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PRLM-FED-TAX-AMT ROUNDED =
                   PRLM-GROSS-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PRLM-FED-TAX-AMT
           END-IF
           ADD PRLM-FED-TAX-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3450-AGE-NOTICE.
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
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE STATE TAX FEE ON THE DEDUCT AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3460-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN PRLT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO PRLT-STATE-TAX-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PRLM-PREFERRED-FLAG = 'Y'
                   COMPUTE PRLT-STATE-TAX-AMT ROUNDED = PRLM-DEDUCT-AMT
                       * PRLM-MARGIN / 200
               WHEN PRLM-REASON-CD = 'B'
                   MOVE 20.00 TO PRLT-STATE-TAX-AMT
               WHEN PRLM-REASON-CD = 'C'
                   MOVE 20.00 TO PRLT-STATE-TAX-AMT
               WHEN OTHER
                   COMPUTE PRLT-STATE-TAX-AMT ROUNDED = PRLM-DEDUCT-AMT
                       * PRLM-MARGIN / 100
           END-EVALUATE
           IF PRLT-STATE-TAX-AMT > 0 AND PRLT-STATE-TAX-AMT < 10.00
               MOVE 10.00 TO PRLT-STATE-TAX-AMT
           END-IF
           IF PRLT-STATE-TAX-AMT > 50.00
               MOVE 50.00 TO PRLT-STATE-TAX-AMT
           END-IF
           ADD PRLT-STATE-TAX-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3470-CHECK-NOTICE.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO PRLM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * B -> C WHEN LAST PAY DATE IS PAST
      * C -> G WHEN LAST PAY DATE IS PAST
      * G -> H WHEN LAST PAY DATE IS PAST
      * H -> L WHEN LAST PAY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3480-SET-REASON.
           EVALUATE PRLM-REASON-CD
               WHEN 'B'
                   IF PRLM-LAST-PAY-DATE > 0 AND PRLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'C' TO PRLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'C'
                   IF PRLM-LAST-PAY-DATE > 0 AND PRLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'G' TO PRLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'G'
                   IF PRLM-LAST-PAY-DATE > 0 AND PRLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO PRLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF PRLM-LAST-PAY-DATE > 0 AND PRLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO PRLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT FED TAX YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3490-SPLIT-FED-TAX.
           COMPUTE WS-WORK-AMT = PRLM-FED-TAX-YTD / 2
           COMPUTE WS-WORK-AMT-2 = PRLM-FED-TAX-YTD - (WS-WORK-AMT * 2)
           COMPUTE PRLM-NET-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PAY FREQ CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3500-DESCRIBE-PAY-FREQ.
           EVALUATE PRLM-PAY-FREQ-CD
               WHEN 'JT'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'XM'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'ZT'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3510-DESCRIBE-REASON.
           EVALUATE PRLM-REASON-CD
               WHEN 'B'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'C'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'G'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'H'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'L'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'T'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'X'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STATE TAX YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3520-ACCRUE-STATE-TAX.
           IF PRLM-STATE-TAX-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PRLM-STATE-TAX-YTD * PRLM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO PRLM-DEDUCT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DEDUCT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FICA AMT.
      *----------------------------------------------------------------*
       3530-PRORATE-FICA.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PRLT-FICA-AMT ROUNDED =
                   PRLM-DEDUCT-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PRLT-FICA-AMT
           END-IF
           ADD PRLT-FICA-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3540-AGE-EXPIRY.
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
                   ADD PRLM-STATE-TAX-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD PRLM-STATE-TAX-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD PRLM-STATE-TAX-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PRLM-STATE-TAX-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PRLM-STATE-TAX-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3550-AGE-POST.
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
                   ADD PRLM-DEDUCT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD PRLM-DEDUCT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD PRLM-DEDUCT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD PRLM-DEDUCT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PRLM-DEDUCT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE STATE TAX AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3560-ACCUM-REGION.
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
      * DEDUCT YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3570-FLAG-DEDUCT.
           IF PRLM-DEDUCT-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PRLM-DEDUCT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FED TAX FEE ON THE FED TAX YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3580-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN PRLT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO PRLM-FED-TAX-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PRLM-HOLD-FLAG = 'Y'
                   COMPUTE PRLM-FED-TAX-AMT ROUNDED = PRLM-FED-TAX-YTD *
                       PRLM-RATE / 200
               WHEN PRLM-TAX-STATE-CD = 'CAZ'
                   MOVE 5.00 TO PRLM-FED-TAX-AMT
               WHEN PRLM-TAX-STATE-CD = 'LZU'
                   MOVE 7.00 TO PRLM-FED-TAX-AMT
               WHEN OTHER
                   COMPUTE PRLM-FED-TAX-AMT ROUNDED = PRLM-FED-TAX-YTD *
                       PRLM-RATE / 100
           END-EVALUATE
           IF PRLM-FED-TAX-AMT > 0 AND PRLM-FED-TAX-AMT < 10.00
               MOVE 10.00 TO PRLM-FED-TAX-AMT
           END-IF
           IF PRLM-FED-TAX-AMT > 75.00
               MOVE 75.00 TO PRLM-FED-TAX-AMT
           END-IF
           ADD PRLM-FED-TAX-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE STATE TAX YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO STATE TAX AMT.
      *----------------------------------------------------------------*
       3590-PRORATE-STATE-TAX.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PRLT-STATE-TAX-AMT ROUNDED =
                   PRLM-STATE-TAX-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PRLT-STATE-TAX-AMT
           END-IF
           ADD PRLT-STATE-TAX-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE NET AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO NET YTD.
      *----------------------------------------------------------------*
       3600-PRORATE-NET.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PRLM-NET-YTD ROUNDED =
                   PRLM-NET-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PRLM-NET-YTD
           END-IF
           ADD PRLM-NET-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE FICA GRID TIER FOR THE NET AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3610-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-FICA-GRID-RATE(1) TO PRLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF PRLM-NET-AMT NOT < WS-FICA-GRID-LOW(WS-TX)
                   MOVE WS-FICA-GRID-RATE(WS-TX) TO PRLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PRLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PRLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3620-ACCRUE-RATE.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * PRLM-RATE / 100 / 360
               ADD WS-WORK-AMT TO PRLM-FICA-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE NET AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FED TAX YTD.
      *----------------------------------------------------------------*
       3630-PRORATE-FED-TAX.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PRLM-FED-TAX-YTD ROUNDED =
                   PRLM-NET-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PRLM-FED-TAX-YTD
           END-IF
           ADD PRLM-FED-TAX-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3640-AGE-CLOSE.
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
                   ADD PRLT-STATE-TAX-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PRLT-STATE-TAX-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD PRLT-STATE-TAX-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD PRLT-STATE-TAX-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PRLT-STATE-TAX-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT STATE TAX YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3650-SPLIT-STATE-TAX.
           COMPUTE WS-WORK-AMT = PRLM-STATE-TAX-YTD / 2
           COMPUTE WS-WORK-AMT-2 = PRLM-STATE-TAX-YTD - (WS-WORK-AMT *
               2)
           COMPUTE PRLT-FICA-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3660-COUNT-SEQ.
           IF PRLT-SEQ < 9999999
               ADD 1 TO PRLT-SEQ
           END-IF
           IF PRLT-SEQ NOT < 6
               MOVE 'Y' TO PRLM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E177 ADDR REQUIRED
      * E297 WORK AMT NEGATIVE
      * E364 WORK AMT OVER 99999
      * E093 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3670-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PRLM-ADDR = SPACES
                   MOVE 'E177' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E297' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 99999.00
                   MOVE 'E364' TO WS-REJECT-CODE
               WHEN PRLM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E093' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3680-COUNT-PAYMENT.
           IF PRLM-PAYMENT-CNT < 9999999
               ADD 1 TO PRLM-PAYMENT-CNT
           END-IF
           IF PRLM-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO PRLM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3690-DESCRIBE-TRAN.
           EVALUATE PRLT-TRAN-CD
               WHEN 'PR'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'AD'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'VD'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'TX'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'DP'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FICA YTD IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3700-LIMIT-FICA.
           IF PRLM-FICA-YTD < 0.00
               MOVE 0.00 TO PRLM-FICA-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PRLM-FICA-YTD > 10000.00
               MOVE 10000.00 TO PRLM-FICA-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3710-COUNT-NOTICE.
           IF PRLM-NOTICE-CNT < 9999999
               ADD 1 TO PRLM-NOTICE-CNT
           END-IF
           IF PRLM-NOTICE-CNT NOT < 5
               MOVE 'Y' TO PRLM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3720-CHECK-CLOSE.
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
               MOVE 'Y' TO PRLM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E120 NAME REQUIRED
      * E757 STATE TAX AMT NEGATIVE
      * E261 STATE TAX AMT OVER 5000
      * E459 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3730-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PRLM-NAME = SPACES
                   MOVE 'E120' TO WS-REJECT-CODE
               WHEN PRLM-STATE-TAX-AMT < 0
                   MOVE 'E757' TO WS-REJECT-CODE
               WHEN PRLM-STATE-TAX-AMT > 5000.00
                   MOVE 'E261' TO WS-REJECT-CODE
               WHEN PRLM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E459' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * PR -> AD WHEN POST DATE IS PAST
      * AD -> VD WHEN POST DATE IS PAST
      * VD -> TX WHEN POST DATE IS PAST
      * TX -> DP WHEN POST DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3740-SET-TRAN.
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
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3750-COUNT-NOTICE.
           IF PRLM-NOTICE-CNT < 9999999
               ADD 1 TO PRLM-NOTICE-CNT
           END-IF
           IF PRLM-NOTICE-CNT NOT < 10
               MOVE 'Y' TO PRLM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3760-ACCRUE-FEE-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * PRLM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO PRLM-NET-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATE TAX AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3770-FLAG-STATE-TAX.
           IF PRLT-STATE-TAX-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PRLT-STATE-TAX-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3780-COUNT-ITEM.
           IF PRLM-ITEM-CNT < 9999999
               ADD 1 TO PRLM-ITEM-CNT
           END-IF
           IF PRLM-ITEM-CNT NOT < 6
               MOVE 'Y' TO PRLM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FICA YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO NET AMT.
      *----------------------------------------------------------------*
       3790-PRORATE-NET.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PRLT-NET-AMT ROUNDED =
                   PRLM-FICA-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PRLT-NET-AMT
           END-IF
           ADD PRLT-NET-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE DEDUCT TIER TIER FOR THE STATE TAX YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3800-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-DEDUCT-TIER-RATE(1) TO PRLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF PRLM-STATE-TAX-YTD NOT < WS-DEDUCT-TIER-LOW(WS-TX)
                   MOVE WS-DEDUCT-TIER-RATE(WS-TX) TO PRLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PRLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PRLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * B -> C WHEN POST DATE IS PAST
      * C -> G WHEN POST DATE IS PAST
      * G -> H WHEN POST DATE IS PAST
      * H -> L WHEN POST DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3810-SET-REASON.
           EVALUATE PRLM-REASON-CD
               WHEN 'B'
                   IF PRLT-POST-DATE > 0 AND PRLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'C' TO PRLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'C'
                   IF PRLT-POST-DATE > 0 AND PRLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'G' TO PRLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'G'
                   IF PRLT-POST-DATE > 0 AND PRLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO PRLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF PRLT-POST-DATE > 0 AND PRLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO PRLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FED TAX AMT IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3820-LIMIT-FED-TAX.
           IF PRLM-FED-TAX-AMT < 10.00
               MOVE 10.00 TO PRLM-FED-TAX-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PRLM-FED-TAX-AMT > 10000.00
               MOVE 10000.00 TO PRLM-FED-TAX-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FICA YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO NET YTD.
      *----------------------------------------------------------------*
       3830-PRORATE-NET.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PRLM-NET-YTD ROUNDED =
                   PRLM-FICA-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PRLM-NET-YTD
           END-IF
           ADD PRLM-NET-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 45 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3840-CHECK-LAST-ACT.
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
               MOVE 'Y' TO PRLM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DEDUCT FEE ON THE DEDUCT AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3850-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN PRLM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO PRLM-DEDUCT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PRLM-STOP-FLAG = 'Y'
                   COMPUTE PRLM-DEDUCT-AMT ROUNDED = PRLT-DEDUCT-AMT *
                       PRLM-FEE-PCT / 200
               WHEN PRLT-CHANNEL-CD = 'BRN'
                   MOVE 3.00 TO PRLM-DEDUCT-AMT
               WHEN PRLT-CHANNEL-CD = 'BAT'
                   MOVE 10.00 TO PRLM-DEDUCT-AMT
               WHEN OTHER
                   COMPUTE PRLM-DEDUCT-AMT ROUNDED = PRLT-DEDUCT-AMT *
                       PRLM-FEE-PCT / 100
           END-EVALUATE
           IF PRLM-DEDUCT-AMT > 0 AND PRLM-DEDUCT-AMT < 1.00
               MOVE 1.00 TO PRLM-DEDUCT-AMT
           END-IF
           IF PRLM-DEDUCT-AMT > 100.00
               MOVE 100.00 TO PRLM-DEDUCT-AMT
           END-IF
           ADD PRLM-DEDUCT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3860-AGE-EFF.
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
                   ADD PRLM-NET-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD PRLM-NET-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD PRLM-NET-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PRLM-NET-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PRLM-NET-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * GROSS AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3870-FLAG-GROSS.
           IF PRLM-GROSS-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PRLM-GROSS-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3880-CHECK-EXPIRY.
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
               MOVE 'Y' TO PRLM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NET AMT IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3890-LIMIT-NET.
           IF PRLM-NET-AMT < 0.00
               MOVE 0.00 TO PRLM-NET-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PRLM-NET-AMT > 10000.00
               MOVE 10000.00 TO PRLM-NET-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PAY FREQ CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3900-DESCRIBE-PAY-FREQ.
           EVALUATE PRLM-PAY-FREQ-CD
               WHEN 'JT'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'XM'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'ZT'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DEDUCT YTD IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3910-LIMIT-DEDUCT.
           IF PRLM-DEDUCT-YTD < 100.00
               MOVE 100.00 TO PRLM-DEDUCT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PRLM-DEDUCT-YTD > 10000.00
               MOVE 10000.00 TO PRLM-DEDUCT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3920-CHECK-EFFECTIVE.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO PRLM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE GROSS AMT BY PAY FREQ CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3930-ACCUM-PAY-FREQ.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PRLM-PAY-FREQ-CD
                   ADD PRLM-GROSS-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PRLM-PAY-FREQ-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PRLM-GROSS-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E487 ADDR REQUIRED
      * E193 NET AMT NEGATIVE
      * E418 NET AMT OVER 5000
      * E007 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3940-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PRLM-ADDR = SPACES
                   MOVE 'E487' TO WS-REJECT-CODE
               WHEN PRLM-NET-AMT < 0
                   MOVE 'E193' TO WS-REJECT-CODE
               WHEN PRLM-NET-AMT > 5000.00
                   MOVE 'E418' TO WS-REJECT-CODE
               WHEN PRLT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E007' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3950-CHECK-CLOSE.
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
           IF WS-DAYS > 365
               MOVE 'Y' TO PRLM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E310 DESC REQUIRED
      * E908 NET AMT NEGATIVE
      * E582 NET AMT OVER 99999
      * E901 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3960-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PRLM-DESC = SPACES
                   MOVE 'E310' TO WS-REJECT-CODE
               WHEN PRLT-NET-AMT < 0
                   MOVE 'E908' TO WS-REJECT-CODE
               WHEN PRLT-NET-AMT > 99999.00
                   MOVE 'E582' TO WS-REJECT-CODE
               WHEN PRLM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E901' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE GROSS AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO NET AMT.
      *----------------------------------------------------------------*
       3970-PRORATE-NET.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PRLM-NET-AMT ROUNDED =
                   PRLM-GROSS-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PRLM-NET-AMT
           END-IF
           ADD PRLM-NET-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DEDUCT AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3980-FLAG-DEDUCT.
           IF PRLM-DEDUCT-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PRLM-DEDUCT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PAY FREQ CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3990-DESCRIBE-PAY-FREQ.
           EVALUATE PRLM-PAY-FREQ-CD
               WHEN 'JT'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'XM'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ZT'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATE TAX AMT IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4000-LIMIT-STATE-TAX.
           IF PRLT-STATE-TAX-AMT < 10.00
               MOVE 10.00 TO PRLT-STATE-TAX-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PRLT-STATE-TAX-AMT > 500.00
               MOVE 500.00 TO PRLT-STATE-TAX-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4010-AGE-LAST-PAY.
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
                   ADD PRLT-DEDUCT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PRLT-DEDUCT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD PRLT-DEDUCT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PRLT-DEDUCT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PRLT-DEDUCT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TAX STATE CD:
      * CAZ -> LZU WHEN NOTICE DATE IS PAST
      * LZU -> TUF WHEN NOTICE DATE IS PAST
      * TUF -> WKE WHEN NOTICE DATE IS PAST
      * WKE -> CAZ WHEN NOTICE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4020-SET-TAX-STATE.
           EVALUATE PRLM-TAX-STATE-CD
               WHEN 'CAZ'
                   IF PRLM-NOTICE-DATE > 0 AND PRLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LZU' TO PRLM-TAX-STATE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LZU'
                   IF PRLM-NOTICE-DATE > 0 AND PRLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TUF' TO PRLM-TAX-STATE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TUF'
                   IF PRLM-NOTICE-DATE > 0 AND PRLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WKE' TO PRLM-TAX-STATE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WKE'
                   IF PRLM-NOTICE-DATE > 0 AND PRLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CAZ' TO PRLM-TAX-STATE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATE TAX AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4030-FLAG-STATE-TAX.
           IF PRLM-STATE-TAX-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PRLM-STATE-TAX-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E282 ADDR REQUIRED
      * E863 WORK AMT 2 NEGATIVE
      * E857 WORK AMT 2 OVER 99999
      * E863 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       4040-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PRLM-ADDR = SPACES
                   MOVE 'E282' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E863' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 99999.00
                   MOVE 'E857' TO WS-REJECT-CODE
               WHEN PRLM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E863' TO WS-REJECT-CODE
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
       4050-FLAG-FICA.
           IF PRLM-FICA-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PRLM-FICA-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * PR -> AD WHEN LAST ACT DATE IS PAST
      * AD -> VD WHEN LAST ACT DATE IS PAST
      * VD -> TX WHEN LAST ACT DATE IS PAST
      * TX -> DP WHEN LAST ACT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4060-SET-TRAN.
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
      * ACCUMULATE NET YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4070-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PRLM-STATUS-CD
                   ADD PRLM-NET-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PRLM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PRLM-NET-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DEDUCT FEE ON THE GROSS YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4080-ASSESS-STOP.
           EVALUATE TRUE
               WHEN PRLM-STOP-FLAG = 'Y'
                   MOVE ZERO TO PRLT-DEDUCT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PRLM-PREFERRED-FLAG = 'Y'
                   COMPUTE PRLT-DEDUCT-AMT ROUNDED = PRLM-GROSS-YTD *
                       PRLM-FEE-PCT / 200
               WHEN PRLM-TAX-STATE-CD = 'CAZ'
                   MOVE 5.00 TO PRLT-DEDUCT-AMT
               WHEN PRLM-TAX-STATE-CD = 'LZU'
                   MOVE 5.00 TO PRLT-DEDUCT-AMT
               WHEN OTHER
                   COMPUTE PRLT-DEDUCT-AMT ROUNDED = PRLM-GROSS-YTD *
                       PRLM-FEE-PCT / 100
           END-EVALUATE
           IF PRLT-DEDUCT-AMT > 0 AND PRLT-DEDUCT-AMT < 10.00
               MOVE 10.00 TO PRLT-DEDUCT-AMT
           END-IF
           IF PRLT-DEDUCT-AMT > 50.00
               MOVE 50.00 TO PRLT-DEDUCT-AMT
           END-IF
           ADD PRLT-DEDUCT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 15 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4090-CHECK-LAST-PAY.
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
      * ACCUMULATE FICA AMT BY TAX STATE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4100-ACCUM-TAX-STATE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PRLM-TAX-STATE-CD
                   ADD PRLM-FICA-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PRLM-TAX-STATE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PRLM-FICA-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE NET AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       4110-PRORATE-NET.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   PRLT-NET-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * NET AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4120-FLAG-NET.
           IF PRLM-NET-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PRLM-NET-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4130-DESCRIBE-CHANNEL.
           EVALUATE PRLT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E866 CUST ID REQUIRED
      * E810 GROSS AMT NEGATIVE
      * E247 GROSS AMT OVER 250000
      * E270 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       4140-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PRLM-CUST-ID = SPACES
                   MOVE 'E866' TO WS-REJECT-CODE
               WHEN PRLM-GROSS-AMT < 0
                   MOVE 'E810' TO WS-REJECT-CODE
               WHEN PRLM-GROSS-AMT > 250000.00
                   MOVE 'E247' TO WS-REJECT-CODE
               WHEN PRLM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E270' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DEDUCT AMT IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4150-LIMIT-DEDUCT.
           IF PRLM-DEDUCT-AMT < 10.00
               MOVE 10.00 TO PRLM-DEDUCT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PRLM-DEDUCT-AMT > 2500.00
               MOVE 2500.00 TO PRLM-DEDUCT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE DEDUCT AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4160-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PRLM-REGION-CD
                   ADD PRLT-DEDUCT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PRLM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PRLT-DEDUCT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STATE TAX YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4170-ACCRUE-STATE-TAX.
           IF PRLM-STATE-TAX-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PRLM-STATE-TAX-YTD * PRLM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       4180-COUNT-PAYMENT.
           IF PRLM-PAYMENT-CNT < 9999999
               ADD 1 TO PRLM-PAYMENT-CNT
           END-IF
           IF PRLM-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO PRLM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE GROSS YTD BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4190-ACCUM-TRAN.
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
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E309 CONTACT REQUIRED
      * E030 NET YTD NEGATIVE
      * E870 NET YTD OVER 10000
      * E198 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       4200-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PRLM-CONTACT = SPACES
                   MOVE 'E309' TO WS-REJECT-CODE
               WHEN PRLM-NET-YTD < 0
                   MOVE 'E030' TO WS-REJECT-CODE
               WHEN PRLM-NET-YTD > 10000.00
                   MOVE 'E870' TO WS-REJECT-CODE
               WHEN PRLM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E198' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FICA AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       4210-PRORATE-FICA.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   PRLT-FICA-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DEDUCT YTD IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4220-LIMIT-DEDUCT.
           IF PRLM-DEDUCT-YTD < 50.00
               MOVE 50.00 TO PRLM-DEDUCT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PRLM-DEDUCT-YTD > 500.00
               MOVE 500.00 TO PRLM-DEDUCT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATE TAX AMT IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4230-LIMIT-STATE-TAX.
           IF PRLM-STATE-TAX-AMT < 0.00
               MOVE 0.00 TO PRLM-STATE-TAX-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PRLM-STATE-TAX-AMT > 50000.00
               MOVE 50000.00 TO PRLM-STATE-TAX-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DEDUCT YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4240-FLAG-DEDUCT.
           IF PRLM-DEDUCT-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PRLM-DEDUCT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FED TAX AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4250-ACCRUE-FED-TAX.
           IF PRLM-FED-TAX-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PRLM-FED-TAX-AMT * PRLM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO PRLT-STATE-TAX-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4260-DESCRIBE-CHANNEL.
           EVALUATE PRLT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE GROSS YTD BY TAX STATE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4270-ACCUM-TAX-STATE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PRLM-TAX-STATE-CD
                   ADD PRLM-GROSS-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PRLM-TAX-STATE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PRLM-GROSS-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON GROSS AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4280-ACCRUE-GROSS.
           IF PRLM-GROSS-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PRLM-GROSS-AMT * PRLM-RATE / 100 / 360
               ADD WS-WORK-AMT TO PRLT-NET-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FICA AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4290-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PRLT-TRAN-CD
                   ADD PRLM-FICA-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PRLT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PRLM-FICA-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * NY -> PL WHEN CLOSE DATE IS PAST
      * PL -> VK WHEN CLOSE DATE IS PAST
      * VK -> WB WHEN CLOSE DATE IS PAST
      * WB -> XL WHEN CLOSE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4300-SET-SOURCE.
           EVALUATE PRLM-SOURCE-CD
               WHEN 'NY'
                   IF PRLM-CLOSE-DATE > 0 AND PRLM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PL' TO PRLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PL'
                   IF PRLM-CLOSE-DATE > 0 AND PRLM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VK' TO PRLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VK'
                   IF PRLM-CLOSE-DATE > 0 AND PRLM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WB' TO PRLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WB'
                   IF PRLM-CLOSE-DATE > 0 AND PRLM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XL' TO PRLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PAY FREQ CD:
      * JT -> XM WHEN LAST PAY DATE IS PAST
      * XM -> ZT WHEN LAST PAY DATE IS PAST
      * ZT -> JT WHEN LAST PAY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4310-SET-PAY-FREQ.
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
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       4320-COUNT-NOTICE.
           IF PRLM-NOTICE-CNT < 9999999
               ADD 1 TO PRLM-NOTICE-CNT
           END-IF
           IF PRLM-NOTICE-CNT NOT < 12
               MOVE 'Y' TO PRLM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * NET YTD IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4330-LIMIT-NET.
           IF PRLM-NET-YTD < 100.00
               MOVE 100.00 TO PRLM-NET-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PRLM-NET-YTD > 5000.00
               MOVE 5000.00 TO PRLM-NET-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4340-AGE-EFF.
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
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE FICA AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4350-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN PRLM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PRLM-PREFERRED-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = PRLM-FICA-AMT *
                       PRLM-MARGIN / 200
               WHEN PRLM-SOURCE-CD = 'NY'
                   MOVE 7.00 TO WS-WORK-AMT
               WHEN PRLM-SOURCE-CD = 'PL'
                   MOVE 10.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = PRLM-FICA-AMT *
                       PRLM-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 1.00
               MOVE 1.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 100.00
               MOVE 100.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON NET AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4360-ACCRUE-NET.
           IF PRLT-NET-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PRLT-NET-AMT * PRLM-RATE / 100 / 365
               ADD WS-WORK-AMT TO PRLM-FED-TAX-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STATE TAX AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4370-ACCRUE-STATE-TAX.
           IF PRLM-STATE-TAX-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PRLM-STATE-TAX-AMT * PRLM-RATE / 100 / 365
               ADD WS-WORK-AMT TO PRLM-NET-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E541 REF NO REQUIRED
      * E472 GROSS AMT NEGATIVE
      * E611 GROSS AMT OVER 1000000
      * E215 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       4380-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PRLM-REF-NO = SPACES
                   MOVE 'E541' TO WS-REJECT-CODE
               WHEN PRLM-GROSS-AMT < 0
                   MOVE 'E472' TO WS-REJECT-CODE
               WHEN PRLM-GROSS-AMT > 1000000.00
                   MOVE 'E611' TO WS-REJECT-CODE
               WHEN PRLT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E215' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FICA AMT IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4390-LIMIT-FICA.
           IF PRLT-FICA-AMT < 0.00
               MOVE 0.00 TO PRLT-FICA-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PRLT-FICA-AMT > 1000.00
               MOVE 1000.00 TO PRLT-FICA-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE GROSS FEE ON THE GROSS YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4400-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN PRLM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO PRLM-GROSS-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PRLM-SUSPEND-FLAG = 'Y'
                   COMPUTE PRLM-GROSS-AMT ROUNDED = PRLM-GROSS-YTD *
                       PRLM-MARGIN / 200
               WHEN PRLM-SOURCE-CD = 'NY'
                   MOVE 10.00 TO PRLM-GROSS-AMT
               WHEN PRLM-SOURCE-CD = 'PL'
                   MOVE 3.00 TO PRLM-GROSS-AMT
               WHEN OTHER
                   COMPUTE PRLM-GROSS-AMT ROUNDED = PRLM-GROSS-YTD *
                       PRLM-MARGIN / 100
           END-EVALUATE
           IF PRLM-GROSS-AMT > 0 AND PRLM-GROSS-AMT < 2.00
               MOVE 2.00 TO PRLM-GROSS-AMT
           END-IF
           IF PRLM-GROSS-AMT > 25.00
               MOVE 25.00 TO PRLM-GROSS-AMT
           END-IF
           ADD PRLM-GROSS-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE GROSS YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO NET AMT.
      *----------------------------------------------------------------*
       4410-PRORATE-NET.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PRLT-NET-AMT ROUNDED =
                   PRLM-GROSS-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PRLT-NET-AMT
           END-IF
           ADD PRLT-NET-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE FICA GRID TIER FOR THE DEDUCT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4420-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-FICA-GRID-RATE(1) TO PRLM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF PRLT-DEDUCT-AMT NOT < WS-FICA-GRID-LOW(WS-TX)
                   MOVE WS-FICA-GRID-RATE(WS-TX) TO PRLM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PRLM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PRLM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FICA AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4430-ACCUM-CHANNEL.
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
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4440-AGE-NEXT-DUE.
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
                   ADD PRLT-FICA-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PRLT-FICA-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD PRLT-FICA-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PRLT-FICA-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PRLT-FICA-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT FED TAX AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4450-SPLIT-FED-TAX.
           COMPUTE WS-WORK-AMT = PRLM-FED-TAX-AMT / 3
           COMPUTE WS-WORK-AMT-2 = PRLM-FED-TAX-AMT - (WS-WORK-AMT * 3)
           COMPUTE PRLM-DEDUCT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DEDUCT TIER TIER FOR THE FICA AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4460-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DEDUCT-TIER-RATE(1) TO PRLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF PRLT-FICA-AMT NOT < WS-DEDUCT-TIER-LOW(WS-TX)
                   MOVE WS-DEDUCT-TIER-RATE(WS-TX) TO PRLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PRLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PRLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E029 DESC REQUIRED
      * E326 NET AMT NEGATIVE
      * E468 NET AMT OVER 99999
      * E219 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       4470-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PRLM-DESC = SPACES
                   MOVE 'E029' TO WS-REJECT-CODE
               WHEN PRLM-NET-AMT < 0
                   MOVE 'E326' TO WS-REJECT-CODE
               WHEN PRLM-NET-AMT > 99999.00
                   MOVE 'E468' TO WS-REJECT-CODE
               WHEN PRLT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E219' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

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
                   MOVE PYPRC-STOP-FLAG
                     TO PRLM-STOP-FLAG
                   MOVE PYPRC-AUTO-RENEW-FLAG
                     TO PRLM-AUTO-RENEW-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RPRC' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-PYPRC' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
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
                   MOVE PYFEE-DEDUCT-AMT
                     TO PRLM-DEDUCT-AMT
                   MOVE PYFEE-NET-AMT
                     TO PRLM-NET-AMT
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
                   MOVE PYTIER-STATE-TAX-AMT
                     TO PRLM-STATE-TAX-AMT
                   MOVE PYTIER-NET-AMT
                     TO PRLM-NET-AMT
                   MOVE PYTIER-GROSS-AMT
                     TO PRLM-GROSS-AMT
                   MOVE PYTIER-FED-TAX-AMT
                     TO PRLM-FED-TAX-AMT
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
           DISPLAY 'PYLNOTC READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'PYLNOTC' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
