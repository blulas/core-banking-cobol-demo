      *================================================================*
      * PROGRAM  : PYLPOST                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : PAYROLL SERVICES - DAILY POSTING. READS THE PRL     *
      *            TRANSACTIONS, UPDATES THE PRL MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. CLIENT PAYROLL CALCULATION, TAX *
      *            DEPOSIT AND DIRECT DEPOSIT                          *
      * INPUT    : PRLMST (KSDS), PRLTRN, BANKCTL                      *
      * OUTPUT   : PRLOUT, PYLPORPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, PYTIER, PYPEN           *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1989-06-16 PQR  ORIGINAL                                       *
      * 1995-08-02 SKN  FED-TAX CUTOFF MOVED INC9232                   *
      * 1996-11-18 RJT  FED-TAX CALCULATION CORRECTED INC4641          *
      * 1996-06-19 TWH  NET LIMIT REVISED CHG4528                      *
      * 2001-06-19 CRP  NET NOTICE TEXT PER LEGAL CHG0821              *
      * 2002-07-23 BHS  STATE-TAX FEE SCHEDULE UPDATE PRB3360          *
      * 2006-01-26 GAF  NEW FICA REPORT COLUMNS INC5937                *
      * 2006-08-24 PQR  STATE-TAX RERUN PROTECTION INC9255             *
      * 2012-08-17 LMB  STATE-TAX RERUN PROTECTION REQ1299             *
      * 2012-04-16 EWS  STATE-TAX ROUNDING - HALF UP CHG6551           *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PYLPOST.
       AUTHOR. D P SHAW.

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
           SELECT REPORT-FILE ASSIGN TO "PYLPORPT"
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
      * WS-FICA-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FICA-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000694500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000355000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010050000000375000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010100000000220000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010150000000183000'.
       01  WS-FICA-SCHED-TBL REDEFINES WS-FICA-SCHED-VALUES.
           05  WS-FICA-SCHED-ROW OCCURS 5 TIMES.
               10  WS-FICA-SCHED-LOW       PIC 9(09)V99.
               10  WS-FICA-SCHED-RATE      PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-FICA-SCHEDX - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FICA-SCHEDX-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000199000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000146500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002600000000680000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002650000000618000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002750000000304000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002850000000397000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012850000000270000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015350000000417000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016350000000720500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016400000000445500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018900000000274500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018950000000206500'.
       01  WS-FICA-SCHEDX-TBL REDEFINES WS-FICA-SCHEDX-VALUES.
           05  WS-FICA-SCHEDX-ROW OCCURS 12 TIMES.
               10  WS-FICA-SCHEDX-LOW      PIC 9(09)V99.
               10  WS-FICA-SCHEDX-RATE     PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  PYLPOST  PAYROLL SERVICES'.
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
       COPY PYTIERP.
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
               PERFORM 3000-SPLIT-STATE-TAX
               PERFORM 3010-ASSESS-AUTO-RENEW
               PERFORM 3020-SPLIT-DEDUCT
               PERFORM 3030-SET-TRAN
               PERFORM 3040-SPLIT-NET
               PERFORM 3050-PRORATE-DEDUCT
               PERFORM 3060-SELECT-RATE
               PERFORM 3070-ACCUM-STATUS
               PERFORM 3080-ASSESS-NOTICE-SENT
               PERFORM 3090-FLAG-NET
               PERFORM 3100-ASSESS-NOTICE-SENT
               PERFORM 3110-CHECK-EXPIRY
               PERFORM 3120-DESCRIBE-PAY-FREQ
               PERFORM 3130-ACCRUE-STATE-TAX
               PERFORM 3140-AGE-LAST-PAY
               PERFORM 3150-ASSESS-SUSPEND
               PERFORM 3160-CHECK-NEXT-REVIEW
               PERFORM 3170-FLAG-GROSS
               PERFORM 3180-SPLIT-DEDUCT
               PERFORM 3190-FLAG-FED-TAX
               PERFORM 3200-AGE-EFFECTIVE
               PERFORM 3210-ACCRUE-NET
               PERFORM 3220-ACCUM-TAX-STATE
               PERFORM 3230-COUNT-CYCLE
               PERFORM 3240-EDIT-CONTACT
               PERFORM 3250-DESCRIBE-PAY-FREQ
               PERFORM 3260-LIMIT-STATE-TAX
               PERFORM 3270-FLAG-DEDUCT
               PERFORM 3280-SET-CHANNEL
               PERFORM 3290-DESCRIBE-TAX-STATE
               PERFORM 3300-COUNT-ITEM
               PERFORM 3310-PRORATE-STATE-TAX
               PERFORM 3320-SPLIT-FICA
               PERFORM 3330-AGE-EXPIRY
               PERFORM 3340-ACCRUE-DEDUCT
               PERFORM 3350-SELECT-RATE
               PERFORM 3360-AGE-POST
               PERFORM 3370-ASSESS-AUTO-RENEW
               PERFORM 3380-PRORATE-NET
               PERFORM 3390-SPLIT-FICA
               PERFORM 3400-FLAG-FED-TAX
               PERFORM 3410-SELECT-RATE
               PERFORM 3420-EDIT-DESC
               PERFORM 3430-EDIT-REJECT
               PERFORM 3440-LIMIT-ITEM
               PERFORM 3450-DESCRIBE-PAY-FREQ
               PERFORM 3460-EDIT-CONTACT
               PERFORM 3470-ACCRUE-NET
               PERFORM 3480-SET-SOURCE
               PERFORM 3490-FLAG-GROSS
               PERFORM 3500-LIMIT-STATE-TAX
               PERFORM 3510-ACCRUE-DEDUCT
               PERFORM 3520-SELECT-FEE-PCT
               PERFORM 3530-DESCRIBE-STATUS
               PERFORM 3540-LIMIT-DEDUCT
               PERFORM 3550-ASSESS-AUTO-RENEW
               PERFORM 3560-SET-STATUS
               PERFORM 3570-SELECT-MARGIN
               PERFORM 3580-SELECT-RATE
               PERFORM 3590-ACCRUE-DEDUCT
               PERFORM 3600-ASSESS-HOLD
               PERFORM 3610-ACCUM-SOURCE
               PERFORM 3620-CHECK-POST
               PERFORM 3630-PRORATE-FED-TAX
               PERFORM 3640-SELECT-FEE-PCT
               PERFORM 3650-CHECK-EXPIRY
               PERFORM 3660-ACCRUE-DEDUCT
               PERFORM 3670-FLAG-NET
               PERFORM 3680-LIMIT-FICA
               PERFORM 3690-AGE-EFFECTIVE
               PERFORM 3700-FLAG-ITEM
               PERFORM 3710-EDIT-CUST-ID
               PERFORM 3720-SET-STATUS
               PERFORM 3730-SELECT-FEE-PCT
               PERFORM 3740-ACCUM-REGION
               PERFORM 3750-SELECT-MARGIN
               PERFORM 3760-SPLIT-NET
               PERFORM 3770-CHECK-CLOSE
               PERFORM 3780-AGE-EXPIRY
               PERFORM 3790-SELECT-FEE-PCT
               PERFORM 3800-SELECT-MARGIN
               PERFORM 3810-PRORATE-NET
               PERFORM 3820-ASSESS-NOTICE-SENT
               PERFORM 3830-DESCRIBE-CHANNEL
               PERFORM 3840-CHECK-OPEN
               PERFORM 3850-SELECT-FEE-PCT
               PERFORM 3860-SELECT-FEE-PCT
               PERFORM 3870-SET-STATUS
               PERFORM 3880-LIMIT-FED-TAX
               PERFORM 3890-SET-SOURCE
               PERFORM 3900-CHECK-LAST-ACT
               PERFORM 3910-CHECK-EXPIRY
               PERFORM 3920-ASSESS-NOTICE-SENT
               PERFORM 3930-EDIT-MEMO
               PERFORM 3940-COUNT-ITEM
               PERFORM 3950-ACCUM-SOURCE
               PERFORM 3960-CHECK-POST
               PERFORM 3970-CHECK-OPEN
               PERFORM 3980-LIMIT-ITEM
               PERFORM 3990-DESCRIBE-TRAN
               PERFORM 4000-ASSESS-PREFERRED
               PERFORM 4010-DESCRIBE-TRAN
               PERFORM 4020-SELECT-MARGIN
               PERFORM 4030-SET-REASON
               PERFORM 4040-LIMIT-GROSS
               PERFORM 4050-COUNT-NOTICE
               PERFORM 4060-EDIT-REJECT
               PERFORM 4070-SELECT-RATE
               PERFORM 4080-SPLIT-NET
               PERFORM 4090-EDIT-NAME
               PERFORM 4100-FLAG-STATE-TAX
               PERFORM 4110-ACCUM-SOURCE
               PERFORM 4120-CHECK-CLOSE
               PERFORM 4130-ACCRUE-FICA
               PERFORM 4140-SET-REGION
               PERFORM 4150-EDIT-DESC
               PERFORM 4160-LIMIT-FED-TAX
               PERFORM 4170-FLAG-DEDUCT
               PERFORM 4180-SPLIT-FED-TAX
               PERFORM 4190-SPLIT-STATE-TAX
               PERFORM 4200-EDIT-ADDR
               PERFORM 4210-COUNT-SEQ
               PERFORM 4220-SELECT-FEE-PCT
               PERFORM 4230-CHECK-EFF
               PERFORM 4240-EDIT-ADDR
               PERFORM 4250-ACCRUE-GROSS
               PERFORM 4260-PRORATE-GROSS
               PERFORM 4270-PRORATE-STATE-TAX
               PERFORM 4280-EDIT-NAME
               PERFORM 4290-COUNT-SEQ
               PERFORM 4300-ASSESS-SUSPEND
               PERFORM 4310-COUNT-NOTICE
               PERFORM 4320-PRORATE-NET
               PERFORM 4330-SELECT-MARGIN
               PERFORM 4340-PRORATE-DEDUCT
               PERFORM 4350-PRORATE-GROSS
               PERFORM 4360-ACCRUE-DEDUCT
               PERFORM 4370-CHECK-NEXT-REVIEW
               PERFORM 4380-ASSESS-PREFERRED
               PERFORM 4390-FLAG-GROSS
               PERFORM 4400-ACCRUE-NET
               PERFORM 4410-CHECK-CLOSE
               PERFORM 4420-ACCRUE-RATE
               PERFORM 4430-PRORATE-NET
               PERFORM 4440-PRORATE-DEDUCT
               PERFORM 4450-ACCRUE-FED-TAX
               PERFORM 4460-LIMIT-STATE-TAX
               PERFORM 4470-FLAG-GROSS
               PERFORM 4480-ASSESS-PREFERRED
               PERFORM 4490-DESCRIBE-STATUS
               PERFORM 4500-CHECK-LAST-ACT
               PERFORM 4510-SPLIT-FED-TAX
               PERFORM 4520-FLAG-NET
               PERFORM 4530-PRORATE-FED-TAX
               PERFORM 7020-CALL-PYTIER
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
      * SPLIT STATE TAX YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3000-SPLIT-STATE-TAX.
           COMPUTE WS-WORK-AMT = PRLM-STATE-TAX-YTD / 2
           COMPUTE WS-WORK-AMT-2 = PRLM-STATE-TAX-YTD - (WS-WORK-AMT *
               2)
           COMPUTE PRLM-GROSS-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE STATE TAX FEE ON THE DEDUCT AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3010-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN PRLM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO PRLM-STATE-TAX-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PRLM-PREFERRED-FLAG = 'Y'
                   COMPUTE PRLM-STATE-TAX-AMT ROUNDED = PRLT-DEDUCT-AMT
                       * PRLM-FEE-PCT / 200
               WHEN PRLT-CHANNEL-CD = 'BRN'
                   MOVE 10.00 TO PRLM-STATE-TAX-AMT
               WHEN PRLT-CHANNEL-CD = 'BAT'
                   MOVE 10.00 TO PRLM-STATE-TAX-AMT
               WHEN OTHER
                   COMPUTE PRLM-STATE-TAX-AMT ROUNDED = PRLT-DEDUCT-AMT
                       * PRLM-FEE-PCT / 100
           END-EVALUATE
           IF PRLM-STATE-TAX-AMT > 0 AND PRLM-STATE-TAX-AMT < 2.00
               MOVE 2.00 TO PRLM-STATE-TAX-AMT
           END-IF
           IF PRLM-STATE-TAX-AMT > 35.00
               MOVE 35.00 TO PRLM-STATE-TAX-AMT
           END-IF
           ADD PRLM-STATE-TAX-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT DEDUCT AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3020-SPLIT-DEDUCT.
           COMPUTE WS-WORK-AMT = PRLT-DEDUCT-AMT / 4
           COMPUTE WS-WORK-AMT-2 = PRLT-DEDUCT-AMT - (WS-WORK-AMT * 4)
           COMPUTE PRLT-STATE-TAX-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * PR -> AD WHEN NOTICE DATE IS PAST
      * AD -> VD WHEN NOTICE DATE IS PAST
      * VD -> TX WHEN NOTICE DATE IS PAST
      * TX -> DP WHEN NOTICE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3030-SET-TRAN.
           EVALUATE PRLT-TRAN-CD
               WHEN 'PR'
                   IF PRLM-NOTICE-DATE > 0 AND PRLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AD' TO PRLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AD'
                   IF PRLM-NOTICE-DATE > 0 AND PRLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VD' TO PRLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VD'
                   IF PRLM-NOTICE-DATE > 0 AND PRLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TX' TO PRLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TX'
                   IF PRLM-NOTICE-DATE > 0 AND PRLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DP' TO PRLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT NET AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3040-SPLIT-NET.
           COMPUTE WS-WORK-AMT = PRLM-NET-AMT / 2
           COMPUTE WS-WORK-AMT-2 = PRLM-NET-AMT - (WS-WORK-AMT * 2)
           COMPUTE PRLM-GROSS-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE STATE TAX AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DEDUCT YTD.
      *----------------------------------------------------------------*
       3050-PRORATE-DEDUCT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PRLM-DEDUCT-YTD ROUNDED =
                   PRLM-STATE-TAX-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PRLM-DEDUCT-YTD
           END-IF
           ADD PRLM-DEDUCT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE FICA SCHEDX TIER FOR THE STATE TAX YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3060-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-FICA-SCHEDX-RATE(1) TO PRLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF PRLM-STATE-TAX-YTD NOT < WS-FICA-SCHEDX-LOW(WS-TX)
                   MOVE WS-FICA-SCHEDX-RATE(WS-TX) TO PRLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PRLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PRLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE GROSS AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3070-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PRLM-STATUS-CD
                   ADD PRLM-GROSS-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PRLM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PRLM-GROSS-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE NET AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3080-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN PRLM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PRLM-SUSPEND-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = PRLM-NET-AMT *
                       PRLM-FEE-PCT / 200
               WHEN PRLM-TAX-STATE-CD = 'CAZ'
                   MOVE 7.00 TO WS-WORK-AMT-2
               WHEN PRLM-TAX-STATE-CD = 'LZU'
                   MOVE 7.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = PRLM-NET-AMT *
                       PRLM-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 5.00
               MOVE 5.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 50.00
               MOVE 50.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * NET AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3090-FLAG-NET.
           IF PRLT-NET-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PRLT-NET-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE DEDUCT YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3100-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN PRLM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PRLT-REVERSAL-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = PRLM-DEDUCT-YTD *
                       PRLM-FEE-PCT / 200
               WHEN PRLM-REASON-CD = 'B'
                   MOVE 15.00 TO WS-WORK-AMT
               WHEN PRLM-REASON-CD = 'C'
                   MOVE 12.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = PRLM-DEDUCT-YTD *
                       PRLM-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 5.00
               MOVE 5.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 75.00
               MOVE 75.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 365 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3110-CHECK-EXPIRY.
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
               MOVE 'Y' TO PRLM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PAY FREQ CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3120-DESCRIBE-PAY-FREQ.
           EVALUATE PRLM-PAY-FREQ-CD
               WHEN 'JT'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'XM'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'ZT'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STATE TAX YTD AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3130-ACCRUE-STATE-TAX.
           IF PRLM-STATE-TAX-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PRLM-STATE-TAX-YTD * PRLM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO PRLM-STATE-TAX-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3140-AGE-LAST-PAY.
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
               WHEN WS-DAYS NOT > 15
                   ADD PRLM-GROSS-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD PRLM-GROSS-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PRLM-GROSS-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PRLM-GROSS-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE FICA FEE ON THE NET AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3150-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN PRLM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO PRLT-FICA-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PRLM-PREFERRED-FLAG = 'Y'
                   COMPUTE PRLT-FICA-AMT ROUNDED = PRLM-NET-AMT *
                       PRLM-MARGIN / 200
               WHEN PRLM-REASON-CD = 'B'
                   MOVE 20.00 TO PRLT-FICA-AMT
               WHEN PRLM-REASON-CD = 'C'
                   MOVE 7.00 TO PRLT-FICA-AMT
               WHEN OTHER
                   COMPUTE PRLT-FICA-AMT ROUNDED = PRLM-NET-AMT *
                       PRLM-MARGIN / 100
           END-EVALUATE
           IF PRLT-FICA-AMT > 0 AND PRLT-FICA-AMT < 2.00
               MOVE 2.00 TO PRLT-FICA-AMT
           END-IF
           IF PRLT-FICA-AMT > 50.00
               MOVE 50.00 TO PRLT-FICA-AMT
           END-IF
           ADD PRLT-FICA-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 365 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3160-CHECK-NEXT-REVIEW.
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
      * GROSS YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3170-FLAG-GROSS.
           IF PRLM-GROSS-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PRLM-GROSS-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DEDUCT AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3180-SPLIT-DEDUCT.
           COMPUTE WS-WORK-AMT = PRLM-DEDUCT-AMT / 4
           COMPUTE WS-WORK-AMT-2 = PRLM-DEDUCT-AMT - (WS-WORK-AMT * 4)
           COMPUTE PRLM-STATE-TAX-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FED TAX YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3190-FLAG-FED-TAX.
           IF PRLM-FED-TAX-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PRLM-FED-TAX-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3200-AGE-EFFECTIVE.
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
                   ADD PRLM-FED-TAX-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PRLM-FED-TAX-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD PRLM-FED-TAX-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PRLM-FED-TAX-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PRLM-FED-TAX-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON NET AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3210-ACCRUE-NET.
           IF PRLM-NET-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PRLM-NET-AMT * PRLM-RATE / 100 / 365
               ADD WS-WORK-AMT TO PRLM-GROSS-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FICA AMT BY TAX STATE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3220-ACCUM-TAX-STATE.
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
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3230-COUNT-CYCLE.
           IF PRLM-CYCLE-CNT < 9999999
               ADD 1 TO PRLM-CYCLE-CNT
           END-IF
           IF PRLM-CYCLE-CNT NOT < 10
               MOVE 'Y' TO PRLM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E269 CONTACT REQUIRED
      * E684 WORK AMT 2 NEGATIVE
      * E405 WORK AMT 2 OVER 99999
      * E441 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3240-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PRLM-CONTACT = SPACES
                   MOVE 'E269' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E684' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 99999.00
                   MOVE 'E405' TO WS-REJECT-CODE
               WHEN PRLM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E441' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PAY FREQ CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3250-DESCRIBE-PAY-FREQ.
           EVALUATE PRLM-PAY-FREQ-CD
               WHEN 'JT'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'XM'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'ZT'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATE TAX AMT IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3260-LIMIT-STATE-TAX.
           IF PRLM-STATE-TAX-AMT < 50.00
               MOVE 50.00 TO PRLM-STATE-TAX-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PRLM-STATE-TAX-AMT > 5000.00
               MOVE 5000.00 TO PRLM-STATE-TAX-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DEDUCT YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3270-FLAG-DEDUCT.
           IF PRLM-DEDUCT-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PRLM-DEDUCT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN EFF DATE IS PAST
      * BAT -> MAL WHEN EFF DATE IS PAST
      * MAL -> TEL WHEN EFF DATE IS PAST
      * TEL -> ONL WHEN EFF DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3280-SET-CHANNEL.
           EVALUATE PRLT-CHANNEL-CD
               WHEN 'BRN'
                   IF PRLT-EFF-DATE > 0 AND PRLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO PRLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF PRLT-EFF-DATE > 0 AND PRLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO PRLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF PRLT-EFF-DATE > 0 AND PRLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO PRLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF PRLT-EFF-DATE > 0 AND PRLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO PRLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TAX STATE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3290-DESCRIBE-TAX-STATE.
           EVALUATE PRLM-TAX-STATE-CD
               WHEN 'CAZ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'LZU'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'TUF'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'WKE'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3300-COUNT-ITEM.
           IF PRLM-ITEM-CNT < 9999999
               ADD 1 TO PRLM-ITEM-CNT
           END-IF
           IF PRLM-ITEM-CNT NOT < 5
               MOVE 'Y' TO PRLM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE NET YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO STATE TAX AMT.
      *----------------------------------------------------------------*
       3310-PRORATE-STATE-TAX.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PRLM-STATE-TAX-AMT ROUNDED =
                   PRLM-NET-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PRLM-STATE-TAX-AMT
           END-IF
           ADD PRLM-STATE-TAX-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT FICA AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3320-SPLIT-FICA.
           COMPUTE WS-WORK-AMT = PRLT-FICA-AMT / 12
           COMPUTE WS-WORK-AMT-2 = PRLT-FICA-AMT - (WS-WORK-AMT * 12)
           COMPUTE PRLM-FICA-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3330-AGE-EXPIRY.
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
                   ADD PRLM-NET-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD PRLM-NET-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD PRLM-NET-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD PRLM-NET-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PRLM-NET-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DEDUCT AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3340-ACCRUE-DEDUCT.
           IF PRLT-DEDUCT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PRLT-DEDUCT-AMT * PRLM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FICA SCHED TIER FOR THE FED TAX YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3350-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-FICA-SCHED-RATE(1) TO PRLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF PRLM-FED-TAX-YTD NOT < WS-FICA-SCHED-LOW(WS-TX)
                   MOVE WS-FICA-SCHED-RATE(WS-TX) TO PRLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PRLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PRLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3360-AGE-POST.
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
                   ADD PRLM-FED-TAX-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD PRLM-FED-TAX-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD PRLM-FED-TAX-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD PRLM-FED-TAX-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PRLM-FED-TAX-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE NET YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3370-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN PRLM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO PRLM-NET-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PRLM-HOLD-FLAG = 'Y'
                   COMPUTE PRLM-NET-YTD ROUNDED = WS-WORK-AMT-2 *
                       PRLM-MARGIN / 200
               WHEN PRLM-REGION-CD = 'DP'
                   MOVE 10.00 TO PRLM-NET-YTD
               WHEN PRLM-REGION-CD = 'TU'
                   MOVE 10.00 TO PRLM-NET-YTD
               WHEN OTHER
                   COMPUTE PRLM-NET-YTD ROUNDED = WS-WORK-AMT-2 *
                       PRLM-MARGIN / 100
           END-EVALUATE
           IF PRLM-NET-YTD > 0 AND PRLM-NET-YTD < 10.00
               MOVE 10.00 TO PRLM-NET-YTD
           END-IF
           IF PRLM-NET-YTD > 250.00
               MOVE 250.00 TO PRLM-NET-YTD
           END-IF
           ADD PRLM-NET-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE NET AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3380-PRORATE-NET.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PRLM-NET-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT FICA YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3390-SPLIT-FICA.
           COMPUTE WS-WORK-AMT = PRLM-FICA-YTD / 3
           COMPUTE WS-WORK-AMT-2 = PRLM-FICA-YTD - (WS-WORK-AMT * 3)
           COMPUTE PRLT-DEDUCT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FED TAX AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3400-FLAG-FED-TAX.
           IF PRLM-FED-TAX-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PRLM-FED-TAX-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FICA SCHED TIER FOR THE NET AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3410-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-FICA-SCHED-RATE(1) TO PRLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF PRLM-NET-AMT NOT < WS-FICA-SCHED-LOW(WS-TX)
                   MOVE WS-FICA-SCHED-RATE(WS-TX) TO PRLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PRLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PRLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E115 DESC REQUIRED
      * E616 DEDUCT AMT NEGATIVE
      * E214 DEDUCT AMT OVER 50000
      * E919 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3420-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PRLM-DESC = SPACES
                   MOVE 'E115' TO WS-REJECT-CODE
               WHEN PRLM-DEDUCT-AMT < 0
                   MOVE 'E616' TO WS-REJECT-CODE
               WHEN PRLM-DEDUCT-AMT > 50000.00
                   MOVE 'E214' TO WS-REJECT-CODE
               WHEN PRLM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E919' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E407 REJECT CD REQUIRED
      * E517 NET AMT NEGATIVE
      * E642 NET AMT OVER 50000
      * E007 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3430-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PRLT-REJECT-CD = SPACES
                   MOVE 'E407' TO WS-REJECT-CODE
               WHEN PRLM-NET-AMT < 0
                   MOVE 'E517' TO WS-REJECT-CODE
               WHEN PRLM-NET-AMT > 50000.00
                   MOVE 'E642' TO WS-REJECT-CODE
               WHEN PRLM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E007' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3440-LIMIT-ITEM.
           IF WS-WORK-AMT < 50.00
               MOVE 50.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 50000.00
               MOVE 50000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PAY FREQ CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3450-DESCRIBE-PAY-FREQ.
           EVALUATE PRLM-PAY-FREQ-CD
               WHEN 'JT'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'XM'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ZT'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E247 CONTACT REQUIRED
      * E058 FICA AMT NEGATIVE
      * E443 FICA AMT OVER 250000
      * E744 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3460-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PRLM-CONTACT = SPACES
                   MOVE 'E247' TO WS-REJECT-CODE
               WHEN PRLM-FICA-AMT < 0
                   MOVE 'E058' TO WS-REJECT-CODE
               WHEN PRLM-FICA-AMT > 250000.00
                   MOVE 'E443' TO WS-REJECT-CODE
               WHEN PRLM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E744' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON NET AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3470-ACCRUE-NET.
           IF PRLT-NET-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PRLT-NET-AMT * PRLM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO PRLM-DEDUCT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * NY -> PL WHEN EXPIRY DATE IS PAST
      * PL -> VK WHEN EXPIRY DATE IS PAST
      * VK -> WB WHEN EXPIRY DATE IS PAST
      * WB -> XL WHEN EXPIRY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3480-SET-SOURCE.
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
      * GROSS YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3490-FLAG-GROSS.
           IF PRLM-GROSS-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PRLM-GROSS-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATE TAX AMT IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3500-LIMIT-STATE-TAX.
           IF PRLM-STATE-TAX-AMT < 0.00
               MOVE 0.00 TO PRLM-STATE-TAX-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PRLM-STATE-TAX-AMT > 500.00
               MOVE 500.00 TO PRLM-STATE-TAX-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DEDUCT AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3510-ACCRUE-DEDUCT.
           IF PRLM-DEDUCT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PRLM-DEDUCT-AMT * PRLM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FICA SCHEDX TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3520-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FICA-SCHEDX-RATE(1) TO PRLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF WS-WORK-AMT NOT < WS-FICA-SCHEDX-LOW(WS-TX)
                   MOVE WS-FICA-SCHEDX-RATE(WS-TX) TO PRLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PRLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PRLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3530-DESCRIBE-STATUS.
           EVALUATE PRLM-STATUS-CD
               WHEN 'MV'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'RD'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'ST'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'TJ'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'UC'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DEDUCT AMT IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3540-LIMIT-DEDUCT.
           IF PRLM-DEDUCT-AMT < 10.00
               MOVE 10.00 TO PRLM-DEDUCT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PRLM-DEDUCT-AMT > 10000.00
               MOVE 10000.00 TO PRLM-DEDUCT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FICA YTD FEE ON THE STATE TAX AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3550-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN PRLM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO PRLM-FICA-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PRLM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE PRLM-FICA-YTD ROUNDED = PRLM-STATE-TAX-AMT *
                       PRLM-RATE / 200
               WHEN PRLM-PAY-FREQ-CD = 'JT'
                   MOVE 15.00 TO PRLM-FICA-YTD
               WHEN PRLM-PAY-FREQ-CD = 'XM'
                   MOVE 5.00 TO PRLM-FICA-YTD
               WHEN OTHER
                   COMPUTE PRLM-FICA-YTD ROUNDED = PRLM-STATE-TAX-AMT *
                       PRLM-RATE / 100
           END-EVALUATE
           IF PRLM-FICA-YTD > 0 AND PRLM-FICA-YTD < 5.00
               MOVE 5.00 TO PRLM-FICA-YTD
           END-IF
           IF PRLM-FICA-YTD > 50.00
               MOVE 50.00 TO PRLM-FICA-YTD
           END-IF
           ADD PRLM-FICA-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * MV -> RD WHEN OPEN DATE IS PAST
      * RD -> ST WHEN OPEN DATE IS PAST
      * ST -> TJ WHEN OPEN DATE IS PAST
      * TJ -> UC WHEN OPEN DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3560-SET-STATUS.
           EVALUATE PRLM-STATUS-CD
               WHEN 'MV'
                   IF PRLM-OPEN-DATE > 0 AND PRLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RD' TO PRLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RD'
                   IF PRLM-OPEN-DATE > 0 AND PRLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ST' TO PRLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ST'
                   IF PRLM-OPEN-DATE > 0 AND PRLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TJ' TO PRLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TJ'
                   IF PRLM-OPEN-DATE > 0 AND PRLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UC' TO PRLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE FICA SCHEDX TIER FOR THE FICA AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3570-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-FICA-SCHEDX-RATE(1) TO PRLM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF PRLT-FICA-AMT NOT < WS-FICA-SCHEDX-LOW(WS-TX)
                   MOVE WS-FICA-SCHEDX-RATE(WS-TX) TO PRLM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PRLM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PRLM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FICA SCHEDX TIER FOR THE DEDUCT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3580-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-FICA-SCHEDX-RATE(1) TO PRLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF PRLT-DEDUCT-AMT NOT < WS-FICA-SCHEDX-LOW(WS-TX)
                   MOVE WS-FICA-SCHEDX-RATE(WS-TX) TO PRLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PRLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PRLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DEDUCT AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3590-ACCRUE-DEDUCT.
           IF PRLM-DEDUCT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PRLM-DEDUCT-AMT * PRLM-RATE / 100 / 365
               ADD WS-WORK-AMT TO PRLM-NET-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FICA FEE ON THE DEDUCT AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3600-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN PRLM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO PRLM-FICA-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PRLM-PREFERRED-FLAG = 'Y'
                   COMPUTE PRLM-FICA-AMT ROUNDED = PRLM-DEDUCT-AMT *
                       PRLM-FEE-PCT / 200
               WHEN PRLM-SOURCE-CD = 'NY'
                   MOVE 5.00 TO PRLM-FICA-AMT
               WHEN PRLM-SOURCE-CD = 'PL'
                   MOVE 15.00 TO PRLM-FICA-AMT
               WHEN OTHER
                   COMPUTE PRLM-FICA-AMT ROUNDED = PRLM-DEDUCT-AMT *
                       PRLM-FEE-PCT / 100
           END-EVALUATE
           IF PRLM-FICA-AMT > 0 AND PRLM-FICA-AMT < 2.00
               MOVE 2.00 TO PRLM-FICA-AMT
           END-IF
           IF PRLM-FICA-AMT > 75.00
               MOVE 75.00 TO PRLM-FICA-AMT
           END-IF
           ADD PRLM-FICA-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3610-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PRLM-SOURCE-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PRLM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 10 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3620-CHECK-POST.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO PRLM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
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
      * SELECT THE FICA SCHEDX TIER FOR THE DEDUCT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3640-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FICA-SCHEDX-RATE(1) TO PRLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF PRLM-DEDUCT-AMT NOT < WS-FICA-SCHEDX-LOW(WS-TX)
                   MOVE WS-FICA-SCHEDX-RATE(WS-TX) TO PRLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PRLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PRLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 45 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3650-CHECK-EXPIRY.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO PRLM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DEDUCT YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3660-ACCRUE-DEDUCT.
           IF PRLM-DEDUCT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PRLM-DEDUCT-YTD * PRLM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO PRLT-STATE-TAX-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NET AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3670-FLAG-NET.
           IF PRLM-NET-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PRLM-NET-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FICA AMT IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3680-LIMIT-FICA.
           IF PRLT-FICA-AMT < 100.00
               MOVE 100.00 TO PRLT-FICA-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PRLT-FICA-AMT > 10000.00
               MOVE 10000.00 TO PRLT-FICA-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3690-AGE-EFFECTIVE.
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
                   ADD PRLM-NET-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD PRLM-NET-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD PRLM-NET-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD PRLM-NET-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PRLM-NET-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3700-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E461 CUST ID REQUIRED
      * E828 NET AMT NEGATIVE
      * E517 NET AMT OVER 250000
      * E200 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3710-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PRLM-CUST-ID = SPACES
                   MOVE 'E461' TO WS-REJECT-CODE
               WHEN PRLM-NET-AMT < 0
                   MOVE 'E828' TO WS-REJECT-CODE
               WHEN PRLM-NET-AMT > 250000.00
                   MOVE 'E517' TO WS-REJECT-CODE
               WHEN PRLT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E200' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * MV -> RD WHEN EXPIRY DATE IS PAST
      * RD -> ST WHEN EXPIRY DATE IS PAST
      * ST -> TJ WHEN EXPIRY DATE IS PAST
      * TJ -> UC WHEN EXPIRY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3720-SET-STATUS.
           EVALUATE PRLM-STATUS-CD
               WHEN 'MV'
                   IF PRLM-EXPIRY-DATE > 0 AND PRLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RD' TO PRLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RD'
                   IF PRLM-EXPIRY-DATE > 0 AND PRLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ST' TO PRLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ST'
                   IF PRLM-EXPIRY-DATE > 0 AND PRLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TJ' TO PRLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TJ'
                   IF PRLM-EXPIRY-DATE > 0 AND PRLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UC' TO PRLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE FICA SCHED TIER FOR THE GROSS AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3730-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FICA-SCHED-RATE(1) TO PRLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF PRLM-GROSS-AMT NOT < WS-FICA-SCHED-LOW(WS-TX)
                   MOVE WS-FICA-SCHED-RATE(WS-TX) TO PRLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PRLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PRLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE DEDUCT AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3740-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PRLM-REGION-CD
                   ADD PRLM-DEDUCT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PRLM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PRLM-DEDUCT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FICA SCHEDX TIER FOR THE NET YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3750-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-FICA-SCHEDX-RATE(1) TO PRLM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF PRLM-NET-YTD NOT < WS-FICA-SCHEDX-LOW(WS-TX)
                   MOVE WS-FICA-SCHEDX-RATE(WS-TX) TO PRLM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PRLM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PRLM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT NET YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3760-SPLIT-NET.
           COMPUTE WS-WORK-AMT = PRLM-NET-YTD / 3
           COMPUTE WS-WORK-AMT-2 = PRLM-NET-YTD - (WS-WORK-AMT * 3)
           COMPUTE PRLM-GROSS-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3770-CHECK-CLOSE.
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
               MOVE 'Y' TO PRLT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3780-AGE-EXPIRY.
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
      * SELECT THE FICA SCHEDX TIER FOR THE GROSS AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3790-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FICA-SCHEDX-RATE(1) TO PRLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF PRLM-GROSS-AMT NOT < WS-FICA-SCHEDX-LOW(WS-TX)
                   MOVE WS-FICA-SCHEDX-RATE(WS-TX) TO PRLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PRLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PRLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FICA SCHEDX TIER FOR THE NET AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3800-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-FICA-SCHEDX-RATE(1) TO PRLM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF PRLT-NET-AMT NOT < WS-FICA-SCHEDX-LOW(WS-TX)
                   MOVE WS-FICA-SCHEDX-RATE(WS-TX) TO PRLM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PRLM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PRLM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE NET AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO NET AMT.
      *----------------------------------------------------------------*
       3810-PRORATE-NET.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PRLT-NET-AMT ROUNDED =
                   PRLM-NET-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PRLT-NET-AMT
           END-IF
           ADD PRLT-NET-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE FICA FEE ON THE STATE TAX YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3820-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN PRLM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO PRLM-FICA-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PRLM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE PRLM-FICA-AMT ROUNDED = PRLM-STATE-TAX-YTD *
                       PRLM-MARGIN / 200
               WHEN PRLT-CHANNEL-CD = 'BRN'
                   MOVE 20.00 TO PRLM-FICA-AMT
               WHEN PRLT-CHANNEL-CD = 'BAT'
                   MOVE 3.00 TO PRLM-FICA-AMT
               WHEN OTHER
                   COMPUTE PRLM-FICA-AMT ROUNDED = PRLM-STATE-TAX-YTD *
                       PRLM-MARGIN / 100
           END-EVALUATE
           IF PRLM-FICA-AMT > 0 AND PRLM-FICA-AMT < 2.00
               MOVE 2.00 TO PRLM-FICA-AMT
           END-IF
           IF PRLM-FICA-AMT > 35.00
               MOVE 35.00 TO PRLM-FICA-AMT
           END-IF
           ADD PRLM-FICA-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3830-DESCRIBE-CHANNEL.
           EVALUATE PRLT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 120 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
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
           IF WS-DAYS > 120
               MOVE 'Y' TO PRLM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FICA SCHED TIER FOR THE FED TAX YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3850-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FICA-SCHED-RATE(1) TO PRLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
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
      * SELECT THE FICA SCHEDX TIER FOR THE NET YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3860-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FICA-SCHEDX-RATE(1) TO PRLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF PRLM-NET-YTD NOT < WS-FICA-SCHEDX-LOW(WS-TX)
                   MOVE WS-FICA-SCHEDX-RATE(WS-TX) TO PRLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PRLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PRLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * MV -> RD WHEN LAST PAY DATE IS PAST
      * RD -> ST WHEN LAST PAY DATE IS PAST
      * ST -> TJ WHEN LAST PAY DATE IS PAST
      * TJ -> UC WHEN LAST PAY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3870-SET-STATUS.
           EVALUATE PRLM-STATUS-CD
               WHEN 'MV'
                   IF PRLM-LAST-PAY-DATE > 0 AND PRLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RD' TO PRLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RD'
                   IF PRLM-LAST-PAY-DATE > 0 AND PRLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ST' TO PRLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ST'
                   IF PRLM-LAST-PAY-DATE > 0 AND PRLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TJ' TO PRLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TJ'
                   IF PRLM-LAST-PAY-DATE > 0 AND PRLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UC' TO PRLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FED TAX YTD IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3880-LIMIT-FED-TAX.
           IF PRLM-FED-TAX-YTD < 50.00
               MOVE 50.00 TO PRLM-FED-TAX-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PRLM-FED-TAX-YTD > 500.00
               MOVE 500.00 TO PRLM-FED-TAX-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * NY -> PL WHEN EFF DATE IS PAST
      * PL -> VK WHEN EFF DATE IS PAST
      * VK -> WB WHEN EFF DATE IS PAST
      * WB -> XL WHEN EFF DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3890-SET-SOURCE.
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
      * DAYS SINCE LAST ACT DATE. MORE THAN 15 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3900-CHECK-LAST-ACT.
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
               MOVE 'Y' TO PRLM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3910-CHECK-EXPIRY.
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
           IF WS-DAYS > 120
               MOVE 'Y' TO PRLT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FICA YTD FEE ON THE FED TAX YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3920-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN PRLM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO PRLM-FICA-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PRLM-STOP-FLAG = 'Y'
                   COMPUTE PRLM-FICA-YTD ROUNDED = PRLM-FED-TAX-YTD *
                       PRLM-RATE / 200
               WHEN PRLM-SOURCE-CD = 'NY'
                   MOVE 3.00 TO PRLM-FICA-YTD
               WHEN PRLM-SOURCE-CD = 'PL'
                   MOVE 10.00 TO PRLM-FICA-YTD
               WHEN OTHER
                   COMPUTE PRLM-FICA-YTD ROUNDED = PRLM-FED-TAX-YTD *
                       PRLM-RATE / 100
           END-EVALUATE
           IF PRLM-FICA-YTD > 0 AND PRLM-FICA-YTD < 10.00
               MOVE 10.00 TO PRLM-FICA-YTD
           END-IF
           IF PRLM-FICA-YTD > 75.00
               MOVE 75.00 TO PRLM-FICA-YTD
           END-IF
           ADD PRLM-FICA-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E082 MEMO REQUIRED
      * E247 STATE TAX AMT NEGATIVE
      * E284 STATE TAX AMT OVER 10000
      * E391 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3930-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PRLT-MEMO = SPACES
                   MOVE 'E082' TO WS-REJECT-CODE
               WHEN PRLM-STATE-TAX-AMT < 0
                   MOVE 'E247' TO WS-REJECT-CODE
               WHEN PRLM-STATE-TAX-AMT > 10000.00
                   MOVE 'E284' TO WS-REJECT-CODE
               WHEN PRLT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E391' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3940-COUNT-ITEM.
           IF PRLM-ITEM-CNT < 9999999
               ADD 1 TO PRLM-ITEM-CNT
           END-IF
           IF PRLM-ITEM-CNT NOT < 5
               MOVE 'Y' TO PRLT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE DEDUCT AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3950-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PRLM-SOURCE-CD
                   ADD PRLM-DEDUCT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PRLM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PRLM-DEDUCT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 60 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3960-CHECK-POST.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO PRLM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 180 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3970-CHECK-OPEN.
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
           IF WS-DAYS > 180
               MOVE 'Y' TO PRLM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3980-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 50.00
               MOVE 50.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 2500.00
               MOVE 2500.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3990-DESCRIBE-TRAN.
           EVALUATE PRLT-TRAN-CD
               WHEN 'PR'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'AD'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'VD'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'TX'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'DP'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE DEDUCT YTD FEE ON THE GROSS AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4000-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN PRLM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO PRLM-DEDUCT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PRLM-SUSPEND-FLAG = 'Y'
                   COMPUTE PRLM-DEDUCT-YTD ROUNDED = PRLM-GROSS-AMT *
                       PRLM-MARGIN / 200
               WHEN PRLM-PAY-FREQ-CD = 'JT'
                   MOVE 5.00 TO PRLM-DEDUCT-YTD
               WHEN PRLM-PAY-FREQ-CD = 'XM'
                   MOVE 3.00 TO PRLM-DEDUCT-YTD
               WHEN OTHER
                   COMPUTE PRLM-DEDUCT-YTD ROUNDED = PRLM-GROSS-AMT *
                       PRLM-MARGIN / 100
           END-EVALUATE
           IF PRLM-DEDUCT-YTD > 0 AND PRLM-DEDUCT-YTD < 1.00
               MOVE 1.00 TO PRLM-DEDUCT-YTD
           END-IF
           IF PRLM-DEDUCT-YTD > 75.00
               MOVE 75.00 TO PRLM-DEDUCT-YTD
           END-IF
           ADD PRLM-DEDUCT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4010-DESCRIBE-TRAN.
           EVALUATE PRLT-TRAN-CD
               WHEN 'PR'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'AD'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'VD'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'TX'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'DP'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE FICA SCHED TIER FOR THE DEDUCT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4020-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-FICA-SCHED-RATE(1) TO PRLM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF PRLM-DEDUCT-AMT NOT < WS-FICA-SCHED-LOW(WS-TX)
                   MOVE WS-FICA-SCHED-RATE(WS-TX) TO PRLM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PRLM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PRLM-MARGIN
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
       4030-SET-REASON.
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
      * GROSS YTD IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4040-LIMIT-GROSS.
           IF PRLM-GROSS-YTD < 100.00
               MOVE 100.00 TO PRLM-GROSS-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PRLM-GROSS-YTD > 500.00
               MOVE 500.00 TO PRLM-GROSS-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       4050-COUNT-NOTICE.
           IF PRLM-NOTICE-CNT < 9999999
               ADD 1 TO PRLM-NOTICE-CNT
           END-IF
           IF PRLM-NOTICE-CNT NOT < 10
               MOVE 'Y' TO PRLM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E717 REJECT CD REQUIRED
      * E636 NET AMT NEGATIVE
      * E931 NET AMT OVER 1000000
      * E285 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       4060-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PRLT-REJECT-CD = SPACES
                   MOVE 'E717' TO WS-REJECT-CODE
               WHEN PRLM-NET-AMT < 0
                   MOVE 'E636' TO WS-REJECT-CODE
               WHEN PRLM-NET-AMT > 1000000.00
                   MOVE 'E931' TO WS-REJECT-CODE
               WHEN PRLM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E285' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FICA SCHEDX TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4070-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-FICA-SCHEDX-RATE(1) TO PRLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF WS-WORK-AMT NOT < WS-FICA-SCHEDX-LOW(WS-TX)
                   MOVE WS-FICA-SCHEDX-RATE(WS-TX) TO PRLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PRLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PRLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT NET YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4080-SPLIT-NET.
           COMPUTE WS-WORK-AMT = PRLM-NET-YTD / 2
           COMPUTE WS-WORK-AMT-2 = PRLM-NET-YTD - (WS-WORK-AMT * 2)
           COMPUTE PRLM-DEDUCT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E351 NAME REQUIRED
      * E737 WORK AMT NEGATIVE
      * E588 WORK AMT OVER 5000
      * E398 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       4090-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PRLM-NAME = SPACES
                   MOVE 'E351' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E737' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 5000.00
                   MOVE 'E588' TO WS-REJECT-CODE
               WHEN PRLM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E398' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATE TAX AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4100-FLAG-STATE-TAX.
           IF PRLT-STATE-TAX-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PRLT-STATE-TAX-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE STATE TAX AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4110-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PRLM-SOURCE-CD
                   ADD PRLT-STATE-TAX-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PRLM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PRLT-STATE-TAX-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4120-CHECK-CLOSE.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO PRLM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FICA AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4130-ACCRUE-FICA.
           IF PRLM-FICA-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PRLM-FICA-AMT * PRLM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO PRLM-GROSS-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * DP -> TU WHEN CLOSE DATE IS PAST
      * TU -> WD WHEN CLOSE DATE IS PAST
      * WD -> DP WHEN CLOSE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4140-SET-REGION.
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
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E110 DESC REQUIRED
      * E820 NET YTD NEGATIVE
      * E356 NET YTD OVER 1000000
      * E346 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       4150-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PRLM-DESC = SPACES
                   MOVE 'E110' TO WS-REJECT-CODE
               WHEN PRLM-NET-YTD < 0
                   MOVE 'E820' TO WS-REJECT-CODE
               WHEN PRLM-NET-YTD > 1000000.00
                   MOVE 'E356' TO WS-REJECT-CODE
               WHEN PRLM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E346' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FED TAX YTD IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4160-LIMIT-FED-TAX.
           IF PRLM-FED-TAX-YTD < 100.00
               MOVE 100.00 TO PRLM-FED-TAX-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PRLM-FED-TAX-YTD > 2500.00
               MOVE 2500.00 TO PRLM-FED-TAX-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DEDUCT AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4170-FLAG-DEDUCT.
           IF PRLM-DEDUCT-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PRLM-DEDUCT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FED TAX AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4180-SPLIT-FED-TAX.
           COMPUTE WS-WORK-AMT = PRLM-FED-TAX-AMT / 4
           COMPUTE WS-WORK-AMT-2 = PRLM-FED-TAX-AMT - (WS-WORK-AMT * 4)
           COMPUTE PRLM-NET-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT STATE TAX AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4190-SPLIT-STATE-TAX.
           COMPUTE WS-WORK-AMT = PRLM-STATE-TAX-AMT / 3
           COMPUTE WS-WORK-AMT-2 = PRLM-STATE-TAX-AMT - (WS-WORK-AMT *
               3)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E926 ADDR REQUIRED
      * E534 NET AMT NEGATIVE
      * E583 NET AMT OVER 250000
      * E950 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       4200-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PRLM-ADDR = SPACES
                   MOVE 'E926' TO WS-REJECT-CODE
               WHEN PRLM-NET-AMT < 0
                   MOVE 'E534' TO WS-REJECT-CODE
               WHEN PRLM-NET-AMT > 250000.00
                   MOVE 'E583' TO WS-REJECT-CODE
               WHEN PRLT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E950' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4210-COUNT-SEQ.
           IF PRLT-SEQ < 9999999
               ADD 1 TO PRLT-SEQ
           END-IF
           IF PRLT-SEQ NOT < 10
               MOVE 'Y' TO PRLT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FICA SCHEDX TIER FOR THE FED TAX YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4220-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FICA-SCHEDX-RATE(1) TO PRLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF PRLM-FED-TAX-YTD NOT < WS-FICA-SCHEDX-LOW(WS-TX)
                   MOVE WS-FICA-SCHEDX-RATE(WS-TX) TO PRLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PRLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PRLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 120 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4230-CHECK-EFF.
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
           IF WS-DAYS > 120
               MOVE 'Y' TO PRLM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E660 ADDR REQUIRED
      * E704 WORK AMT NEGATIVE
      * E233 WORK AMT OVER 1000000
      * E453 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       4240-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PRLM-ADDR = SPACES
                   MOVE 'E660' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E704' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 1000000.00
                   MOVE 'E233' TO WS-REJECT-CODE
               WHEN PRLM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E453' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON GROSS YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4250-ACCRUE-GROSS.
           IF PRLM-GROSS-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PRLM-GROSS-YTD * PRLM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO PRLM-FICA-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FICA YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO GROSS YTD.
      *----------------------------------------------------------------*
       4260-PRORATE-GROSS.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PRLM-GROSS-YTD ROUNDED =
                   PRLM-FICA-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PRLM-GROSS-YTD
           END-IF
           ADD PRLM-GROSS-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE DEDUCT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO STATE TAX YTD.
      *----------------------------------------------------------------*
       4270-PRORATE-STATE-TAX.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PRLM-STATE-TAX-YTD ROUNDED =
                   PRLM-DEDUCT-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PRLM-STATE-TAX-YTD
           END-IF
           ADD PRLM-STATE-TAX-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E619 NAME REQUIRED
      * E532 WORK AMT NEGATIVE
      * E517 WORK AMT OVER 5000
      * E435 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       4280-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PRLM-NAME = SPACES
                   MOVE 'E619' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E532' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 5000.00
                   MOVE 'E517' TO WS-REJECT-CODE
               WHEN PRLM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E435' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       4290-COUNT-SEQ.
           IF PRLT-SEQ < 9999999
               ADD 1 TO PRLT-SEQ
           END-IF
           IF PRLT-SEQ NOT < 6
               MOVE 'Y' TO PRLM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE NET FEE ON THE DEDUCT AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4300-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN PRLM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO PRLT-NET-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PRLM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE PRLT-NET-AMT ROUNDED = PRLM-DEDUCT-AMT *
                       PRLM-MARGIN / 200
               WHEN PRLT-TRAN-CD = 'PR'
                   MOVE 15.00 TO PRLT-NET-AMT
               WHEN PRLT-TRAN-CD = 'AD'
                   MOVE 20.00 TO PRLT-NET-AMT
               WHEN OTHER
                   COMPUTE PRLT-NET-AMT ROUNDED = PRLM-DEDUCT-AMT *
                       PRLM-MARGIN / 100
           END-EVALUATE
           IF PRLT-NET-AMT > 0 AND PRLT-NET-AMT < 5.00
               MOVE 5.00 TO PRLT-NET-AMT
           END-IF
           IF PRLT-NET-AMT > 75.00
               MOVE 75.00 TO PRLT-NET-AMT
           END-IF
           ADD PRLT-NET-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       4310-COUNT-NOTICE.
           IF PRLM-NOTICE-CNT < 9999999
               ADD 1 TO PRLM-NOTICE-CNT
           END-IF
           IF PRLM-NOTICE-CNT NOT < 6
               MOVE 'Y' TO PRLM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE STATE TAX YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO NET AMT.
      *----------------------------------------------------------------*
       4320-PRORATE-NET.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PRLT-NET-AMT ROUNDED =
                   PRLM-STATE-TAX-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PRLT-NET-AMT
           END-IF
           ADD PRLT-NET-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE FICA SCHED TIER FOR THE GROSS YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4330-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-FICA-SCHED-RATE(1) TO PRLM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF PRLM-GROSS-YTD NOT < WS-FICA-SCHED-LOW(WS-TX)
                   MOVE WS-FICA-SCHED-RATE(WS-TX) TO PRLM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PRLM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PRLM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FED TAX YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DEDUCT AMT.
      *----------------------------------------------------------------*
       4340-PRORATE-DEDUCT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PRLT-DEDUCT-AMT ROUNDED =
                   PRLM-FED-TAX-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PRLT-DEDUCT-AMT
           END-IF
           ADD PRLT-DEDUCT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE STATE TAX AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO GROSS AMT.
      *----------------------------------------------------------------*
       4350-PRORATE-GROSS.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PRLM-GROSS-AMT ROUNDED =
                   PRLT-STATE-TAX-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PRLM-GROSS-AMT
           END-IF
           ADD PRLM-GROSS-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DEDUCT YTD AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4360-ACCRUE-DEDUCT.
           IF PRLM-DEDUCT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PRLM-DEDUCT-YTD * PRLM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 180 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4370-CHECK-NEXT-REVIEW.
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
           IF WS-DAYS > 180
               MOVE 'Y' TO PRLM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE NET YTD FEE ON THE STATE TAX AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4380-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN PRLM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO PRLM-NET-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PRLM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE PRLM-NET-YTD ROUNDED = PRLT-STATE-TAX-AMT *
                       PRLM-FEE-PCT / 200
               WHEN PRLM-TAX-STATE-CD = 'CAZ'
                   MOVE 12.00 TO PRLM-NET-YTD
               WHEN PRLM-TAX-STATE-CD = 'LZU'
                   MOVE 5.00 TO PRLM-NET-YTD
               WHEN OTHER
                   COMPUTE PRLM-NET-YTD ROUNDED = PRLT-STATE-TAX-AMT *
                       PRLM-FEE-PCT / 100
           END-EVALUATE
           IF PRLM-NET-YTD > 0 AND PRLM-NET-YTD < 10.00
               MOVE 10.00 TO PRLM-NET-YTD
           END-IF
           IF PRLM-NET-YTD > 50.00
               MOVE 50.00 TO PRLM-NET-YTD
           END-IF
           ADD PRLM-NET-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * GROSS YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4390-FLAG-GROSS.
           IF PRLM-GROSS-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PRLM-GROSS-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON NET AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4400-ACCRUE-NET.
           IF PRLM-NET-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PRLM-NET-AMT * PRLM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4410-CHECK-CLOSE.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO PRLM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4420-ACCRUE-RATE.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * PRLM-RATE / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DEDUCT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO NET YTD.
      *----------------------------------------------------------------*
       4430-PRORATE-NET.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PRLM-NET-YTD ROUNDED =
                   PRLM-DEDUCT-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PRLM-NET-YTD
           END-IF
           ADD PRLM-NET-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE FED TAX YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DEDUCT AMT.
      *----------------------------------------------------------------*
       4440-PRORATE-DEDUCT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PRLT-DEDUCT-AMT ROUNDED =
                   PRLM-FED-TAX-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PRLT-DEDUCT-AMT
           END-IF
           ADD PRLT-DEDUCT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FED TAX AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4450-ACCRUE-FED-TAX.
           IF PRLM-FED-TAX-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PRLM-FED-TAX-AMT * PRLM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO PRLM-STATE-TAX-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATE TAX YTD IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4460-LIMIT-STATE-TAX.
           IF PRLM-STATE-TAX-YTD < 50.00
               MOVE 50.00 TO PRLM-STATE-TAX-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PRLM-STATE-TAX-YTD > 500.00
               MOVE 500.00 TO PRLM-STATE-TAX-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * GROSS YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4470-FLAG-GROSS.
           IF PRLM-GROSS-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PRLM-GROSS-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE NET YTD FEE ON THE GROSS AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4480-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN PRLM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO PRLM-NET-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PRLM-HOLD-FLAG = 'Y'
                   COMPUTE PRLM-NET-YTD ROUNDED = PRLM-GROSS-AMT *
                       PRLM-MARGIN / 200
               WHEN PRLM-TAX-STATE-CD = 'CAZ'
                   MOVE 3.00 TO PRLM-NET-YTD
               WHEN PRLM-TAX-STATE-CD = 'LZU'
                   MOVE 3.00 TO PRLM-NET-YTD
               WHEN OTHER
                   COMPUTE PRLM-NET-YTD ROUNDED = PRLM-GROSS-AMT *
                       PRLM-MARGIN / 100
           END-EVALUATE
           IF PRLM-NET-YTD > 0 AND PRLM-NET-YTD < 5.00
               MOVE 5.00 TO PRLM-NET-YTD
           END-IF
           IF PRLM-NET-YTD > 35.00
               MOVE 35.00 TO PRLM-NET-YTD
           END-IF
           ADD PRLM-NET-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4490-DESCRIBE-STATUS.
           EVALUATE PRLM-STATUS-CD
               WHEN 'MV'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'RD'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ST'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'TJ'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'UC'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 60 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4500-CHECK-LAST-ACT.
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
               MOVE 'Y' TO PRLM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FED TAX AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4510-SPLIT-FED-TAX.
           COMPUTE WS-WORK-AMT = PRLM-FED-TAX-AMT / 3
           COMPUTE WS-WORK-AMT-2 = PRLM-FED-TAX-AMT - (WS-WORK-AMT * 3)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NET AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4520-FLAG-NET.
           IF PRLM-NET-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PRLM-NET-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE NET YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FED TAX YTD.
      *----------------------------------------------------------------*
       4530-PRORATE-FED-TAX.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PRLM-FED-TAX-YTD ROUNDED =
                   PRLM-NET-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PRLM-FED-TAX-YTD
           END-IF
           ADD PRLM-FED-TAX-YTD TO WS-TOTAL-PRORATED.

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
                   MOVE PYTIER-STATE-TAX-AMT
                     TO PRLM-STATE-TAX-AMT
                   MOVE PYTIER-GROSS-AMT
                     TO PRLM-GROSS-AMT
                   MOVE PYTIER-FED-TAX-AMT
                     TO PRLM-FED-TAX-AMT
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
                   MOVE PYPEN-FED-TAX-YTD
                     TO PRLM-FED-TAX-YTD
                   MOVE PYPEN-DEDUCT-YTD
                     TO PRLM-DEDUCT-YTD
                   MOVE PYPEN-PREFERRED-FLAG
                     TO PRLM-PREFERRED-FLAG
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
           DISPLAY 'PYLPOST READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'PYLPOST' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
